#' Structured assistant turns from ellmer, before conversion
#'
#' [ellmer::parallel_chat_structured()] and [ellmer::batch_chat_structured()]
#' both end in ellmer's `multi_convert()`, which converts each turn's JSON to
#' a row and attaches the request errors, token counts and cost only when the
#' result is a data frame. Asking it for the unconverted values
#' (`convert = FALSE`) returns a bare list: `NULL` both for a request that
#' failed and for a response that yielded no JSON, with no usage and no finish
#' reason. Validating a response against the codebook before conversion
#' (#140) needs the turns themselves: the parsed JSON as the provider sent it,
#' the finish reason, the token counts, and each request's error at its
#' original position.
#'
#' This is the one place quallmer reaches past ellmer's exported interface
#' for that. It repeats the few orchestration lines of the two ellmer
#' functions up to the point where they convert, and reuses ellmer's own
#' request construction, throttling, batch submission and result parsing
#' through the internals that `ellmer_structured_internals()` looks up.
#' Nothing here builds a request or reads a response body.
#'
#' @param chat An ellmer `Chat`, carrying the system prompt, model
#'   parameters and tools.
#' @param prompts A list of prompts as `as_input_content()` returns them: each
#'   a string, a single ellmer `Content` object, or a list of `Content`
#'   objects.
#' @param type The codebook schema, unwrapped.
#' @param batch Whether to submit through the provider's batch API.
#' @param execution_args The arguments routed to ellmer's structured call:
#'   `max_active`, `rpm` and `on_error` for a parallel run; `path`, `wait`
#'   and `ignore_hash` for a batch. Defaults are those of the ellmer function
#'   the run replaces. `include_tokens`, `include_cost` and `convert` are
#'   accepted and ignored: the turns carry the usage, and nothing is
#'   converted here.
#'
#' @return A list with one element per prompt, in order: an
#'   [ellmer::AssistantTurn]; an error condition, for a request the provider
#'   refused; or `NULL`, for a request never performed because an earlier
#'   failure stopped the run. `NULL` as a whole for a batch job that has not
#'   completed under `wait = FALSE`, as ellmer returns.
#' @keywords internal
#' @noRd
structured_chat_turns <- function(chat, prompts, type, batch = FALSE,
                                  execution_args = list()) {
  internals <- ellmer_structured_internals()
  provider <- chat$get_provider()
  model <- chat$get_model_object()

  # An OpenAI-compatible endpoint takes only an object at the root, so ellmer
  # wraps any other type in one and unwraps the answer. The wrapped type is
  # what goes on the wire; the original is what the answer is validated
  # against, so the caller asks structured_needs_wrapper() again when reading
  needs_wrapper <- internals$type_needs_wrapper(type, provider)
  wire_type <- internals$wrap_type_if_needed(type, needs_wrapper)

  if (batch) {
    job <- internals$BatchJob$new(
      chat = chat,
      prompts = prompts,
      type = wire_type,
      path = execution_args$path,
      wait = execution_args$wait %||% TRUE,
      ignore_hash = execution_args$ignore_hash %||% FALSE
    )
    # Not done, and told not to wait: ellmer's contract is a NULL result,
    # with the job's state on disk for the next call to resume
    if (is.null(job$step_until_done())) {
      return(NULL)
    }
    # Retrieval logs the turns itself, so none of that here
    return(job$result_turns())
  }

  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- lapply(prompts, function(prompt) {
    c(existing, list(structured_user_turn(prompt)))
  })

  turns <- internals$parallel_turns(
    provider = provider,
    model = model,
    conversations = conversations,
    tools = chat$get_tools(),
    type = wire_type,
    max_active = execution_args$max_active %||% 10,
    rpm = execution_args$rpm %||% 500,
    on_error = execution_args$on_error %||% "return"
  )
  if (is.function(internals$log_turns)) {
    internals$log_turns(provider, model, turns)
  }
  turns
}


#' Does ellmer wrap this type in an object for this provider?
#'
#' @inheritParams structured_chat_turns
#' @param provider The chat's provider object.
#'
#' @return `TRUE` when the answer arrives as `{"wrapper": <value>}`.
#' @keywords internal
#' @noRd
structured_needs_wrapper <- function(type, provider) {
  ellmer_structured_internals()$type_needs_wrapper(type, provider)
}


#' A user turn for one prompt, as ellmer builds it
#'
#' Mirrors ellmer's `as_user_turn()` for the three shapes
#' `as_input_content()` produces, using exported constructors only.
#'
#' @param prompt A string, a `Content` object, or a list of `Content` objects.
#'
#' @return An [ellmer::UserTurn].
#' @keywords internal
#' @noRd
structured_user_turn <- function(prompt) {
  if (inherits(prompt, "ellmer::Turn")) {
    return(prompt)
  }
  contents <- if (is.character(prompt) && length(prompt) == 1L) {
    list(ellmer::ContentText(prompt))
  } else if (inherits(prompt, "ellmer::Content")) {
    list(prompt)
  } else if (is.list(prompt) &&
             all(vapply(prompt, inherits, logical(1), "ellmer::Content"))) {
    prompt
  } else {
    cli::cli_abort(
      "A prompt must be a string, a Content object, or a list of Content objects.",
      .internal = TRUE
    )
  }
  ellmer::UserTurn(contents)
}


#' The ellmer internals the structured path depends on
#'
#' Looked up in one place, and checked against the signatures quallmer was
#' written to, so that a change in ellmer stops the run here with a message
#' that says what changed, before anything is uploaded or billed, rather than
#' failing later or handing back unvalidated results.
#'
#' The required set is `parallel_turns()`, `BatchJob`, `type_needs_wrapper()`,
#' `wrap_type_if_needed()` and `convert_from_type()`. `log_turns()` is
#' optional: without it the parallel turns are not logged, which loses
#' nothing a coding run depends on.
#'
#' @param call The environment to report a compatibility error from.
#'
#' @return A named list of the functions, plus the `BatchJob` generator.
#' @keywords internal
#' @noRd
ellmer_structured_internals <- function(call = rlang::caller_env()) {
  formals_wanted <- list(
    parallel_turns = c("provider", "model", "conversations", "tools", "type",
                       "max_active", "rpm", "on_error"),
    type_needs_wrapper = c("type", "provider"),
    wrap_type_if_needed = c("type", "needs_wrapper"),
    convert_from_type = c("x", "type")
  )
  found <- lapply(names(formals_wanted), ellmer_symbol)
  names(found) <- names(formals_wanted)

  missing <- vapply(names(found), function(name) {
    f <- found[[name]]
    !is.function(f) || !all(formals_wanted[[name]] %in% names(formals(f)))
  }, logical(1))

  batch_job <- ellmer_symbol("BatchJob")
  batch_ok <- !is.null(batch_job) && is.function(batch_job$new)
  if (batch_ok && inherits(batch_job, "R6ClassGenerator")) {
    methods <- batch_job$public_methods
    batch_ok <- all(c("step_until_done", "result_turns") %in% names(methods)) &&
      all(c("chat", "prompts", "path", "type", "wait", "ignore_hash") %in%
            names(formals(methods$initialize)))
  }

  changed <- c(names(missing)[missing], if (!batch_ok) "BatchJob")
  if (length(changed)) {
    version <- tryCatch(
      as.character(utils::packageVersion("ellmer")),
      error = function(e) "unknown"
    )
    cli::cli_abort(c(
      "This version of {.pkg ellmer} ({version}) does not provide what {.fn qlm_code} needs to validate structured output.",
      "x" = "Missing or changed: {.code {changed}}.",
      "i" = "quallmer was written against ellmer 0.5.0.",
      "i" = "Install that version, or report this at {.url https://github.com/quallmer/quallmer/issues}."
    ), call = call)
  }

  log_turns <- ellmer_symbol("log_turns")
  if (!is.function(log_turns) ||
      !all(c("provider", "model", "turns") %in% names(formals(log_turns)))) {
    log_turns <- NULL
  }

  c(found, list(BatchJob = batch_job, log_turns = log_turns))
}


#' Look up one unexported ellmer object
#'
#' @param name The object's name in ellmer's namespace.
#'
#' @return The object, or `NULL` when ellmer has no such name.
#' @keywords internal
#' @noRd
ellmer_symbol <- function(name) {
  tryCatch(
    utils::getFromNamespace(name, "ellmer"),
    error = function(e) NULL
  )
}
