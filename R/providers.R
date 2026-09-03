#' Provider prefix of a model specification
#'
#' `qlm_code()` and `qlm_segment()` take `model` in ellmer's
#' `"provider/model"` form, or `"provider"` alone. This returns the provider
#' part, which is what dispatch and several defaults key on.
#'
#' @param model A model specification, e.g. `"openai/gpt-4o-mini"`.
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
model_provider <- function(model) {
  sub("/.*$", "", model)
}


#' Providers reachable by name through ellmer
#'
#' [ellmer::chat()] dispatches on the model string's prefix: it looks up
#' `chat_<provider>()` in ellmer's namespace and additionally requires that
#' function to take `model`, `system_prompt` and `params`, calling anything
#' else unsupported. Both gates are mirrored here so that this list is exactly
#' the set of prefixes that can work.
#'
#' Derived from the installed ellmer at run time rather than hard-coded, so a
#' provider ellmer adds or drops later needs no change here.
#'
#' @return A sorted character vector of provider names, without the `chat_`
#'   prefix.
#' @keywords internal
#' @noRd
ellmer_providers <- function() {
  ns <- asNamespace("ellmer")
  fns <- grep("^chat_", getNamespaceExports("ellmer"), value = TRUE)

  # The same three formals ellmer::chat() insists on before dispatching.
  dispatchable <- vapply(
    fns,
    function(fn) {
      all(c("model", "system_prompt", "params") %in% names(formals(get(fn, envir = ns))))
    },
    logical(1)
  )

  sort(sub("^chat_", "", fns[dispatchable]))
}


#' Fail early when a model's provider cannot be reached by name
#'
#' Without this, `model = "qwen/qwen3-max"` reaches [ellmer::chat()] and aborts
#' with `Can't find provider ellmer::chat_qwen()`, which leaks an ellmer
#' internal and says nothing about what to do instead. Every provider ellmer
#' has no `chat_*()` for is reachable as `openai_compatible/<model>` with a
#' `base_url`, so the error names both the prefixes that work and that route.
#'
#' Checked before any request rather than by interpreting a failure, because
#' the answer is known without asking the provider anything.
#'
#' @param model A model specification, as passed to [qlm_code()].
#' @param call The calling environment, for the error message.
#'
#' @return `model`, invisibly.
#' @keywords internal
#' @noRd
check_model_provider <- function(model, call = rlang::caller_env()) {
  # Anything that is not a single string is not this function's business:
  # callers validate that themselves, or ellmer's own `check_string()` does.
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return(invisible(model))
  }

  provider <- model_provider(model)
  providers <- ellmer_providers()

  if (provider %in% providers) {
    return(invisible(model))
  }

  # cli truncates a vector this long by default ("openrouter", ..., "vllm"),
  # which would hide several of the very names the message exists to give.
  providers <- cli::cli_vec(providers, style = list("vec-trunc" = Inf))

  cli::cli_abort(
    c(
      "Can't reach provider {.val {provider}} by name.",
      "i" = "{.fn ellmer::chat} dispatches on the prefix of {.arg model}, and ellmer has no {.fn ellmer::chat_{provider}}.",
      "i" = "Reachable by name: {.val {providers}}.",
      "i" = "Any other OpenAI-compatible endpoint is reachable as {.code openai_compatible/<model>} with {.arg base_url} and {.arg credentials}.",
      "i" = "See the {.strong Provider-specific parameters} section of {.code ?qlm_code} for a worked example."
    ),
    call = call
  )
}


#' Explain a rejected run when the model name is the likely cause
#'
#' A model name that does not exist for its provider is the easiest mistake to
#' make and the worst reported: the first sign is an HTTP 4xx whose message
#' varies from a list of valid names (DeepSeek) to a bare "HTTP 400 Bad
#' Request." (#133). Called only once a run has already been rejected in its
#' entirety, so the happy path costs nothing; it then asks the provider for
#' its model list, through ellmer's `models_<provider>()`, and says whether
#' the name is on it.
#'
#' A diagnostic, never a gate. Whenever the lookup cannot run -- the provider
#' publishes no list, ellmer has no `models_*()` for it, no credentials, no
#' network -- nothing is added and the provider's own error stands unchanged.
#' Nothing is added either when the name is on the list, since the cause is
#' then something else.
#'
#' @param model The model specification, as passed to [qlm_code()].
#' @param chat_args The run's arguments to [ellmer::chat()]; `base_url`,
#'   `api_key` and `credentials` are passed on to the lookup where it takes
#'   them, so a custom endpoint is asked rather than the provider's default.
#'
#' @return A character vector of cli bullets, empty when there is nothing to
#'   say.
#' @keywords internal
#' @noRd
model_name_hint <- function(model, chat_args = list()) {
  # "provider" alone means the provider's default model, which exists
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !grepl("/", model, fixed = TRUE)) {
    return(character())
  }
  provider <- model_provider(model)
  id <- sub("^[^/]*/", "", model)
  models <- provider_models(provider, chat_args)
  if (is.null(models) || id %in% models) {
    return(character())
  }

  lines <- cli::format_inline(
    "{.val {id}} is not a model that {.val {provider}} lists; ",
    "{.fn ellmer::models_{provider}} gives the {length(models)} it does."
  )
  close <- closest_model_names(id, models)
  if (length(close)) {
    lines <- c(lines, cli::format_inline("Did you mean {.val {close}}?"))
  }
  set_bullets(lines, n = Inf, bullet = "i")
}


#' The model names a provider publishes, or `NULL`
#'
#' Asks ellmer's `models_<provider>()`, a credentialed network call, so the
#' answer is memoised for the session in a package-local environment, keyed
#' by provider and `base_url`. Negative results are cached too: a run that
#' fails on every unit must not re-probe the provider each time it reports.
#' Populated on first use, never at load, so `library(quallmer)` touches no
#' network and no credential.
#'
#' @param provider The provider prefix.
#' @param chat_args The run's arguments to [ellmer::chat()].
#'
#' @return A character vector of model ids, or `NULL` when the provider has no
#'   `models_*()` in ellmer or the lookup failed.
#' @keywords internal
#' @noRd
provider_models <- function(provider, chat_args = list()) {
  key <- paste(provider, chat_args$base_url %||% "", sep = "\n")
  if (exists(key, envir = model_list_cache, inherits = FALSE)) {
    return(get(key, envir = model_list_cache, inherits = FALSE))
  }
  lister <- models_lister(provider)
  models <- NULL
  if (!is.null(lister)) {
    args <- chat_args[intersect(names(chat_args), names(formals(lister)))]
    models <- tryCatch(
      {
        listing <- suppressMessages(suppressWarnings(do.call(lister, args)))
        ids <- listing$id
        if (is.character(ids) && length(ids)) ids else NULL
      },
      error = function(e) NULL
    )
  }
  assign(key, models, envir = model_list_cache)
  models
}

model_list_cache <- new.env(parent = emptyenv())


#' ellmer's model-listing function for a provider, or `NULL`
#'
#' Looked up in ellmer's exports at run time, as `ellmer::chat()` looks up
#' `chat_<provider>()`, so a listing ellmer adds later is used without a
#' change here. Fifteen of ellmer's providers have one; the rest are
#' bring-your-own-endpoint providers for which a list would be meaningless.
#'
#' @param provider The provider prefix.
#'
#' @return A function, or `NULL`.
#' @keywords internal
#' @noRd
models_lister <- function(provider) {
  fn <- paste0("models_", provider)
  if (!fn %in% getNamespaceExports("ellmer")) {
    return(NULL)
  }
  get(fn, envir = asNamespace("ellmer"))
}


#' The published model names nearest a mistyped one
#'
#' Edit distance, case-insensitive, keeping only names within half the length
#' of the one typed (and never fewer than two edits), so that a name unlike
#' anything on the list gets no suggestion rather than a misleading one.
#'
#' @param id The model name as typed.
#' @param models The provider's model names.
#' @param n At most this many suggestions.
#'
#' @return A character vector, possibly empty, nearest first.
#' @keywords internal
#' @noRd
closest_model_names <- function(id, models, n = 3L) {
  d <- utils::adist(tolower(id), tolower(models))[1, ]
  near <- d <= max(2, ceiling(nchar(id) / 2))
  candidates <- models[near][order(d[near])]
  utils::head(unique(candidates), n)
}
