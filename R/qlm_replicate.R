#' Replicate a coding task
#'
#' Re-executes a coding task from a `qlm_coded` object, optionally with
#' modified settings. If no overrides are provided, uses identical settings
#' to the original coding: both the execution arguments and the arguments the
#' original run passed to [ellmer::chat()], such as `params` and `api_args`.
#' Credentials and endpoint settings are an exception, and are carried over
#' only while the endpoint itself is unchanged.
#'
#' The coding path is reproduced from the path the original run actually took,
#' not the `structured` mode it requested: a run that asked for `"auto"` and
#' fell back to JSON mode replicates as `"json"`, so that an intermittently
#' conforming endpoint cannot quietly skip the local validation the original
#' relied on. Pass `structured` explicitly to override.
#'
#' @param x A `qlm_coded` object.
#' @param ... Optional overrides passed to [qlm_code()], such as `params`,
#'   `api_args`, or `max_active`. Any setting not overridden is restored from
#'   the original run when the endpoint is unchanged, including the arguments
#'   it passed to [ellmer::chat()]. An endpoint is identified by both the
#'   provider prefix and `base_url`, since every provider ellmer has no
#'   `chat_*()` for is reached as `openai_compatible/<model>` — so Qwen through
#'   Alibaba Model Studio and Kimi through Moonshot share a prefix while being
#'   different services with different credentials. When either changes, only
#'   portable chat settings (`params` and `echo`) are carried over; supply
#'   credentials, endpoint settings and other endpoint-specific arguments
#'   explicitly. An informational message names inherited arguments that were
#'   omitted and not explicitly replaced. Registered `tools` are never carried
#'   over.
#' @param codebook Optional replacement codebook. If `NULL` (default), uses
#'   the codebook from `x`.
#' @param model Optional replacement model (e.g., `"openai/gpt-4o"`). If `NULL`
#'   (default), uses the model from `x`.
#' @param batch Optional logical to override batch processing setting. If `NULL`
#'   (default), uses the batch setting from `x`. Set to `TRUE` to use batch
#'   processing or `FALSE` to use parallel processing, regardless of the
#'   original setting.
#' @param name Optional name for this run. If `NULL`, defaults to the model
#'   name (if changed) or `"replication_N"` where N is the replication count.
#' @param notes Optional character string with descriptive notes about this
#'   replication. Useful for documenting why this replication was run or what
#'   differs from the original. Default is `NULL`.
#'
#' @return A `qlm_coded` object with `run$parent` set to the parent's run name.
#'
#' @seealso [qlm_code()] for initial coding, [qlm_compare()] for comparing
#'   replicated results.
#'
#' @examples
#' \dontrun{
#' # First create a coded object
#' texts <- c("I love this!", "Terrible.", "It's okay.")
#' coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini", name = "run1")
#'
#' # Replicate with same model
#' coded2 <- qlm_replicate(coded, name = "run2")
#'
#' # Compare results
#' qlm_compare(coded, coded2, by = "sentiment", level = "nominal")
#' }
#'
#' @importFrom utils modifyList
#' @export
qlm_replicate <- function(x, ..., codebook = NULL, model = NULL, batch = NULL, name = NULL, notes = NULL) {
  # Input validation, including that .id is a key and the run metadata and
  # codebook are present; also upgrades an old metadata layout
  x <- check_qlm_coded(x)

  # Extract original components
  original_data <- attr(x, "data")
  meta_attr <- attr(x, "meta")
  original_codebook <- attr(x, "codebook")
  original_model <- meta_attr$object$chat_args$name
  # Ensure it's always a list (empty if NULL)
  original_execution_args <- meta_attr$object$execution_args %||% list()
  # Everything the original passed to ellmer::chat() -- params, api_args,
  # base_url, credentials -- must be restored for the same provider, or a
  # replication runs with different model settings than the run it claims to
  # replicate. Two exclusions: `name` is the model and is passed separately,
  # and registered `tools` are not safe to recreate automatically.
  original_chat_args <- meta_attr$object$chat_args %||% list()
  original_chat_args[c("name", "tools")] <- NULL
  # Extract batch flag (default to FALSE for backward compatibility)
  original_batch <- meta_attr$object$batch %||% FALSE
  parent_name <- meta_attr$user$name

  # Apply batch override if provided, otherwise use original
  use_batch <- batch %||% original_batch

  # Capture the current call
  current_call <- match.call()

  # Apply overrides (NULL means use original)
  use_codebook <- codebook %||% original_codebook
  use_model <- model %||% original_model
  overrides <- list(...)

  # Credentials and endpoint settings belong to an endpoint, not to a model in
  # the abstract. Carrying them across endpoints can send a credential to the
  # wrong service, or point the replacement model at the original host.
  #
  # The prefix alone is not enough to identify an endpoint. Providers ellmer
  # has no `chat_*()` for are all reached as `openai_compatible/<model>`, so
  # Qwen through Alibaba Model Studio and Kimi through Moonshot share a prefix
  # while being entirely different services with different credentials. What
  # distinguishes them is `base_url`, so that is part of the identity too.
  #
  # A `base_url` supplied in `...` counts as a change: the caller is pointing
  # this run at a different host, so the credential inherited for the old one
  # must not travel with it.
  original_endpoint <- list(
    provider = sub("/.*$", "", original_model),
    base_url = original_chat_args$base_url %||% NA_character_
  )
  use_endpoint <- list(
    provider = sub("/.*$", "", use_model),
    base_url = overrides$base_url %||% original_chat_args$base_url %||% NA_character_
  )

  if (!identical(original_endpoint, use_endpoint)) {
    portable_chat_args <- c("params", "echo")
    endpoint_bound_args <- setdiff(names(original_chat_args), portable_chat_args)
    omitted_args <- setdiff(endpoint_bound_args, names(overrides))
    omitted_args <- unique(omitted_args[nzchar(omitted_args)])
    if (length(omitted_args)) {
      changed <- if (!identical(original_endpoint$provider, use_endpoint$provider)) {
        paste0("provider from \"", original_endpoint$provider, "\" to \"",
               use_endpoint$provider, "\"")
      } else {
        paste0("endpoint from \"", original_endpoint$base_url, "\" to \"",
               use_endpoint$base_url, "\"")
      }
      omitted_text <- paste0("`", omitted_args, "`", collapse = ", ")
      cli::cli_inform(c(
        "i" = paste0(
          "Changing ", changed, "; not carrying over endpoint-specific argument",
          if (length(omitted_args) == 1L) "" else "s", ": ", omitted_text,
          ". Supply ", if (length(omitted_args) == 1L) "it" else "them",
          " explicitly in `...` if needed."
        )
      ))
    }
    original_chat_args <- original_chat_args[
      names(original_chat_args) %in% portable_chat_args
    ]
  }

  # Determine run name
  if (is.null(name)) {
    if (!is.null(model) && model != original_model) {
      # Use new model name as run name
      name <- sub(".*/", "", model)  # extract model part after provider/
    } else {
      # Generate replication name
      name <- paste0("replication_",
                     sum(grepl("^replication_", c(parent_name))) + 1)
    }
  }

  # Merge overrides over everything the original run used. chat_args and
  # execution_args are disjoint by construction -- qlm_code() splits `...`
  # between them by name -- so they can be merged here and re-split there,
  # keeping one source of truth for the routing rules.
  original_args <- c(original_chat_args, original_execution_args)
  call_args <- modifyList(original_args, overrides)

  # `structured` and `max_retries` are formals of qlm_code(), so they are
  # recorded in the run metadata rather than in chat_args. An explicit override
  # in `...` is left alone.
  #
  # Derived from `backend`, the path the run actually took, NOT from
  # `structured`, the mode it asked for. A run that requested "auto" and fell
  # back to JSON mode must replicate in JSON mode: requesting "auto" again
  # would let an intermittently-conforming endpoint take the structured path
  # this time, skipping the local validation the original relied on, so the two
  # runs would not be comparable -- which is the whole point of replicating.
  #
  # Deriving from `backend` also covers objects coded before `structured`
  # existed, which record a backend but no mode.
  original_backend <- meta_attr$object$backend
  original_mode <- if (identical(original_backend, "json_mode")) {
    "json"
  } else if (identical(original_backend, "structured")) {
    "structured"
  } else {
    NULL
  }
  if (!"structured" %in% names(call_args) && !is.null(original_mode)) {
    call_args$structured <- original_mode
  }
  # max_retries has no effect on the purely structured path, where supplying it
  # is an error, so carry it only where it can apply.
  if (!"max_retries" %in% names(call_args) &&
      !identical(call_args$structured, "structured")) {
    original_retries <- meta_attr$user$max_retries
    if (!is.null(original_retries)) {
      call_args$max_retries <- original_retries
    }
  }

  # Rates supplied to cost the original run belong to a pricing context:
  # the model, the endpoint it was reached through (provider and base_url,
  # the identity used above), whether it ran as a batch, and the service
  # tier, each of which providers price differently. A replication that keeps
  # all four is costed the same way again; one that changes any of them is
  # not costed on the old rates, and says so. A tier left unset is ellmer's
  # default, "auto", and a change to any explicit tier counts. An explicit
  # `prices` in `...` is left alone (#135).
  original_prices <- meta_attr$user$prices
  if (!"prices" %in% names(call_args) && !is.null(original_prices)) {
    original_tier <- meta_attr$object$chat_args$service_tier %||% "auto"
    use_tier <- call_args$service_tier %||% "auto"
    changed <- c(
      if (!identical(use_model, original_model)) "model",
      if (!identical(use_endpoint, original_endpoint)) "endpoint",
      if (!identical(use_batch, original_batch)) "batch setting",
      if (!identical(use_tier, original_tier)) "service tier"
    )
    if (length(changed) == 0L) {
      call_args$prices <- original_prices
    } else {
      cli::cli_inform(c(
        "i" = paste0(
          "Not carrying over `prices`: the ", paste(changed, collapse = ", "),
          if (length(changed) == 1L) " differs" else " differ",
          " from the run that supplied them. Supply this run's rates in `...` ",
          "to cost the replication."
        )
      ))
    }
  }

  # Call qlm_code with merged arguments, including batch flag
  result <- do.call(qlm_code, c(
    list(
      x = original_data,
      codebook = use_codebook,
      model = use_model,
      batch = use_batch,
      name = name,
      notes = notes
    ),
    call_args
  ))

  # Override the metadata to reflect this is a replication
  result_meta <- attr(result, "meta")
  result_meta$object$call <- current_call
  result_meta$object$parent <- parent_name
  attr(result, "meta") <- result_meta

  result
}
