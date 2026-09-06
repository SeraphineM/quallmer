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
#' relied on. Pass `structured` explicitly to override. When the endpoint
#' changes, provider or `base_url`, the path is chosen afresh for it. By the
#' same rule,
#' a parent that was completed with [qlm_backfill()] has its passes replayed
#' on the replication, so the two are complete on the same terms; see
#' `backfill`.
#'
#' @param x A `qlm_coded` object.
#' @param ... Optional overrides passed to [qlm_code()], such as `params`,
#'   `api_args`, or `max_active`. Any setting not overridden is restored from
#'   the original run when the endpoint is unchanged, including the arguments
#'   it passed to [ellmer::chat()]. An endpoint is identified by both the
#'   provider prefix and `base_url` (an explicit `base_url = NULL`, meaning
#'   the provider's default host, counts as a change of endpoint), since
#'   every provider ellmer has no `chat_*()` for is reached as
#'   `openai_compatible/<model>` — so Qwen through
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
#' @param backfill Logical, integer, or `NULL`; controls backfilling after the
#'   replication. `NULL` (default) replays the passes recorded on `x`, using
#'   the same models and overrides in the same order. `FALSE` or `0` performs
#'   no backfill. `TRUE` runs a fresh backfill with the replication's model
#'   and the default number of passes, currently two; a positive integer runs
#'   at most that many fresh passes. A fresh backfill does not reproduce a
#'   different model used by the parent's recorded passes. If a replayed pass
#'   fails outright, the replication and any earlier recoveries are retained,
#'   the failure is recorded, and no later passes are replayed.
#' @param name Optional name for this run. If `NULL`, defaults to the model
#'   name (if changed) or `"replication_N"` where N is the replication count.
#' @param notes Optional character string with descriptive notes about this
#'   replication. Useful for documenting why this replication was run or what
#'   differs from the original. Default is `NULL`.
#'
#' @return A `qlm_coded` object with `run$parent` set to the parent's run name.
#'
#' @seealso [qlm_code()] for initial coding, [qlm_compare()] for comparing
#'   replicated results, [qlm_backfill()] to re-code only the units a run
#'   failed on.
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
qlm_replicate <- function(x, ..., codebook = NULL, model = NULL, batch = NULL,
                          backfill = NULL, name = NULL, notes = NULL) {
  # Input validation, including that .id is a key and the run metadata is
  # present; also upgrades an old metadata layout
  x <- check_qlm_coded(x)
  # Checked before the replication is coded, not after: a bad value must not
  # cost a paid call. The value itself is read again by replay_backfill().
  backfill_passes(backfill, null = NULL)

  # Extract original components
  original_data <- attr(x, "data")
  meta_attr <- attr(x, "meta")
  original_codebook <- attr(x, "codebook")
  original_model <- meta_attr$object$chat_args$name
  # Extract batch flag (default to FALSE for backward compatibility)
  original_batch <- meta_attr$object$batch %||% FALSE
  parent_name <- meta_attr$user$name

  # Apply batch override if provided, otherwise use original
  use_batch <- batch %||% original_batch

  # Capture the current call
  current_call <- match.call()

  # Apply overrides (NULL means use original)
  use_codebook <- codebook %||% original_codebook

  # Everything the original run passed to ellmer, merged with the overrides,
  # on the path the original run took. Shared with qlm_backfill().
  restored <- restore_run_args(x, overrides = list(...), model = model,
                               batch = use_batch)
  use_model <- restored$model
  call_args <- restored$call_args

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

  # A file input is uploaded again from the recorded paths: first check they
  # still hold the bytes the parent coded, and that a model the parent
  # accepted by registration is registered in this session too
  if (use_codebook$input_type %in% file_input_types()) {
    verify_input_files(x)
    check_registered_input_model(x, use_model)
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
  result_meta$object$provider_resolution <- restored$resolution
  # The text is the parent's, so its transcription record is too; read from
  # the parent's metadata when the stored input no longer carries it (#178)
  if (is.null(result_meta$user$transcription) &&
      !is.null(meta_attr$user$transcription)) {
    result_meta$user$transcription <- meta_attr$user$transcription
  }
  attr(result, "meta") <- result_meta

  replay_backfill(result, parent = x, backfill = backfill)
}


#' Restore the arguments a run was coded with
#'
#' Everything the original run passed to [ellmer::chat()] -- `params`,
#' `api_args`, `base_url`, `credentials` -- and to the execution function,
#' merged with any overrides, plus the `structured` mode and `json_retries`
#' derived from the path the run actually took. Used by [qlm_replicate()] and
#' [qlm_backfill()], so that both re-run a coding with the settings it was
#' made with.
#'
#' Rates supplied to cost the original run (`prices`) travel only within
#' their pricing context, which is why the batch setting the caller will use
#' is an argument here: a backfill always runs as a parallel call, so a
#' batch-coded run's rates do not apply to it.
#'
#' @param x A `qlm_coded` object, already upgraded.
#' @param overrides Named list of overrides, as from `...`.
#' @param model Replacement model, or `NULL` to keep the original.
#' @param batch The batch setting the caller will run with.
#'
#' @return A list with `model` (the model to use) and `call_args` (arguments
#'   for [qlm_code()] beyond `x`, `codebook`, `model`, `batch`, `name` and
#'   `notes`).
#' @keywords internal
#' @noRd
restore_run_args <- function(x, overrides = list(), model = NULL, batch = FALSE) {
  meta_attr <- attr(x, "meta")
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
  # An object read back from a trail carries "<redacted>" where a credential
  # was; that is not a value to send. An override in `...` supersedes it
  # silently, as it would any recorded value.
  stripped <- drop_redacted_args(original_chat_args)
  original_chat_args <- stripped$args
  dropped <- setdiff(stripped$dropped, names(overrides))
  if (length(dropped)) {
    cli::cli_inform(c("i" = redacted_args_note(dropped)))
  }
  # Resolve a replacement before comparing endpoints. Its registered URL and
  # credential source must not inherit those of the parent run.
  requested_model <- model %||% original_model
  prefix <- model_provider(requested_model)
  resolved <- if (rlang::is_string(model) && !prefix %in% ellmer_providers() &&
                  !is.null(provider_definition(prefix))) {
    resolve_provider(model, overrides)
  } else {
    list(model = requested_model, args = overrides, resolution = NULL)
  }
  use_model <- resolved$model
  overrides <- resolved$args

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
  # A `base_url` supplied in `...` counts as a change, whether it names
  # another host or is `NULL` to fall back to the provider's default: either
  # way the caller is pointing this run away from the host whose credential
  # was recorded, and that credential must not travel with it. `%||%` cannot
  # tell a named `NULL` from an absent argument, so presence is tested by
  # name; `modifyList()` below then drops the recorded URL as intended.
  original_endpoint <- list(
    provider = sub("/.*$", "", original_model),
    base_url = original_chat_args$base_url %||% NA_character_
  )
  use_base_url <- if ("base_url" %in% names(overrides)) {
    overrides$base_url %||% NA_character_
  } else {
    original_chat_args$base_url %||% NA_character_
  }
  use_endpoint <- list(
    provider = sub("/.*$", "", use_model),
    base_url = use_base_url
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
        paste0("endpoint from ", describe_endpoint(original_endpoint$base_url),
               " to ", describe_endpoint(use_endpoint$base_url))
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

  # `on_error`, `max_active` and `rpm` belong to the parallel call; `path`,
  # `wait` and `ignore_hash` to the batch API. Neither function accepts the
  # other's, so a setting recorded on one path must not travel to a run on the
  # other. Only the inherited value is dropped, before the overrides are
  # merged: an override in `...` is the caller's own, and qlm_code() says why
  # it cannot apply rather than having it vanish here.
  pcs_names <- names(formals(ellmer::parallel_chat_structured))
  batch_names <- names(formals(ellmer::batch_chat_structured))
  incompatible <- if (batch) {
    setdiff(pcs_names, batch_names)
  } else {
    setdiff(batch_names, pcs_names)
  }
  original_execution_args[
    intersect(incompatible, names(original_execution_args))
  ] <- NULL

  # Merge overrides over everything the original run used. chat_args and
  # execution_args are disjoint by construction -- qlm_code() splits `...`
  # between them by name -- so they can be merged here and re-split there,
  # keeping one source of truth for the routing rules.
  original_args <- c(original_chat_args, original_execution_args)
  call_args <- modifyList(original_args, overrides)

  # `structured` and `json_retries` are formals of qlm_code(), so they are
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
  #
  # The path does not carry across endpoints: the one an endpoint took says
  # nothing about what another accepts (DeepSeek rejects the schema-
  # constrained request outright; two OpenAI-compatible services behind the
  # same prefix enforce a schema quite differently), so with a new endpoint
  # the mode is left for qlm_code() to choose as it would for a fresh run.
  # The endpoint identity is the one computed above, prefix plus base_url.
  # json_retries still travels, since JSON mode is reachable anywhere and the
  # setting is inert on the structured path.
  endpoint_changed <- !identical(original_endpoint, use_endpoint)
  original_backend <- if (endpoint_changed) NULL else meta_attr$object$backend
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
  # json_retries has no effect on the purely structured path, where supplying it
  # is an error, so carry it only where it can apply.
  if (!"json_retries" %in% names(call_args) &&
      !identical(call_args$structured, "structured")) {
    # Objects from development versions before the rename recorded max_retries
    original_retries <- meta_attr$user$json_retries %||% meta_attr$user$max_retries
    if (!is.null(original_retries)) {
      call_args$json_retries <- original_retries
    }
  }

  # Rates supplied to cost the original run belong to a pricing context:
  # the model, the endpoint it was reached through (provider and base_url,
  # the identity used above), whether it ran as a batch, and the service
  # tier, each of which providers price differently. A run that keeps all
  # four is costed the same way again; one that changes any of them is not
  # costed on the old rates, and says so. A tier left unset is ellmer's
  # default, "auto", and a change to any explicit tier counts. An explicit
  # `prices` in `...` is left alone (#135).
  original_prices <- meta_attr$user$prices
  if (!"prices" %in% names(call_args) && !is.null(original_prices)) {
    original_batch <- meta_attr$object$batch %||% FALSE
    original_tier <- meta_attr$object$chat_args$service_tier %||% "auto"
    use_tier <- call_args$service_tier %||% "auto"
    changed <- c(
      if (!identical(use_model, original_model)) "model",
      if (!identical(use_endpoint, original_endpoint)) "endpoint",
      if (!identical(batch, original_batch)) "batch setting",
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
          "to cost it."
        )
      ))
    }
  }

  resolution <- resolved$resolution
  if (is.null(model)) {
    resolution <- meta_attr$object$provider_resolution
    if (!is.null(resolution) && endpoint_changed) {
      resolution$endpoint_overridden <- TRUE
    }
    if (endpoint_changed || any(c("credentials", "api_key") %in% names(overrides))) {
      resolution$api_key_env <- NULL
    }
  }
  list(model = use_model, call_args = call_args, resolution = resolution,
       overrides = overrides, endpoint_changed = endpoint_changed)
}


#' Name an endpoint for a message
#'
#' @param base_url The endpoint's `base_url`, or `NA` for the provider's
#'   default host.
#'
#' @return A single string.
#' @keywords internal
#' @noRd
describe_endpoint <- function(base_url) {
  if (is.na(base_url)) {
    "the provider's default endpoint"
  } else {
    paste0("\"", base_url, "\"")
  }
}
