#' Re-code the units a run failed on
#'
#' A coding run over a real corpus rarely comes back complete. Requests time
#' out or are rate-limited, a provider refuses a text on one pass and codes it
#' on the next, an endpoint accepts a schema and ignores it for a few units.
#' The failed units sit in the `qlm_coded` object as `NA` rows, listed by
#' [qlm_failures()]. `qlm_backfill()` re-codes only those units and merges
#' what comes back into the original object. Everything that succeeded the
#' first time is left exactly as it was.
#'
#' By default the passes use the run's own model, codebook and settings, so
#' the result is what the run should have produced. A different `model` can
#' be given, for units the original model consistently refuses or cannot fit
#' in its context window; the object then records which units were coded by
#' which model, and `print()` says so, since a result coded by two instruments
#' has to be disclosed as one.
#'
#' @param x A `qlm_coded` object produced by [qlm_code()] or [qlm_replicate()].
#' @param ... Optional overrides passed to [qlm_code()] for the backfill
#'   passes, such as `params`, `max_active` or `on_error`. Any setting not
#'   overridden is restored from the original run, as [qlm_replicate()] does,
#'   with the same rule for credentials and endpoint settings when the
#'   provider changes. The codebook cannot be changed.
#' @param model Optional model for the passes, in the form used by
#'   [qlm_code()]. `NULL` (default) uses the run's own model.
#' @param attempts Maximum number of passes. Default is 2. A pass that
#'   recovers nothing ends the backfill early, since the failures that remain
#'   are then evidently not transient.
#'
#' @details
#' Which units are re-coded is decided afresh on every pass, from the object as
#' it then stands, by the same test [qlm_failures()] uses: a unit that carries
#' an `.error`, or whose required scalar properties are all `NA`. Two kinds of
#' failure are left alone, because re-sending the same request cannot change
#' the outcome:
#'
#' * a text the provider rejected as longer than the model's context window,
#'   unless `model` changes;
#' * a response cut off at the `max_tokens` limit, unless `model` changes or
#'   the backfill overrides `params`, which is how the limit is raised.
#'
#' Content refusals are deliberately retried. They look deterministic and are
#' not: the same document is refused on one pass and coded on the next, at
#' more than one provider.
#'
#' Each pass is an ordinary [qlm_code()] call over the failed units, on the
#' path the original run took (a run that fell back to JSON mode is backfilled
#' in JSON mode; with a different provider the path is chosen afresh), and
#' always as a parallel call: a run coded through the batch API is backfilled
#' through the parallel API, with the same model and settings, and any
#' batch-only arguments (`path`, `wait`, `ignore_hash`) set aside. A pass that
#' fails outright on the first attempt is an error, since nothing has been
#' gained yet and the cause is most likely configuration; on a later pass it
#' is a warning, and what earlier passes recovered is kept.
#'
#' The merge is by `.id`. Rows keep their order; a unit is replaced only when
#' the retry produced a usable coding, so a retry that failed again never
#' overwrites anything, though its `.error` is recorded as the latest reason.
#' Token and cost columns, when present, are summed across all attempts, since
#' a failed request may still have been billed. The passes are recorded in the
#' object metadata as `backfill`, one entry per pass with its timestamp, the
#' model if it differed from the run's, the overrides, and the `.id`s attempted
#' and recovered, so the result can say which of its rows came from which pass
#' and which model. [qlm_replicate()] replays these passes on a replication, so
#' that a replication of a completed run is completed on the same terms.
#'
#' @return `x`, with the recovered units filled in and `backfill` added to
#'   its object metadata. The run name, parent, codebook and inputs are
#'   unchanged.
#'
#' @seealso [qlm_failures()] for the units a run failed on and why;
#'   [qlm_code()], whose `backfill = TRUE` completes a run in the same call;
#'   [qlm_replicate()] to re-run a whole coding.
#'
#' @examples
#' \dontrun{
#' coded <- qlm_code(texts, codebook, model = "openai/gpt-4o-mini",
#'                   on_error = "continue")
#' qlm_failures(coded)
#'
#' filled <- qlm_backfill(coded)
#' qlm_failures(filled)
#' qlm_meta(filled, "backfill", type = "object")
#'
#' # Responses cut off at the output limit are retried only with a higher one
#' filled <- qlm_backfill(coded, params = ellmer::params(max_tokens = 32000))
#'
#' # Units one model refuses or cannot fit, coded by another; the result
#' # records which units came from which model
#' filled <- qlm_backfill(filled, model = "deepseek/deepseek-chat")
#' }
#'
#' @export
qlm_backfill <- function(x, ..., model = NULL, attempts = 2L) {
  if (!inherits(x, "qlm_coded")) {
    cli::cli_abort("{.arg x} must be a {.cls qlm_coded} object.")
  }
  x <- upgrade_meta(x)
  meta_attr <- attr(x, "meta")

  if (identical(meta_attr$object$source, "human")) {
    cli::cli_abort(c(
      "{.arg x} is human-coded; there is no coding run to re-code.",
      "i" = "{.fn qlm_backfill} re-runs the model of a {.fn qlm_code} run on the units it failed on."
    ))
  }
  if (length(attempts) != 1L || !is.numeric(attempts) || is.na(attempts) ||
      attempts < 1 || attempts != trunc(attempts)) {
    cli::cli_abort("{.arg attempts} must be a single positive integer.")
  }
  attempts <- as.integer(attempts)
  if (!is.null(model) && (!is.character(model) || length(model) != 1L || is.na(model))) {
    cli::cli_abort("{.arg model} must be a single string, or {.code NULL} for the run's own model.")
  }

  overrides <- list(...)
  if ("codebook" %in% names(overrides) || "x" %in% names(overrides)) {
    cli::cli_abort(c(
      "The codebook cannot be changed in a backfill.",
      "i" = "A backfill fills the gaps in this run's coding; a different codebook is a different coding.",
      "i" = "Use {.fn qlm_replicate} to code with another codebook."
    ))
  }
  if ("batch" %in% names(overrides)) {
    cli::cli_abort(c(
      "{.arg batch} cannot be set in a backfill.",
      "i" = "Backfill passes are always parallel calls over the failed units."
    ))
  }

  run_model <- meta_attr$object$chat_args$name
  restored <- restore_run_args(x, overrides = overrides, model = model)
  model_changed <- !identical(restored$model, run_model)
  call_args <- restored$call_args
  # A backfill is a small parallel call whatever the original run was; the
  # batch API's cache arguments would point at the original run's file.
  batch_only <- setdiff(
    names(formals(ellmer::batch_chat_structured)),
    names(formals(ellmer::parallel_chat_structured))
  )
  call_args[intersect(batch_only, names(call_args))] <- NULL

  data <- inputs(x)
  ids <- as.character(x$.id)
  run_name <- meta_attr$user$name
  passes <- list()

  for (attempt in seq_len(attempts)) {
    failed <- failed_units(x)
    if (!any(failed)) {
      if (attempt == 1L) {
        cli::cli_inform(c("i" = "Nothing to backfill: every unit is coded."))
      }
      break
    }

    # Re-derived each pass from the object as it now stands: the previous
    # pass may have recorded a length rejection that was not visible before.
    reasons <- failure_reasons(recorded_errors(x), failed)
    terminal <- failed & is_terminal_failure(reasons, overrides, model_changed)
    retry <- which(failed & !terminal)

    if (attempt == 1L && any(terminal)) {
      cli::cli_inform(c(
        "i" = "Leaving {sum(terminal)} unit{?s} alone: rejected on length, or cut off at {.code max_tokens}. A different {.arg model}, or new {.arg params} for the limit, would retry them."
      ))
    }
    if (!length(retry)) {
      cli::cli_inform(c("i" = "Nothing recoverable remains."))
      break
    }

    cli::cli_inform(c(
      "i" = "Backfill pass {attempt} of {attempts}: re-coding {length(retry)} unit{?s} with {.val {restored$model}}."
    ))

    subset <- data[retry]
    names(subset) <- ids[retry]
    result <- tryCatch(
      do.call(qlm_code, c(
        list(
          x = subset,
          codebook = codebook(x),
          model = restored$model,
          batch = FALSE,
          name = paste0(run_name %||% "run", "_backfill_", attempt)
        ),
        call_args
      )),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      if (attempt == 1L) {
        cli::cli_abort(c(
          "The backfill pass failed before recovering anything.",
          set_bullets(strip_ansi(conditionMessage(result))),
          "i" = "Nothing was changed."
        ))
      }
      cli::cli_warn(c(
        "Backfill pass {attempt} failed; keeping what earlier passes recovered.",
        set_bullets(strip_ansi(conditionMessage(result)))
      ))
      break
    }

    x <- merge_backfill_rows(x, result)
    recovered <- ids[retry][!failed_units(result)]
    passes[[length(passes) + 1L]] <- list(
      timestamp = Sys.time(),
      model = if (model_changed) restored$model else NULL,
      overrides = overrides,
      attempted = ids[retry],
      recovered = recovered
    )

    remaining <- sum(failed_units(x))
    cli::cli_inform(c(
      "i" = "Recovered {length(recovered)} unit{?s}; {remaining} still failed."
    ))
    if (!length(recovered)) {
      cli::cli_inform(c("i" = "No progress; the remaining failures do not look transient. Stopping."))
      break
    }
  }

  if (length(passes)) {
    meta_attr <- attr(x, "meta")
    meta_attr$object$backfill <- c(meta_attr$object$backfill, passes)
    attr(x, "meta") <- meta_attr
  }
  x
}


#' Complete a replication the way its parent was completed
#'
#' A replication is meant to be comparable with its parent, so a parent that
#' was backfilled has its passes replayed on the replication: the same
#' sequence of models and overrides, each as one pass, recorded again on the
#' new object. This is the same rule that reproduces the coding path the
#' parent took rather than the mode it asked for.
#'
#' @param result The freshly replicated `qlm_coded` object.
#' @param parent The object it replicates.
#' @param backfill `NULL` to replay the parent's passes if it had any, `TRUE`
#'   to run a default [qlm_backfill()] regardless, `FALSE` to do neither.
#'
#' @return `result`, possibly backfilled.
#' @keywords internal
#' @noRd
replay_backfill <- function(result, parent, backfill = NULL) {
  if (!is.null(backfill) &&
      (!is.logical(backfill) || length(backfill) != 1L || is.na(backfill))) {
    cli::cli_abort("{.arg backfill} must be {.code TRUE}, {.code FALSE} or {.code NULL}.")
  }
  if (isFALSE(backfill)) {
    return(result)
  }
  if (isTRUE(backfill)) {
    return(qlm_backfill(result))
  }

  passes <- attr(parent, "meta")$object$backfill
  if (!length(passes)) {
    return(result)
  }
  cli::cli_inform(c(
    "i" = "Replaying the {length(passes)} backfill pass{?es} of {.val {attr(parent, 'meta')$user$name}}."
  ))
  for (pass in passes) {
    if (!any(failed_units(result))) {
      break
    }
    result <- do.call(qlm_backfill, c(
      list(result, model = pass$model, attempts = 1L),
      pass$overrides
    ))
  }
  result
}


#' Is a failure one that re-sending the same request cannot fix?
#'
#' A text longer than the context window is rejected identically every time,
#' by the same model. A response cut off at the output limit is reproduced by
#' the same limit, so it is retried only when the backfill supplies new
#' `params`, which is where the limit is set. A different model changes both,
#' so under a model change neither is terminal. Content refusals are never
#' terminal: they are not deterministic, and a retry recovers them often
#' enough to be worth it.
#'
#' @param reasons Character vector of failure reasons, `NA` for units that
#'   did not fail.
#' @param overrides The backfill's `...` overrides.
#' @param model_changed Whether the backfill uses a different model.
#'
#' @return A logical vector.
#' @keywords internal
#' @noRd
is_terminal_failure <- function(reasons, overrides = list(), model_changed = FALSE) {
  if (model_changed) {
    return(rep(FALSE, length(reasons)))
  }
  vapply(reasons, function(reason) {
    if (is.na(reason)) {
      return(FALSE)
    }
    if (is_length_rejection(reason)) {
      return(TRUE)
    }
    is_output_truncation(reason) && !"params" %in% names(overrides)
  }, logical(1), USE.NAMES = FALSE)
}


#' Was the failure a response cut off at the output limit?
#'
#' Matches the reasons quallmer records on either path, and the one ellmer's
#' `check_finish_reason()` gives, all of which name `max_tokens`.
#'
#' @param msg A failure reason.
#'
#' @return `TRUE` for a response cut off at `max_tokens`.
#' @keywords internal
#' @noRd
is_output_truncation <- function(msg) {
  length(msg) == 1L && !is.na(msg) && grepl("max_tokens", msg, fixed = TRUE)
}


#' Merge a backfill pass into the object it was run for
#'
#' Rows are matched by `.id` and keep their order. A row is replaced only
#' where the retry produced a usable coding; where it failed again, the
#' coded values are left alone and the retry's `.error` is recorded as the
#' latest reason. Usage columns are summed over both attempts, because a
#' failed request may still have been billed.
#'
#' Columns are replaced with vctrs so that list-columns (arrays), data-frame
#' columns (nested objects) and factors are handled alike; where the two
#' objects disagree on a column's type, both are cast to their common type.
#'
#' @param x The `qlm_coded` object being backfilled.
#' @param new The `qlm_coded` object a backfill pass returned, whose `.id`s
#'   are a subset of those in `x`.
#'
#' @return `x`, updated.
#' @keywords internal
#' @noRd
merge_backfill_rows <- function(x, new) {
  pos <- match(as.character(new$.id), as.character(x$.id))
  if (anyNA(pos)) {
    cli::cli_abort(
      "A backfill pass returned units that are not in the original run.",
      .internal = TRUE
    )
  }
  got <- !failed_units(new)
  usage_cols <- c("input_tokens", "output_tokens", "cached_input_tokens", "cost")
  coded_cols <- setdiff(intersect(names(x), names(new)), c(".id", ".error", usage_cols))

  # Column assignment on a tibble subclass can drop the attributes that make
  # this a qlm_coded object, so work on the columns and restore them after.
  kept <- attributes(x)[c("class", "data", "codebook", "meta")]

  if (any(got)) {
    for (col in coded_cols) {
      x[[col]] <- assign_rows(x[[col]], pos[got], vctrs::vec_slice(new[[col]], got))
    }
  }

  for (col in intersect(usage_cols, intersect(names(x), names(new)))) {
    old <- x[[col]][pos]
    add <- new[[col]]
    old[is.na(old)] <- 0
    add[is.na(add)] <- 0
    x[[col]][pos] <- old + add
  }

  if (".error" %in% names(x) || ".error" %in% names(new)) {
    had_error <- ".error" %in% names(x)
    errors <- recorded_errors(x)
    new_errors <- recorded_errors(new)
    for (j in seq_along(pos)) {
      errors[pos[j]] <- if (got[j]) list(NULL) else list(new_errors[[j]])
    }
    x$.error <- errors
    if (!had_error) {
      usage <- intersect(usage_cols, names(x))
      others <- setdiff(names(x), c(".error", usage))
      x <- x[, c(others, ".error", usage)]
    }
  }

  for (a in names(kept)) {
    attr(x, a) <- kept[[a]]
  }
  x
}


#' Replace rows of a column, reconciling types if need be
#'
#' @param old The column in the object being backfilled.
#' @param i Row positions to replace.
#' @param value The replacement values, of length `length(i)`.
#'
#' @return `old`, updated.
#' @keywords internal
#' @noRd
assign_rows <- function(old, i, value) {
  ptype <- vctrs::vec_ptype2(old, value)
  vctrs::vec_assign(vctrs::vec_cast(old, ptype), i, vctrs::vec_cast(value, ptype))
}
