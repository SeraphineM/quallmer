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
#' which model, and `print()` and [qlm_trail()] say so, since a result coded
#' by two instruments has to be disclosed as one.
#'
#' @param x qlm_coded; a coded object produced by [qlm_code()] or [qlm_replicate()].
#' @param ... optional overrides passed to [qlm_code()] for the backfill
#'   passes, such as `params`, `max_active` or `on_error`. Any setting not
#'   overridden is restored from the original run, as [qlm_replicate()] does,
#'   with the same rule for credentials and endpoint settings when the
#'   provider changes. The codebook cannot be changed.
#' @param model character or `NULL`; the model for the passes, in the form
#'   used by [qlm_code()]. `NULL` (default) uses the run's own model.
#' @param attempts integer; the maximum number of passes. Default is 2. A pass that
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
#'   the backfill raises the limit, by passing a `params(max_tokens = )`
#'   higher than the run's own. Other `params` leave the limit where it was,
#'   and so leave those units alone.
#'
#' Content refusals are deliberately retried. They look deterministic and are
#' not: the same document is refused on one pass and coded on the next, at
#' more than one provider.
#'
#' Each pass is an ordinary [qlm_code()] call over the failed units, on the
#' path the original run took (a run that fell back to JSON mode is backfilled
#' in JSON mode; with a different endpoint the path is chosen afresh), and
#' always as a parallel call: a run coded through the batch API is backfilled
#' through the parallel API, with the same model and settings, and any
#' batch-only arguments (`path`, `wait`, `ignore_hash`) set aside. A pass that
#' fails outright on the first attempt is an error, since nothing has been
#' gained yet and the cause is most likely configuration; on a later pass it
#' is a warning, and what earlier passes recovered is kept.
#'
#' Units are identified by `.id` throughout: the failed units' inputs are
#' looked up by `.id`, so an object whose rows have been reordered or subset
#' is backfilled correctly, and the merge is by `.id`. Rows keep their order;
#' a unit is replaced only when the retry produced a usable coding, so a retry
#' that failed again never overwrites anything, though its `.error` is
#' recorded as the latest reason. Token and cost columns, when present, are
#' summed across all attempts, since a failed request may still have been
#' billed. The passes are recorded in the object metadata as `backfill`, one
#' entry per pass with its timestamp, the model if it differed from the run's,
#' the overrides, and the `.id`s attempted and recovered, so the result can say
#' which of its rows came from which pass and which model. [qlm_replicate()]
#' replays these passes on a replication, so that a replication of a completed
#' run is completed on the same terms.
#'
#' @return `x`, with the recovered units filled in and `backfill` added to
#'   its object metadata. The run name, parent, codebook and inputs are
#'   unchanged.
#'
#' @seealso [qlm_failures()] for the units a run failed on and why;
#'   [qlm_code()], whose `backfill_attempts` completes a run in the same call;
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
  check_unique_ids(x$.id, what = "{.arg x}")

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
  limit_raised <- raises_output_limit(overrides, meta_attr$object$chat_args)
  call_args <- restored$call_args
  # A backfill is a small parallel call whatever the original run was; the
  # batch API's cache arguments would point at the original run's file.
  batch_only <- setdiff(
    names(formals(ellmer::batch_chat_structured)),
    names(formals(ellmer::parallel_chat_structured))
  )
  call_args[intersect(batch_only, names(call_args))] <- NULL

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
    terminal <- failed & is_terminal_failure(
      recorded_errors(x), limit_raised = limit_raised, model_changed = model_changed
    )
    retry <- which(failed & !terminal)

    if (attempt == 1L && any(terminal)) {
      cli::cli_inform(c(
        "i" = "Leaving {sum(terminal)} unit{?s} alone: rejected on length, or cut off at {.code max_tokens}. A different {.arg model}, or a higher {.code params(max_tokens = )}, would retry them."
      ))
    }
    if (!length(retry)) {
      cli::cli_inform(c("i" = "Nothing recoverable remains."))
      break
    }

    cli::cli_inform(c(
      "i" = "Backfill pass {attempt} of {attempts}: re-coding {length(retry)} unit{?s} with {.val {restored$model}}."
    ))

    subset <- inputs_by_id(x, ids[retry])
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


#' The inputs of given units, looked up by `.id`
#'
#' `inputs()` returns the run's original input vector in its original order,
#' which is not the row order of an object that has been reordered or subset.
#' `.id` is the key: for a named input it is the name, and for an unnamed one
#' it is the position, which [qlm_code()] assigned as the sequence. Anything
#' that cannot be mapped that way is an error rather than a guess, because a
#' wrong mapping would silently code one text under another's identifier.
#'
#' @param x A `qlm_coded` object.
#' @param ids Character vector of `.id` values.
#'
#' @return The matching elements of `inputs(x)`, named by `ids`.
#' @keywords internal
#' @noRd
inputs_by_id <- function(x, ids) {
  data <- inputs(x)
  pos <- if (!is.null(names(data))) {
    match(ids, names(data))
  } else {
    suppressWarnings(as.integer(ids))
  }
  bad <- is.na(pos) | pos < 1L | pos > length(data) |
    (is.null(names(data)) & as.character(pos) != ids)
  if (any(bad)) {
    cli::cli_abort(c(
      "Cannot find the input for {sum(bad)} unit{?s} of {.arg x} by {.field .id}: {.val {ids[bad]}}.",
      "i" = "The {.field .id} of each row must be the name, or for unnamed input the position, of its input in {.fn inputs}."
    ))
  }
  out <- data[pos]
  names(out) <- ids
  out
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
  check_backfill_arg(backfill)
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


#' Check the `backfill` argument of qlm_replicate()
#'
#' Called at the top of [qlm_replicate()], before the replication is coded,
#' so that a bad value is caught before a paid call rather than after it.
#'
#' @param backfill The argument.
#' @param call The call to report the error against.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
check_backfill_arg <- function(backfill, call = rlang::caller_env()) {
  if (!is.null(backfill) &&
      (!is.logical(backfill) || length(backfill) != 1L || is.na(backfill))) {
    cli::cli_abort("{.arg backfill} must be {.code TRUE}, {.code FALSE} or {.code NULL}.",
                   call = call)
  }
  invisible(NULL)
}


#' Does the backfill raise the run's output limit?
#'
#' Only a `params(max_tokens = )` above the run's own counts. Other `params`
#' leave the limit as it was, through the merge in `restore_run_args()`, and
#' so cannot change the outcome for a response cut off at that limit. When
#' the run declared no limit, the provider's default applied, which is not
#' known here; an explicit limit then counts as raising it.
#'
#' @param overrides The backfill's `...` overrides.
#' @param chat_args The run's recorded arguments to [ellmer::chat()].
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
raises_output_limit <- function(overrides, chat_args) {
  new <- declared_max_tokens(overrides)
  if (is.null(new)) {
    return(FALSE)
  }
  old <- declared_max_tokens(chat_args)
  is.null(old) || new > old
}


#' Is a failure one that re-sending the same request cannot fix?
#'
#' A text longer than the context window is rejected identically every time,
#' by the same model. A response cut off at the output limit is reproduced by
#' the same limit, so it is retried only when the backfill raises that limit.
#' A different model changes both, so under a model change neither is
#' terminal. Content refusals are never terminal: they are not deterministic,
#' and a retry recovers them often enough to be worth it. A unit with no
#' recorded error, failed by returning `NA` for every required property, is
#' never terminal either.
#'
#' @param errors The list from `recorded_errors()`.
#' @param limit_raised Whether the backfill raises the output limit.
#' @param model_changed Whether the backfill uses a different model.
#'
#' @return A logical vector, one element per unit.
#' @keywords internal
#' @noRd
is_terminal_failure <- function(errors, limit_raised = FALSE, model_changed = FALSE) {
  if (model_changed) {
    return(rep(FALSE, length(errors)))
  }
  vapply(errors, function(e) {
    if (is.null(e)) {
      return(FALSE)
    }
    if (is_output_truncation(e)) {
      return(!limit_raised)
    }
    is_length_rejection(condition_text(e))
  }, logical(1), USE.NAMES = FALSE)
}


#' An error recorded for a response cut off at the output limit
#'
#' Carries a class of its own so that a backfill can recognise it without
#' reading the message, which a validation error could also contain (a
#' schema property named `max_tokens`, say).
#'
#' @param message The reason.
#'
#' @return A condition inheriting from `simpleError`.
#' @keywords internal
#' @noRd
truncation_error <- function(message) {
  structure(
    simpleError(message),
    class = c("quallmer_truncation_error", "simpleError", "error", "condition")
  )
}


#' Was the failure a response cut off at the output limit?
#'
#' By class where quallmer recorded it. Objects coded before the class existed
#' carry plain conditions, so the exact phrasings quallmer and ellmer use for
#' the same event are recognised as well; nothing else is, since any other
#' message that happens to mention `max_tokens` (a validation error naming a
#' schema property) is a failure a backfill should retry.
#'
#' @param e A condition, or a message.
#'
#' @return `TRUE` for a response cut off at `max_tokens`.
#' @keywords internal
#' @noRd
is_output_truncation <- function(e) {
  if (inherits(e, "quallmer_truncation_error")) {
    return(TRUE)
  }
  msg <- condition_text(e)
  if (length(msg) != 1L || is.na(msg)) {
    return(FALSE)
  }
  grepl(
    paste0(
      "^The response was cut off at the max_tokens limit|",
      "^The response used the whole max_tokens limit|",
      "^Response was truncated because it hit the `?max_tokens`? limit"
    ),
    msg
  )
}

condition_text <- function(e) {
  if (inherits(e, "condition")) {
    strip_ansi(conditionMessage(e))
  } else if (is.character(e)) {
    e
  } else {
    NA_character_
  }
}


#' Describe a backfill for a header line
#'
#' Shared by `print.qlm_coded()` and [qlm_trail()], so that both disclose the
#' same thing the same way: how many passes, how many of the units attempted
#' were recovered, and how many of those a model other than the run's own
#' supplied, which makes the object a composite.
#'
#' @param passes The `backfill` entry of the object metadata.
#'
#' @return A single string, or `NULL` when there were no passes.
#' @keywords internal
#' @noRd
backfill_summary <- function(passes) {
  if (!length(passes)) {
    return(NULL)
  }
  n_pass <- length(passes)
  recovered <- lengths(lapply(passes, `[[`, "recovered"))
  n_attempted <- length(unique(unlist(lapply(passes, `[[`, "attempted"))))
  other <- vapply(passes, function(p) p$model %||% NA_character_, character(1))
  by_other <- tapply(recovered[!is.na(other)], other[!is.na(other)], sum)
  by_other <- by_other[by_other > 0]
  detail <- if (length(by_other)) {
    paste0(" (", paste0(by_other, " with ", names(by_other), collapse = ", "), ")")
  } else {
    ""
  }
  paste0(
    n_pass, if (n_pass == 1L) " pass" else " passes",
    ", recovered ", sum(recovered), " of ", n_attempted, detail
  )
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
  check_unique_ids(x$.id, what = "{.arg x}")
  check_unique_ids(new$.id, what = "the backfill pass")
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
