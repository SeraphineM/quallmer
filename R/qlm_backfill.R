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
#'   provider changes. The codebook, `batch`, `name` and `backfill` cannot be
#'   set: `passes` is the only bound on the number of passes.
#'   Nor can `include_tokens` and `include_cost`: usage is recorded as the run
#'   recorded it, so that the merged columns mean one thing. `prices` may be
#'   given, to cost the passes where the run's own rates do not carry (a
#'   batch-coded run's rates do not cover its parallel passes), and needs the
#'   run to have recorded token counts and cost of its own to add to.
#' @param model character or `NULL`; the model for the passes, in the form
#'   used by [qlm_code()]. `NULL` (default) uses the run's own model.
#' @param passes A single positive integer giving the maximum number of
#'   backfill passes. Default is 2. This counts total passes, not additional
#'   retries. Backfilling stops early when a pass recovers no units, since
#'   the failures that remain are then evidently not transient.
#'
#' @details
#' Which units are re-coded is decided afresh on every pass, from the object as
#' it then stands, by the same test [qlm_failures()] uses: a unit that carries
#' an `.error`, or whose required scalar properties are all `NA`. Two kinds of
#' failure are left alone, because re-sending the same request cannot change
#' the outcome:
#'
#' * a text the provider rejected as longer than the model's context window,
#'   unless the model or endpoint changes;
#' * a response cut off at the `max_tokens` limit, unless the model or endpoint
#'   changes or the backfill raises the limit, by passing `params(max_tokens = )`
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
#' is a warning, and what earlier passes recovered is kept. The failed pass is
#' still recorded, with the units it attempted, no recoveries and the error,
#' since the provider may have billed it, and for the same reason the token
#' and cost columns of the units it attempted become `NA`: how much was
#' billed is not known, so no total for them is.
#'
#' Units are identified by `.id` throughout: the failed units' inputs are
#' looked up by `.id`, so an object whose rows have been reordered or subset
#' is backfilled correctly, and the merge is by `.id`. Rows keep their order;
#' a unit is replaced only when the retry produced a usable coding, so a retry
#' that failed again never overwrites anything, though its `.error` is
#' recorded as the latest reason. Token and cost columns, when present, are
#' summed across all attempts, since a failed request may still have been
#' billed; a total is `NA` when any attempt's figure is, because `NA` means
#' the provider did not report it, not that nothing was billed, so a retry's
#' known figure cannot stand in for the whole. The passes are recorded in the
#' object metadata as `backfill`, one entry per pass with its timestamp, the
#' model if it or the endpoint differed from the run's, the overrides, the
#' `.id`s attempted and recovered, where its cost came from when that was not
#' where the run's did, and for a pass that failed outright its error, so the
#' result can say
#' which of its rows came from which pass and which model. A pass whose cost
#' came from somewhere else than the run's, other supplied rates, ellmer's
#' own table where the run rested on supplied rates, or nowhere where the
#' run was priced, is disclosed by `print()` and [qlm_trail()] beside the
#' run's own cost note, since part of the cost column then rests on it.
#' [qlm_trail()] redacts any credential among a pass's overrides as it does
#' the run's own, and a pass replayed from a trail does not send a redacted
#' value. [qlm_replicate()] replays these passes on a replication, so that a
#' replication of a completed run is completed on the same terms.
#'
#' @return `x`, with the recovered units filled in and `backfill` added to
#'   its object metadata. The run name, parent, codebook and inputs are
#'   unchanged.
#'
#' @seealso [qlm_failures()] for the units a run failed on and why;
#'   [qlm_code()], whose `backfill` completes a run in the same call;
#'   [qlm_replicate()] to re-run a whole coding.
#'
#' @examples
#' # A run that came back incomplete, and what qlm_backfill() made of it. Both
#' # were coded once and saved with the package (see data_creation/ in the
#' # source), so they can be looked at without a key.
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#' incomplete <- examples$example_coded_incomplete
#' incomplete
#' qlm_failures(incomplete)
#'
#' # What qlm_backfill(incomplete) returned: the timed-out unit re-coded, the
#' # responses cut off at max_tokens left alone, and the pass on record
#' filled <- examples$example_coded_backfilled
#' filled
#' qlm_failures(filled)
#' qlm_meta(filled, "backfill", type = "object")
#'
#' \dontrun{
#' filled <- qlm_backfill(incomplete)
#'
#' # Responses cut off at the output limit are retried only with a higher one
#' filled <- qlm_backfill(filled, params = ellmer::params(max_tokens = 2000))
#'
#' # Units one model refuses or cannot fit, coded by another; the result
#' # records which units came from which model
#' filled <- qlm_backfill(filled, model = "deepseek/deepseek-chat")
#' }
#'
#' @export
qlm_backfill <- function(x, ..., model = NULL, passes = 2L) {
  # Integrity first: the class, the run metadata, and .id being a key, since
  # the merge below is by .id; also upgrades an old metadata layout
  x <- check_qlm_coded(x)
  meta_attr <- attr(x, "meta")

  if (identical(meta_attr$object$source, "human")) {
    cli::cli_abort(c(
      "{.arg x} is human-coded; there is no coding run to re-code.",
      "i" = "{.fn qlm_backfill} re-runs the model of a {.fn qlm_code} run on the units it failed on."
    ))
  }
  if (!is_count(passes, min = 1L)) {
    cli::cli_abort("{.arg passes} must be a single positive integer.")
  }
  passes <- as.integer(passes)
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
  # `passes` is the bound on paid calls. Letting qlm_code()'s `backfill`
  # through to the passes would nest up to that many further passes inside
  # each one, multiplying the calls and losing the nested passes' provenance
  # in the merge. `name` is set per pass; the object keeps the run's own name.
  if ("backfill" %in% names(overrides)) {
    cli::cli_abort(c(
      "{.arg backfill} cannot be set in a backfill.",
      "i" = "Use {.arg passes} for the number of passes."
    ))
  }
  if ("name" %in% names(overrides)) {
    cli::cli_abort(c(
      "{.arg name} cannot be set in a backfill.",
      "i" = "A backfill fills the gaps in a run under the run's own name."
    ))
  }
  # Usage is recorded as the run recorded it, so that a merged column means
  # one thing. A pass that recorded more than the run could not be merged
  # truthfully, since the earlier attempts' usage is unknown; one that
  # recorded less would leave the run's own figures looking complete.
  usage_flags <- intersect(c("include_tokens", "include_cost"), names(overrides))
  if (length(usage_flags)) {
    cli::cli_abort(c(
      "{.arg {usage_flags}} cannot be set in a backfill.",
      "i" = "A backfill records token counts and cost as the run did."
    ))
  }
  # Supplied rates cost a pass from its token counts, and a merged row is
  # known only when the run's own figure is too, so without usage recorded
  # on the run the rates would be spent for nothing.
  usage_cols <- c("input_tokens", "output_tokens", "cached_input_tokens", "cost")
  if ("prices" %in% names(overrides) && !all(usage_cols %in% names(x))) {
    cli::cli_abort(c(
      "{.arg prices} cannot cost a backfill of a run that recorded no usage.",
      "i" = paste0("The run was coded without {.code include_tokens = TRUE} and ",
                   "{.code include_cost = TRUE}, so its own attempts have no token ",
                   "counts or cost for the passes' to be added to.")
    ))
  }

  run_model <- meta_attr$object$chat_args$name
  restored <- restore_run_args(x, overrides = overrides, model = model, batch = FALSE)
  overrides <- restored$overrides
  route_changed <- !identical(restored$model, run_model) || restored$endpoint_changed
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
  records <- list()

  for (attempt in seq_len(passes)) {
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
      recorded_errors(x), limit_raised = limit_raised, model_changed = route_changed
    )
    retry <- which(failed & !terminal)

    if (attempt == 1L && any(terminal)) {
      cli::cli_inform(c(
        "i" = "Leaving {sum(terminal)} unit{?s} alone: rejected on length, or cut off at {.code max_tokens}. A different {.arg model} or endpoint, or a higher {.code params(max_tokens = )}, would retry them."
      ))
    }
    if (!length(retry)) {
      cli::cli_inform(c("i" = "Nothing recoverable remains."))
      break
    }

    cli::cli_inform(c(
      "i" = "Backfill pass {attempt} of {passes}: re-coding {length(retry)} unit{?s} with {.val {restored$model}}."
    ))

    subset <- inputs_by_id(x, ids[retry])
    # The files about to be uploaded again must be the ones the run coded;
    # a URL is checked by the pass against the download it uploads
    verify_input_files(x, ids[retry])
    result <- tryCatch(
      with_expected_hashes(expected_url_hashes(x, ids[retry]), do.call(qlm_code, c(
        list(
          x = subset,
          codebook = codebook(x),
          model = restored$model,
          batch = FALSE,
          name = paste0(run_name %||% "run", "_backfill_", attempt),
          backfill = FALSE
        ),
        call_args
      ))),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      if (attempt == 1L) {
        # Classed, and carrying what the pass attempted, so that
        # replay_backfill() can record the pass without losing the
        # replication it was completing.
        cli::cli_abort(c(
          "The backfill pass failed before recovering anything.",
          set_bullets(strip_ansi(conditionMessage(result))),
          "i" = "Nothing was changed."
        ), class = "quallmer_backfill_error",
        attempted = ids[retry], cause = condition_text(result))
      }
      cli::cli_warn(c(
        "Backfill pass {attempt} failed; keeping what earlier passes recovered.",
        set_bullets(strip_ansi(conditionMessage(result)))
      ))
      # Recorded even though nothing is merged: the pass was attempted, and
      # the provider may have billed it, so the trail must show it, and the
      # units it attempted can no longer claim a known usage total.
      x <- unknown_usage(x, ids[retry])
      records[[length(records) + 1L]] <- backfill_pass(
        model = if (route_changed) restored$model else NULL,
        overrides = overrides,
        resolution = restored$resolution,
        attempted = ids[retry],
        recovered = character(0),
        error = condition_text(result)
      )
      break
    }

    x <- merge_backfill_rows(x, result)
    # The bytes the pass coded are now the bytes behind its units, so its
    # hashes replace the run's for those units; a run coded before hashes
    # were recorded gains them for exactly the units this pass re-coded
    x <- merge_input_files(x, result)
    recovered <- ids[retry][!failed_units(result)]
    # What the pass was costed on is what qlm_code() settled, not what was
    # asked: rates it was given and did not need are not recorded on it.
    result_meta <- attr(result, "meta")
    records[[length(records) + 1L]] <- backfill_pass(
      model = if (route_changed) restored$model else NULL,
      overrides = overrides,
      resolution = restored$resolution,
      attempted = ids[retry],
      recovered = recovered,
      prices = result_meta$user$prices,
      cost_note = result_meta$user$cost_note
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

  if (length(records)) {
    meta_attr <- attr(x, "meta")
    meta_attr$object$backfill <- c(meta_attr$object$backfill, records)
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
#' parent took rather than the mode it asked for. A pass the parent records
#' as failed is replayed too: what is replayed is the sequence the parent was
#' meant to be completed by, not the luck it had.
#'
#' @param result The freshly replicated `qlm_coded` object.
#' @param parent The object it replicates.
#' @param backfill `NULL` to replay the parent's passes if it had any; `TRUE`
#'   or a positive integer to run a fresh [qlm_backfill()] of the default or
#'   that many passes regardless; `FALSE` or `0` to do neither.
#'
#' @return `result`, possibly backfilled.
#' @keywords internal
#' @noRd
replay_backfill <- function(result, parent, backfill = NULL) {
  fresh <- backfill_passes(backfill, null = NULL)
  if (!is.null(fresh)) {
    if (fresh == 0L) {
      return(result)
    }
    return(qlm_backfill(result, passes = fresh))
  }

  recorded <- attr(parent, "meta")$object$backfill
  if (!length(recorded)) {
    return(result)
  }
  cli::cli_inform(c(
    "i" = "Replaying the {length(recorded)} backfill pass{?es} of {.val {attr(parent, 'meta')$user$name}}."
  ))
  for (i in seq_along(recorded)) {
    if (!any(failed_units(result))) {
      break
    }
    pass <- recorded[[i]]
    # A parent read back from a trail records "<redacted>" where a pass was
    # given a credential; that is not a value to send.
    stripped <- drop_redacted_args(pass$overrides)
    if (length(stripped$dropped)) {
      cli::cli_inform(c("i" = paste0("Pass ", i, ": ", redacted_args_note(stripped$dropped))))
    }
    # Each pass runs as its own single-attempt backfill, whose "first attempt
    # failed" rule would otherwise abort here and discard the paid replication
    # and everything earlier passes recovered. So a pass that fails outright
    # is caught: a warning, an entry in the record with what it attempted and
    # no recoveries, and no further passes. Other errors (a recorded override
    # the replication cannot take) are configuration, and propagate.
    replayed <- tryCatch(
      do.call(qlm_backfill, c(
        list(result, model = pass$model, passes = 1L),
        stripped$args
      )),
      quallmer_backfill_error = function(e) e
    )
    if (inherits(replayed, "quallmer_backfill_error")) {
      cli::cli_warn(c(
        "Replayed backfill pass {i} failed; keeping the replication and what earlier passes recovered.",
        set_bullets(replayed$cause)
      ))
      result <- unknown_usage(result, replayed$attempted)
      meta_attr <- attr(result, "meta")
      meta_attr$object$backfill <- c(meta_attr$object$backfill, list(backfill_pass(
        model = pass$model,
        overrides = pass$overrides,
        resolution = pass$provider_resolution,
        attempted = replayed$attempted,
        recovered = character(0),
        error = replayed$cause
      )))
      attr(result, "meta") <- meta_attr
      break
    }
    if (!is.null(pass$provider_resolution)) {
      replay_meta <- attr(replayed, "meta")
      last <- length(replay_meta$object$backfill)
      if (last > length(attr(result, "meta")$object$backfill)) {
        replay_meta$object$backfill[[last]]$provider_resolution <- pass$provider_resolution
      }
      attr(replayed, "meta") <- replay_meta
    }
    result <- replayed
  }
  result
}


#' Normalise a `backfill` argument to a number of passes
#'
#' `backfill` is shared by [qlm_code()] and [qlm_replicate()] and takes the
#' same values in both: `FALSE` or `0` for no backfill, `TRUE` for
#' [qlm_backfill()]'s own default number of passes, a positive integer for
#' at most that many. `NULL` is contextual, so the caller says what it means:
#' no backfill in a fresh run, which has no parent, and a replay of the
#' parent's recorded passes in a replication. Called before any paid call,
#' so that a bad value costs nothing.
#'
#' `TRUE` is recognised before the numeric test, so it means the default
#' number of passes and not one pass by coercion.
#'
#' @param backfill The argument as given.
#' @param null What to return for `NULL`.
#' @param call The call to report an error against.
#'
#' @return An integer number of passes, or `null`.
#' @keywords internal
#' @noRd
backfill_passes <- function(backfill, null = 0L, call = rlang::caller_env()) {
  if (is.null(backfill)) {
    return(null)
  }
  if (isTRUE(backfill)) {
    return(formals(qlm_backfill)$passes)
  }
  if (isFALSE(backfill)) {
    return(0L)
  }
  if (!is_count(backfill)) {
    cli::cli_abort(paste0(
      "{.arg backfill} must be {.code TRUE}, {.code FALSE}, {.code NULL} ",
      "or a single non-negative integer."
    ), call = call)
  }
  as.integer(backfill)
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
#' @param model_changed Whether the backfill uses a different model or endpoint.
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
  n_failed <- sum(!vapply(passes, function(p) is.null(p$error), logical(1)))
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
    if (n_failed) paste0(" (", n_failed, " failed)") else "",
    ", recovered ", sum(recovered), " of ", n_attempted, detail
  )
}


#' One entry of the `backfill` object metadata
#'
#' @param model The pass's model when it differed from the run's, else `NULL`.
#' @param overrides The pass's `...` overrides.
#' @param attempted,recovered Character vectors of `.id`s.
#' @param error For a pass that failed outright, its message; else `NULL`,
#'   and the element is absent.
#' @param prices,cost_note What [qlm_code()] recorded for the pass: the rates
#'   its cost rests on, and one line saying where the cost came from. `NULL`
#'   when the pass was priced by ellmer, and the elements are absent.
#' @param resolution Requested and effective model identity for a registered
#'   provider, if used.
#' @return A list.
#' @keywords internal
#' @noRd
backfill_pass <- function(model, overrides, attempted, recovered, error = NULL,
                          prices = NULL, cost_note = NULL, resolution = NULL) {
  pass <- list(
    timestamp = Sys.time(),
    model = model,
    overrides = overrides,
    attempted = attempted,
    recovered = recovered
  )
  if (!is.null(resolution)) {
    pass$provider_resolution <- resolution
  }
  if (!is.null(prices)) {
    pass$prices <- prices
  }
  if (!is.null(cost_note)) {
    pass$cost_note <- cost_note
  }
  if (!is.null(error)) {
    pass$error <- error
  }
  pass
}


#' Cost notes of the passes costed differently from the run
#'
#' Each pass records where its cost came from, as the run does, and in both
#' the absence of a note means ellmer priced it from its own table. A pass
#' costed as the run was has nothing to add. One costed on other rates, left
#' `NA` where the run was priced, or priced by ellmer where the run rested on
#' supplied rates, has to be disclosed wherever the run's own note is, since
#' part of the cost column then rests on something else (#135). A pass that
#' failed outright merged no figures and has no provenance to give.
#'
#' @param passes The `backfill` entry of the object metadata.
#' @param run_note The run's own `cost_note`, possibly `NULL`.
#'
#' @return A character vector named `"backfill pass <i>"`, possibly empty.
#' @keywords internal
#' @noRd
backfill_cost_notes <- function(passes, run_note = NULL) {
  ellmer_note <- "from ellmer's price table"
  run_note <- run_note %||% ellmer_note
  notes <- character()
  for (i in seq_along(passes)) {
    pass <- passes[[i]]
    if (!is.null(pass$error)) {
      next
    }
    note <- pass$cost_note %||% ellmer_note
    if (!identical(note, run_note)) {
      notes[paste0("backfill pass ", i)] <- note
    }
  }
  notes
}


#' Merge a backfill pass into the object it was run for
#'
#' Rows are matched by `.id` and keep their order. A row is replaced only
#' where the retry produced a usable coding; where it failed again, the
#' coded values are left alone and the retry's `.error` is recorded as the
#' latest reason. Usage columns are summed over both attempts, because a
#' failed request may still have been billed, and a total is `NA` when
#' either attempt's figure is: `NA` means the provider did not report it,
#' not that nothing was billed, so a retry's known figure cannot stand in
#' for the whole. A usage column the pass did not record leaves the merged
#' rows unknown for the same reason; one the run did not record cannot be
#' added, since the earlier attempts' usage is unknown, and is left out.
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
  check_ids(x$.id, what = "{.field .id} of {.arg x}")
  check_ids(new$.id, what = "{.field .id} of the backfill pass")
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

  for (col in intersect(usage_cols, names(x))) {
    x[[col]][pos] <- if (col %in% names(new)) x[[col]][pos] + new[[col]] else NA
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


#' Mark the usage of given units unknown
#'
#' A pass that fails outright may still have been billed for the requests it
#' sent, and how much is not known. The units it attempted then have no
#' truthful total: the run's figure is what was spent before the pass, not in
#' all. So their usage columns become `NA`, which in these columns means
#' unknown rather than nothing.
#'
#' @param x The `qlm_coded` object.
#' @param ids Character vector of `.id`s the pass attempted.
#'
#' @return `x`, with those rows' usage columns set to `NA`.
#' @keywords internal
#' @noRd
unknown_usage <- function(x, ids) {
  usage <- intersect(c("input_tokens", "output_tokens", "cached_input_tokens", "cost"), names(x))
  if (!length(usage) || !length(ids)) {
    return(x)
  }
  kept <- attributes(x)[c("class", "data", "codebook", "meta")]
  pos <- match(ids, as.character(x$.id))
  for (col in usage) {
    x[[col]][pos] <- NA
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
