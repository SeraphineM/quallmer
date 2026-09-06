#' List the units a coding run failed on
#'
#' Reports which units of a `qlm_coded` object produced no usable coding, and
#' why. A run over a real corpus rarely comes back complete: requests fail,
#' providers refuse a text or reject it on length, and an endpoint can accept
#' a schema and then ignore it. The object records all of this, in an
#' `.error` list-column and as `NA` values, but nothing about its shape says
#' how many units were affected, and for an array-valued property the obvious
#' check does not work (see below). `print()` uses the same test to report a
#' count.
#'
#' A unit counts as failed when either of two things holds:
#'
#' * it carries an `.error`. [qlm_code()] records one when the request
#'   failed, when the provider cut the response off or withheld it (see the
#'   *Truncated responses* section of [qlm_code()]), when the response held
#'   no JSON or JSON that did not parse, and when its JSON did not match the
#'   codebook schema, naming the offending path; or
#' * every required scalar property of the codebook schema is `NA` for it.
#'   That is how an object coded before every response was validated shows
#'   a response the endpoint sent without honouring the schema; a run coded
#'   since records such a unit under the first rule.
#'
#' Array and nested-object properties are not consulted. After conversion, a
#' missing array and a schema-valid empty one are the same zero-length
#' list-column cell, so neither `is.na()` nor a row count on such a column
#' can tell failure from a unit to which nothing applied. For a codebook whose
#' required properties are all arrays or nested objects, only `.error`
#' identifies failed units.
#'
#' @param x A `qlm_coded` object.
#'
#' @return A tibble with one row per failed unit and columns `.id`, `reason`
#'   (a character description) and `.error` (the recorded condition, or `NULL`
#'   for a unit that failed by returning `NA` for every required property).
#'   Zero rows when every unit was coded.
#'
#' @seealso [qlm_backfill()] to re-code the failed units; [qlm_code()], whose
#'   default `on_error = "continue"` attempts every unit and leaves the failed
#'   ones in the object rather than stopping the run; [accessors] for the
#'   other accessor functions.
#'
#' @examples
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#'
#' # A complete run: zero rows
#' qlm_failures(examples$example_coded_sentiment)
#'
#' # A run that came back incomplete: a request that timed out, and responses
#' # cut off at max_tokens
#' qlm_failures(examples$example_coded_incomplete)
#'
#' # The same run after qlm_backfill(): the timed-out unit recovered, the
#' # cut-off ones left alone, since re-sending the request cannot fix them
#' qlm_failures(examples$example_coded_backfilled)
#'
#' @export
qlm_failures <- function(x) {
  x <- check_qlm_coded(x)

  failed <- failed_units(x)
  errors <- recorded_errors(x)

  tibble::tibble(
    .id = x$.id[failed],
    reason = failure_reasons(errors, failed)[failed],
    .error = errors[failed]
  )
}


#' Which units of a coded object failed?
#'
#' The single definition of "failed" shared by [qlm_failures()] and
#' `print.qlm_coded()`. See the documentation of [qlm_failures()] for what
#' counts and why arrays are not consulted.
#'
#' @param x A `qlm_coded` object.
#' @return A logical vector, one element per row of `x`.
#' @keywords internal
#' @noRd
failed_units <- function(x) {
  errored <- errored_rows(x)

  # required_scalar_fields() ignores arrays and nested objects deliberately;
  # for those, empty is not missing.
  schema <- codebook(x)$schema
  fields <- intersect(required_scalar_fields(schema), names(x))
  blank <- if (length(fields)) {
    Reduce(`&`, lapply(fields, function(f) is.na(x[[f]])))
  } else {
    rep(FALSE, nrow(x))
  }

  errored | blank
}


#' The .error column, or a list of NULLs when there is none
#'
#' ellmer adds `.error` only when some request failed, so its absence is the
#' common case and means no recorded errors, not an older object.
#'
#' @param x A `qlm_coded` object.
#' @return A list of length `nrow(x)`.
#' @keywords internal
#' @noRd
recorded_errors <- function(x) {
  if (".error" %in% names(x)) {
    as.list(x$.error)
  } else {
    vector("list", nrow(x))
  }
}


#' Which rows carry a recorded error?
#'
#' @param x A `qlm_coded` object, or any data frame that may have an `.error`
#'   list-column.
#' @return A logical vector of length `nrow(x)`.
#' @keywords internal
#' @noRd
errored_rows <- function(x) {
  !vapply(recorded_errors(x), is.null, logical(1))
}


#' Describe why each failed unit failed
#'
#' @param errors The list from `recorded_errors()`.
#' @param failed The logical vector from `failed_units()`.
#' @return A character vector, `NA` for units that did not fail.
#' @keywords internal
#' @noRd
failure_reasons <- function(errors, failed) {
  vapply(seq_along(errors), function(i) {
    e <- errors[[i]]
    if (inherits(e, "condition")) {
      # cli colours API errors; the codes would otherwise travel into reports
      strip_ansi(conditionMessage(e))
    } else if (!is.null(e)) {
      paste(as.character(e), collapse = " ")
    } else if (failed[[i]]) {
      "every required property is NA"
    } else {
      NA_character_
    }
  }, character(1))
}


#' Abort unless unit identifiers form a key
#'
#' `.id` is the key on which comparison, validation and backfill all merge.
#' A missing value is not an identity, and base `merge()` pairs `NA` with
#' `NA`, so two units with no identity would be matched with each other; a
#' repeated value pairs the wrong rows as a Cartesian product (#156). Both
#' are refused. Enforced wherever identifiers enter, [qlm_code()] on its
#' input names and the constructor on the finished table, and again at each
#' public merge boundary, since ordinary row subsetting keeps the class and
#' objects from before the check exist.
#'
#' @param ids The identifiers.
#' @param what cli markup naming what they are, for the message.
#' @param call The call to report the error against.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
check_ids <- function(ids, what = "{.field .id}", call = rlang::caller_env()) {
  # A key is a plain vector of labels or numbers. A list-column (with NULL
  # cells, say) or a matrix would pass the tests below in odd ways and then
  # fail deep inside a merge, so it is refused here, by name.
  if (!is.atomic(ids) || !is.null(dim(ids))) {
    cli::cli_abort(c(
      paste0(what, " must be a character, factor or numeric vector, not {.cls {class(ids)}}."),
      "i" = "One identifier per unit, with nothing nested in it."
    ), call = call)
  }
  missing <- is.na(ids)
  # A factor is a common identifier column; its labels are checked like text
  if (is.character(ids) || is.factor(ids)) {
    missing <- missing | !nzchar(as.character(ids))
  }
  if (any(missing)) {
    cli::cli_abort(c(
      paste0(what, " must not be missing: {sum(missing)} value{?s} {?is/are} {.code NA} or empty."),
      "i" = "A unit without an identifier cannot be matched to anything, and would be matched to every other unit without one."
    ), call = call)
  }
  dups <- unique(ids[duplicated(ids)])
  if (length(dups)) {
    shown <- utils::head(dups, 5)
    more <- if (length(dups) > 5) ", ..." else ""
    cli::cli_abort(c(
      paste0(what, " must be unique: {length(dups)} value{?s} occur{?s/} more than once."),
      "x" = "Duplicated: {.val {shown}}{more}",
      "i" = "Every later operation merges on {.field .id}, and a repeated value would pair the wrong rows."
    ), call = call)
  }
  invisible(NULL)
}


#' Abort unless an object is a well-formed qlm_coded object
#'
#' The single integrity check run by every function that takes a
#' `qlm_coded` object, before it does anything with it. The class alone
#' proves little: tibble and dplyr operations keep the class and attributes
#' through row operations, so `dplyr::slice(x, c(1, 1))` or `rbind(x, x)` is
#' a classed object with a repeated key, and an object can be forged by
#' assignment or saved before a check existed. So the object is checked for
#' what the functions rely on: its run metadata, exactly one `.id` column,
#' and `.id` being a key (see `check_ids()`). A codebook is not required
#' here: a trail, a failure listing or a comparison with explicit levels
#' works without one, and the functions that need it say so themselves.
#'
#' @param x The object.
#' @param what cli markup naming it, for the message.
#' @param call The call to report the error against.
#'
#' @return `x`, with its metadata upgraded if it was in the old layout.
#' @keywords internal
#' @noRd
check_qlm_coded <- function(x, what = "{.arg x}", call = rlang::caller_env()) {
  if (!inherits(x, "qlm_coded")) {
    cli::cli_abort(paste0(what, " must be a {.cls qlm_coded} object."), call = call)
  }
  x <- upgrade_meta(x)
  if (is.null(attr(x, "meta")) || is.null(attr(x, "meta")$object)) {
    cli::cli_abort(c(
      paste0(what, " has no run metadata."),
      "i" = "A {.cls qlm_coded} object carries its run in the {.field meta} attribute; this one has lost it."
    ), call = call)
  }
  n_id <- sum(names(x) == ".id")
  if (n_id != 1L) {
    cli::cli_abort(c(
      paste0(what, " must have exactly one {.field .id} column; found {n_id}."),
      "i" = "{.field .id} identifies each unit and is the key every operation merges on."
    ), call = call)
  }
  check_ids(x[[".id"]], what = paste0("{.field .id} of ", what), call = call)
  x
}
