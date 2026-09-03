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
#' * it carries an `.error`. ellmer records one when the request failed or
#'   the response could not be converted to the schema, and quallmer's JSON
#'   path records one when a response never validated; or
#' * every required scalar property of the codebook schema is `NA` for it.
#'   A structured call can succeed at the HTTP level and still return nothing
#'   usable, when the endpoint accepted the JSON schema and ignored it, so an
#'   `.error` alone is not a sufficient test.
#'
#' Array and nested-object properties are not consulted. After conversion, a
#' missing array and a schema-valid empty one are the same zero-length
#' list-column cell, so neither `is.na()` nor a row count on such a column
#' can tell failure from a unit to which nothing applied. For a codebook whose
#' required properties are all arrays or nested objects, only `.error`
#' identifies failed units, which is what ellmer sets when a request fails.
#'
#' @param x A `qlm_coded` object.
#'
#' @return A tibble with one row per failed unit and columns `.id`, `reason`
#'   (a character description) and `.error` (the recorded condition, or `NULL`
#'   for a unit that failed by returning `NA` for every required property).
#'   Zero rows when every unit was coded.
#'
#' @seealso [qlm_code()], whose `on_error = "continue"` is what leaves failed
#'   units in the object rather than stopping the run; [accessors] for the
#'   other accessor functions.
#'
#' @examples
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#' qlm_failures(examples$example_coded_sentiment)
#'
#' @export
qlm_failures <- function(x) {
  if (!inherits(x, "qlm_coded")) {
    cli::cli_abort(c(
      "{.arg x} must be a {.cls qlm_coded} object.",
      "i" = "Got {.cls {class(x)}}."
    ))
  }

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
  errored <- !vapply(recorded_errors(x), is.null, logical(1))

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
