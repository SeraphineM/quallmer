#' Is this a single whole number that `as.integer()` can hold?
#'
#' The test every count argument shares: `json_retries`, `backfill` once its
#' logical forms are set aside, and `passes`. Each of these bounds paid
#' calls, so a bad value has to be refused before the first one, and the
#' cases a simple check lets through are exactly the ones that fail late:
#' `Inf` is numeric, non-negative and equal to its truncation, and a finite
#' value above `.Machine$integer.max` is too; either becomes `NA` with a
#' warning in `as.integer()` and errors somewhere later, after money has
#' been spent.
#'
#' @param x Any value.
#' @param min The smallest acceptable value.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
is_count <- function(x, min = 0L) {
  length(x) == 1L && is.numeric(x) && is.finite(x) && x >= min &&
    x == trunc(x) && x <= .Machine$integer.max
}
