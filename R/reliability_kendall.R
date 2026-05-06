#' Kendall's W coefficient of concordance
#'
#' `r lifecycle::badge("experimental")`
#'
#' Thin wrapper around [irr::kendall()]; awaiting native replacement
#' under issue #112. Computes Kendall's coefficient of concordance for
#' ordinal agreement among `m` raters across `N` subjects.
#'
#' @param ratings A `subjects x raters` matrix or data.frame.
#' @param correct Logical; apply Kendall's tie correction.
#'
#' @return The list returned by [irr::kendall()] (irrlist with elements
#'   `method`, `subjects`, `raters`, `irr.name`, `value`, `stat.name`,
#'   `statistic`, `p.value`, optionally `error`).
#'
#' @keywords internal
reliability_kendall_w <- function(ratings, correct = FALSE) {
  irr::kendall(ratings, correct = correct)
}
