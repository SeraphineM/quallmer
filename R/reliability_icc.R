#' Intraclass correlation coefficient
#'
#' `r lifecycle::badge("experimental")`
#'
#' Thin wrapper around [irr::icc()]; awaiting native replacement under
#' issue #112. Computes the intraclass correlation coefficient for
#' interval/ratio agreement.
#'
#' @param ratings A `subjects x raters` matrix or data.frame.
#' @param model `"oneway"` (raters are random) or `"twoway"` (raters and
#'   subjects are both random -- the more common choice for IRR).
#' @param type `"consistency"` (relative agreement) or `"agreement"`
#'   (absolute agreement, including rater bias).
#' @param unit `"single"` (reliability of one rater's score) or
#'   `"average"` (reliability of the mean across raters).
#' @param r0 Null-hypothesis value for the ICC.
#' @param conf.level Confidence level for the analytic CI.
#'
#' @return The list returned by [irr::icc()], including `value`,
#'   `lbound`, `ubound`, `Fvalue`, `p.value`, and metadata.
#'
#' @keywords internal
reliability_icc <- function(ratings,
                            model = c("oneway", "twoway"),
                            type  = c("consistency", "agreement"),
                            unit  = c("single", "average"),
                            r0 = 0, conf.level = 0.95) {
  model <- match.arg(model)
  type  <- match.arg(type)
  unit  <- match.arg(unit)
  irr::icc(ratings, model = model, type = type, unit = unit,
           r0 = r0, conf.level = conf.level)
}
