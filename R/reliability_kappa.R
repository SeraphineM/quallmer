# @param ratings A subject x rater matrix or data.frame with exactly 2 columns
# @param weight One of "unweighted", "equal", "squared", or a numeric vector
# @param sort.levels Logical; sort factor levels
# @return An irrlist with elements: method, subjects, raters, irr.name, value,
#   stat.name, statistic, p.value
# @keywords internal
# @noRd
reliability_kappa2 <- function(ratings, weight = c("unweighted", "equal", "squared"),
                               sort.levels = FALSE) {
  if (is.character(weight)) weight <- match.arg(weight)
  irr::kappa2(ratings, weight = weight, sort.levels = sort.levels)
}


# @param ratings A subject x rater matrix or data.frame with 2 or more columns
# @param exact Logical; use exact kappa variant
# @param detail Logical; include per-category kappa values
# @return An irrlist with elements: method, subjects, raters, irr.name, value,
#   stat.name, statistic, p.value (and optionally detail)
# @keywords internal
# @noRd
reliability_kappam_fleiss <- function(ratings, exact = FALSE, detail = FALSE) {
  irr::kappam.fleiss(ratings, exact = exact, detail = detail)
}
