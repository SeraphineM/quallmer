# @param ratings A subject x rater matrix or data.frame
# @param correct Logical; apply correction for ties
# @return An irrlist with elements: method, subjects, raters, irr.name, value,
#   stat.name, statistic, p.value (and optionally error)
# @keywords internal
# @noRd
reliability_kendall_w <- function(ratings, correct = FALSE) {
  irr::kendall(ratings, correct = correct)
}
