# tibble is imported by name so that its namespace loads with quallmer's.
# The print methods of qlm_coded and the other classes reach print.tbl_df()
# through NextMethod(), and a namespaced tibble::call() loads tibble only
# once something has made one; before that, print() fell through to
# print.data.frame(), which cannot format a condition in an .error cell.
#' @keywords internal
#' @import ellmer
#' @importFrom tibble tibble
#'
#' @references
#' Krippendorff, K. (2019). Content Analysis: An Introduction to Its
#' Methodology. 4th ed. Thousand Oaks, CA: SAGE. \doi{10.4135/9781071878781}
#'
#' Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
#' Psychological Bulletin, 76(5), 378–382. \doi{10.1037/h0031619}
#'
#' Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational
#' and Psychological Measurement, 20(1), 37–46. \doi{10.1177/001316446002000104}
#'
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses
#' in assessing rater reliability. *Psychological Bulletin*, 86(2),
#' 420-428. \doi{10.1037/0033-2909.86.2.420}
#'
#' Sokolova, M., & Lapalme, G. (2009). A systematic analysis of performance
#' measures for classification tasks. Information Processing & Management,
#' 45(4), 427–437. \doi{10.1016/j.ipm.2009.03.002}
#'
#' Wickham H, Cheng J, Jacobs A, Aden-Buie G, Schloerke B (2025). _ellmer: Chat
#' with Large Language Models_. R package. <https://github.com/tidyverse/ellmer>
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
