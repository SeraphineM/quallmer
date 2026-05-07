#' Kendall's W coefficient of concordance
#'
#' Native implementation of Kendall's W (Kendall & Smith, 1939, Eq. 2)
#' for assessing concordance among `m` rankings of `n` objects. Each
#' column of `observations` is one rater's ordering; values are ranked
#' within each column (`rank()` with average ties), so either raw scores
#' or already-assigned ranks may be passed. The tie correction (Kendall
#' & Smith 1939, footnote on p. 277; modern textbook formula) is applied
#' automatically when ties are present.
#'
#' @param observations A `subjects x raters` matrix or data.frame.
#'   Rows are objects/units being ranked; columns are raters. Must not
#'   contain `NA`.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`method`}{`"kendall_w"`.}
#'     \item{`value`}{Numeric -- W on the interval `[0, 1]`.}
#'     \item{`ci_lower`, `ci_upper`}{`NA_real_` (W has no closed-form CI).}
#'     \item{`per_value`}{`NULL` (Kendall's W has no per-category breakdown).}
#'     \item{`n_observers`}{Number of raters (m).}
#'     \item{`n_units`}{Number of objects ranked (n).}
#'     \item{`n_pairable`}{`m * n`.}
#'     \item{`chi_squared`}{Friedman chi-square statistic, `m(n-1)W`
#'       (Kendall & Smith 1939, Eq. 5).}
#'     \item{`df`}{Degrees of freedom for the chi-square test (n - 1).}
#'     \item{`p_value`}{Upper-tail p-value from the chi-square distribution.}
#'     \item{`S`}{Sum of squared deviations of rank sums from their mean
#'       (Kendall & Smith 1939, Eq. 2 numerator / 12).}
#'   }
#'
#' @references
#' Kendall, M. G., & Babington Smith, B. (1939). The problem of m
#' rankings. *Annals of Mathematical Statistics*, 10(3), 275-287.
#' \doi{10.1214/aoms/1177732186}
#'
#' Kendall, M. G., & Gibbons, J. D. (1990). *Rank Correlation Methods*
#' (5th ed.), Chapter 6. Oxford University Press.
#'
#' @keywords internal
reliability_kendall_w <- function(observations) {
  if (is.data.frame(observations)) observations <- as.matrix(observations)
  if (!is.matrix(observations)) {
    cli::cli_abort("{.arg observations} must be a matrix or data.frame.")
  }
  if (ncol(observations) < 2L) {
    cli::cli_abort("Kendall's W requires at least 2 raters (columns).")
  }
  if (nrow(observations) < 2L) {
    cli::cli_abort("Kendall's W requires at least 2 objects (rows).")
  }
  if (anyNA(observations)) {
    cli::cli_abort(c(
      "{.arg observations} must not contain {.val NA}.",
      "i" = "Filter to complete cases first (see {.fn stats::complete.cases})."
    ))
  }

  n <- nrow(observations)   # objects
  m <- ncol(observations)   # raters

  # Rank within each rater's column; average ranks for ties (standard).
  ranks <- apply(observations, 2L, rank, ties.method = "average")

  # Sum of ranks per object and the (squared-deviation) sum S.
  R       <- rowSums(ranks)
  mean_R  <- m * (n + 1) / 2
  S       <- sum((R - mean_R)^2)

  # Tie correction T_j per rater (Kendall & Smith 1939; modern formula):
  # for each tied group of size t, contribute t^3 - t. T = sum across raters.
  Tj_total <- 0
  for (j in seq_len(m)) {
    grp <- table(ranks[, j])
    tied <- grp[grp > 1L]
    if (length(tied)) {
      t_sizes <- as.numeric(tied)
      Tj_total <- Tj_total + sum(t_sizes^3 - t_sizes)
    }
  }

  denom <- m^2 * (n^3 - n) - m * Tj_total
  W <- if (denom == 0) NA_real_ else 12 * S / denom

  # Friedman chi-square (Kendall & Smith 1939, Eq. 5)
  chi_sq <- m * (n - 1) * W
  df     <- n - 1L
  p_val  <- if (is.na(W)) NA_real_ else stats::pchisq(chi_sq, df, lower.tail = FALSE)

  list(
    method      = "kendall_w",
    value       = W,
    ci_lower    = NA_real_,
    ci_upper    = NA_real_,
    per_value   = NULL,
    n_observers = as.integer(m),
    n_units     = as.integer(n),
    n_pairable  = as.integer(m * n),
    chi_squared = chi_sq,
    df          = df,
    p_value     = p_val,
    S           = S
  )
}
