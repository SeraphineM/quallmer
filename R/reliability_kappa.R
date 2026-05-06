#' Cohen's kappa for two raters
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of Cohen's `kappa` for nominal-scale agreement
#' between two raters (Cohen, 1960). Unweighted (Eq. 1) and weighted
#' (linear or quadratic) variants are supported.
#'
#' @param observations A `subjects × 2 raters` matrix or data.frame.
#'   Rows are units; the two columns are the two raters. Must not
#'   contain `NA`.
#' @param weight Weighting scheme for disagreements:
#'   \describe{
#'     \item{`"unweighted"`}{All disagreements equally serious (default).}
#'     \item{`"equal"`}{Linear weights: `1 - |i - j|/(k - 1)`.}
#'     \item{`"squared"`}{Quadratic weights: `1 - ((i - j)/(k - 1))^2`.}
#'   }
#'
#' @return A list with elements `method`, `value`, `ci_lower`, `ci_upper`,
#'   `per_value`, `n_observers`, `n_units`, `n_pairable`. `ci_lower` and
#'   `ci_upper` are populated for unweighted kappa using the asymptotic
#'   standard error from Cohen (1960, Eq. 7); `NA` for weighted variants.
#'   `per_value` (unweighted only) gives per-category κ via dichotomisation.
#'
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales.
#' *Educational and Psychological Measurement*, 20(1), 37–46.
#' \doi{10.1177/001316446002000104}
#'
#' @keywords internal
reliability_kappa <- function(observations,
                              weight = c("unweighted", "equal", "squared")) {
  weight <- match.arg(weight)

  if (is.data.frame(observations)) observations <- as.matrix(observations)
  if (!is.matrix(observations)) {
    cli::cli_abort("{.arg observations} must be a matrix or data.frame.")
  }
  if (ncol(observations) != 2L) {
    cli::cli_abort(c(
      "Cohen's kappa requires exactly 2 raters (columns).",
      "x" = "Got {ncol(observations)} column{?s}.",
      "i" = "For 3 or more raters, use {.fn reliability_kappa_fleiss}."
    ))
  }
  if (anyNA(observations)) {
    cli::cli_abort(c(
      "{.arg observations} must not contain {.val NA}.",
      "i" = "Filter to complete cases first (see {.fn stats::complete.cases})."
    ))
  }

  values <- sort(unique(as.vector(observations)))
  k <- length(values)
  N <- nrow(observations)

  # Contingency table with explicit levels in both directions
  ra <- factor(observations[, 1L], levels = values)
  rb <- factor(observations[, 2L], levels = values)
  ct <- unname(as.matrix(table(ra, rb)))

  p_a <- rowSums(ct) / N        # rater 1 marginal
  p_b <- colSums(ct) / N        # rater 2 marginal

  w <- kappa_weights(k, weight)

  p_o <- sum(w * ct) / N
  p_e <- sum(w * outer(p_a, p_b))

  kappa_value <- alpha_from_disagreements(1 - p_o, 1 - p_e)

  if (weight == "unweighted") {
    # Cohen (1960, Eq. 7): σ_κ ≈ sqrt(p_o(1-p_o) / (N(1-p_e)^2))
    se <- if (1 - p_e == 0) NA_real_ else {
      sqrt(p_o * (1 - p_o) / (N * (1 - p_e)^2))
    }
    ci_lower <- kappa_value - 1.96 * se
    ci_upper <- kappa_value + 1.96 * se
    per_value <- per_category_kappa_cohen(observations, values)
    method_label <- "kappa_cohen"
  } else {
    se        <- NA_real_
    ci_lower  <- NA_real_
    ci_upper  <- NA_real_
    per_value <- NULL
    method_label <- paste0("kappa_cohen_", weight)
  }

  list(
    method      = method_label,
    value       = kappa_value,
    ci_lower    = ci_lower,
    ci_upper    = ci_upper,
    per_value   = per_value,
    n_observers = 2L,
    n_units     = N,
    n_pairable  = 2L * N
  )
}


#' Fleiss' kappa for many raters
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of Fleiss' generalisation of κ to a constant
#' number of raters per subject (Fleiss, 1971), where the raters rating
#' one subject need not be the same as those rating another. For two
#' raters use [reliability_kappa()] (Cohen's): the two coefficients
#' differ even on the same data because Cohen's uses each rater's
#' marginals while Fleiss' uses pooled marginals.
#'
#' @param observations A `subjects × raters` matrix or data.frame.
#'   Rows are units; columns are raters. Must not contain `NA`. The
#'   number of raters per subject is taken to be `ncol(observations)`.
#'
#' @return A list with elements `method`, `value`, `ci_lower`, `ci_upper`,
#'   `per_value`, `n_observers`, `n_units`, `n_pairable`. CI bounds are
#'   from the asymptotic SE in Fleiss (1971, Eq. 16). `per_value` gives
#'   per-category κⱼ from Fleiss (1971, Eqs. 20–21).
#'
#' @references
#' Fleiss, J. L. (1971). Measuring nominal scale agreement among many
#' raters. *Psychological Bulletin*, 76(5), 378–382.
#' \doi{10.1037/h0031619}
#'
#' @keywords internal
reliability_kappa_fleiss <- function(observations) {
  if (is.data.frame(observations)) observations <- as.matrix(observations)
  if (!is.matrix(observations)) {
    cli::cli_abort("{.arg observations} must be a matrix or data.frame.")
  }
  if (ncol(observations) < 2L) {
    cli::cli_abort("Fleiss' kappa requires at least 2 raters (columns).")
  }
  if (anyNA(observations)) {
    cli::cli_abort(c(
      "{.arg observations} must not contain {.val NA}.",
      "i" = "Filter to complete cases first (see {.fn stats::complete.cases})."
    ))
  }

  N <- nrow(observations)
  n <- ncol(observations)
  values <- sort(unique(as.vector(observations)))
  k <- length(values)

  # n_ij: number of raters who placed subject i into category j
  nij <- matrix(0L, nrow = N, ncol = k,
                dimnames = list(NULL, as.character(values)))
  for (i in seq_len(N)) {
    nij[i, ] <- tabulate(match(observations[i, ], values), nbins = k)
  }

  # Per-subject agreement Pᵢ (Fleiss 1971, Eq. 2)
  Pi <- (rowSums(nij^2) - n) / (n * (n - 1))
  P_bar <- mean(Pi)

  # Pooled marginal proportions and chance agreement (Eqs. 1, 5)
  pj      <- colSums(nij) / (N * n)
  Pe_bar  <- sum(pj^2)

  kappa_value <- alpha_from_disagreements(1 - P_bar, 1 - Pe_bar)

  # Asymptotic variance of κ (Fleiss 1971, Eq. 16)
  if (1 - Pe_bar == 0) {
    se <- NA_real_
  } else {
    var_kappa <- (2 / (N * n * (n - 1))) *
      (Pe_bar - (2 * n - 3) * Pe_bar^2 + 2 * (n - 2) * sum(pj^3)) /
      (1 - Pe_bar)^2
    se <- if (var_kappa < 0) NA_real_ else sqrt(var_kappa)
  }

  per_value <- per_category_kappa_fleiss(nij, pj, N, n, values)

  list(
    method      = "kappa_fleiss",
    value       = kappa_value,
    ci_lower    = kappa_value - 1.96 * se,
    ci_upper    = kappa_value + 1.96 * se,
    per_value   = per_value,
    n_observers = as.integer(n),
    n_units     = as.integer(N),
    n_pairable  = as.integer(N * n)
  )
}


# Weight matrix for κ (k × k). Diagonal = 1; off-diagonal weight depends
# on |i - j| (linear "equal" or quadratic "squared").
kappa_weights <- function(k, weight) {
  if (weight == "unweighted" || k <= 1L) return(diag(k))
  i <- seq_len(k)
  d <- abs(outer(i, i, "-")) / (k - 1L)
  switch(weight,
    equal   = 1 - d,
    squared = 1 - d^2
  )
}


# Per-category Cohen's κ: dichotomise each category against all others.
per_category_kappa_cohen <- function(observations, values) {
  N <- nrow(observations)
  out <- data.frame(
    value = as.character(values),
    kappa = NA_real_,
    n     = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(values)) {
    cv <- values[i]
    bin1 <- observations[, 1L] == cv
    bin2 <- observations[, 2L] == cv
    out$n[i] <- as.integer(sum(bin1) + sum(bin2))

    n11 <- sum( bin1 &  bin2)
    n10 <- sum( bin1 & !bin2)
    n01 <- sum(!bin1 &  bin2)
    n00 <- sum(!bin1 & !bin2)

    p_o <- (n11 + n00) / N
    p_a1 <- (n11 + n10) / N
    p_b1 <- (n11 + n01) / N
    p_e <- p_a1 * p_b1 + (1 - p_a1) * (1 - p_b1)

    out$kappa[i] <- alpha_from_disagreements(1 - p_o, 1 - p_e)
  }

  out
}


# Per-category Fleiss' κⱼ (Fleiss 1971, Eqs. 20–21).
per_category_kappa_fleiss <- function(nij, pj, N, n, values) {
  k <- length(values)
  out <- data.frame(
    value = as.character(values),
    kappa = NA_real_,
    n     = as.integer(colSums(nij)),
    stringsAsFactors = FALSE
  )

  for (j in seq_len(k)) {
    pjj <- pj[j]
    qjj <- 1 - pjj
    if (pjj == 0 || qjj == 0) {
      out$kappa[j] <- if (pjj == 1) 1.0 else NA_real_
      next
    }
    Pj_bar <- (sum(nij[, j]^2) - N * n * pjj) / (N * n * (n - 1) * pjj)
    out$kappa[j] <- unname((Pj_bar - pjj) / qjj)
  }

  out
}
