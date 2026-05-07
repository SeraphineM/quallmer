#' Intraclass correlation coefficient
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of the intraclass correlation coefficient (ICC)
#' family for a `subjects x raters` matrix of interval/ratio ratings.
#' Six forms are exposed via `model`/`type`/`unit`, following the
#' Shrout-Fleiss naming and the McGraw-Wong calculation tables:
#'
#' | `model`  | `type`        | `unit`    | Shrout & Fleiss | McGraw & Wong |
#' |----------|---------------|-----------|-----------------|---------------|
#' | oneway   | (n/a)         | single    | ICC(1,1)        | ICC(1)        |
#' | oneway   | (n/a)         | average   | ICC(1,k)        | ICC(k)        |
#' | twoway   | consistency   | single    | ICC(3,1)        | ICC(C,1)      |
#' | twoway   | consistency   | average   | ICC(3,k)        | ICC(C,k)      |
#' | twoway   | agreement     | single    | ICC(2,1)        | ICC(A,1)      |
#' | twoway   | agreement     | average   | ICC(2,k)        | ICC(A,k)      |
#'
#' For `model = "oneway"` the `type` argument is ignored (only one form
#' exists). The two-way random and two-way mixed models share the same
#' calculations; they differ only in interpretation (whether the column
#' factor levels are treated as a random sample or as fixed). See Koo &
#' Li (2016) for guidance on selecting a form.
#'
#' @param ratings A `subjects x raters` matrix or data.frame of numeric
#'   ratings. Rows are objects of measurement (subjects); columns are
#'   raters. Must not contain `NA`.
#' @param model `"oneway"` (each subject rated by a different random set
#'   of raters) or `"twoway"` (the same `k` raters rate every subject).
#' @param type `"consistency"` (column variance excluded -- relative
#'   agreement) or `"agreement"` (column variance included -- absolute
#'   agreement). Ignored for `model = "oneway"`.
#' @param unit `"single"` (reliability of one rater's score) or
#'   `"average"` (reliability of the mean across `k` raters; the
#'   Spearman-Brown stepped-up form).
#' @param r0 Null-hypothesis value for the F-test. Default 0 tests
#'   `H0: ICC = 0`.
#' @param conf.level Confidence level for the CI on the population ICC
#'   (default 0.95).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`method`}{Short label, e.g. `"icc_2_1"` or `"icc_3_k"`.}
#'     \item{`value`}{The ICC estimate.}
#'     \item{`ci_lower`, `ci_upper`}{Confidence interval bounds at
#'       `conf.level`.}
#'     \item{`per_value`}{`NULL` (ICC has no per-category breakdown).}
#'     \item{`n_observers`, `n_units`, `n_pairable`}{Counts (k, n, k*n).}
#'     \item{`model`, `type`, `unit`}{The configuration that produced the
#'       ICC.}
#'     \item{`icc_name`}{Canonical Shrout-Fleiss name, e.g. `"ICC(2,1)"`.}
#'     \item{`F_value`, `df1`, `df2`, `p_value`, `r0`}{F-test of
#'       `H0: ICC = r0`.}
#'   }
#'
#' @references
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses
#' in assessing rater reliability. *Psychological Bulletin*, 86(2),
#' 420-428. \doi{10.1037/0033-2909.86.2.420}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. *Psychological Methods*, 1(1),
#' 30-46. \doi{10.1037/1082-989X.1.1.30}
#'
#' Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
#' intraclass correlation coefficients for reliability research.
#' *Journal of Chiropractic Medicine*, 15(2), 155-163.
#' \doi{10.1016/j.jcm.2016.02.012}
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

  if (is.data.frame(ratings)) ratings <- as.matrix(ratings)
  if (!is.matrix(ratings) || !is.numeric(ratings)) {
    cli::cli_abort("{.arg ratings} must be a numeric matrix or data.frame.")
  }
  if (anyNA(ratings)) {
    cli::cli_abort(c(
      "{.arg ratings} must not contain {.val NA}.",
      "i" = "Filter to complete cases first (see {.fn stats::complete.cases})."
    ))
  }
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  if (ns < 2L) cli::cli_abort("ICC requires at least 2 subjects (rows).")
  if (nr < 2L) cli::cli_abort("ICC requires at least 2 raters (columns).")

  alpha <- 1 - conf.level

  # ANOVA mean squares (one observation per cell randomized-block design).
  SStotal <- stats::var(as.numeric(ratings)) * (ns * nr - 1)
  MSr     <- stats::var(rowMeans(ratings)) * nr
  MSw     <- mean(apply(ratings, 1L, stats::var))
  MSc     <- stats::var(colMeans(ratings)) * ns
  MSe     <- (SStotal - MSr * (ns - 1) - MSc * (nr - 1)) / ((ns - 1) * (nr - 1))

  # Forms below mirror the formulas in McGraw & Wong (1996), Tables 4-7,
  # cross-walked to Shrout & Fleiss (1979) names. F-tests follow Table 8.

  if (model == "oneway") {
    if (unit == "single") {
      icc_name <- "ICC(1,1)";  method_label <- "icc_1_1"
      coeff   <- (MSr - MSw) / (MSr + (nr - 1) * MSw)
      Fvalue  <- MSr / MSw * ((1 - r0) / (1 + (nr - 1) * r0))
      df1     <- ns - 1
      df2     <- ns * (nr - 1)
      FL      <- (MSr / MSw) / stats::qf(1 - alpha / 2, ns - 1, ns * (nr - 1))
      FU      <- (MSr / MSw) * stats::qf(1 - alpha / 2, ns * (nr - 1), ns - 1)
      ci_lower <- (FL - 1) / (FL + (nr - 1))
      ci_upper <- (FU - 1) / (FU + (nr - 1))
    } else {
      icc_name <- sprintf("ICC(1,%d)", nr);  method_label <- "icc_1_k"
      coeff   <- (MSr - MSw) / MSr
      Fvalue  <- MSr / MSw * (1 - r0)
      df1     <- ns - 1
      df2     <- ns * (nr - 1)
      FL      <- (MSr / MSw) / stats::qf(1 - alpha / 2, ns - 1, ns * (nr - 1))
      FU      <- (MSr / MSw) * stats::qf(1 - alpha / 2, ns * (nr - 1), ns - 1)
      ci_lower <- 1 - 1 / FL
      ci_upper <- 1 - 1 / FU
    }
  } else if (type == "consistency") {
    if (unit == "single") {
      icc_name <- "ICC(3,1)";  method_label <- "icc_3_1"
      coeff   <- (MSr - MSe) / (MSr + (nr - 1) * MSe)
      Fvalue  <- MSr / MSe * ((1 - r0) / (1 + (nr - 1) * r0))
      df1     <- ns - 1
      df2     <- (ns - 1) * (nr - 1)
      FL      <- (MSr / MSe) / stats::qf(1 - alpha / 2, ns - 1, (ns - 1) * (nr - 1))
      FU      <- (MSr / MSe) * stats::qf(1 - alpha / 2, (ns - 1) * (nr - 1), ns - 1)
      ci_lower <- (FL - 1) / (FL + (nr - 1))
      ci_upper <- (FU - 1) / (FU + (nr - 1))
    } else {
      icc_name <- sprintf("ICC(3,%d)", nr);  method_label <- "icc_3_k"
      coeff   <- (MSr - MSe) / MSr
      Fvalue  <- MSr / MSe * (1 - r0)
      df1     <- ns - 1
      df2     <- (ns - 1) * (nr - 1)
      FL      <- (MSr / MSe) / stats::qf(1 - alpha / 2, ns - 1, (ns - 1) * (nr - 1))
      FU      <- (MSr / MSe) * stats::qf(1 - alpha / 2, (ns - 1) * (nr - 1), ns - 1)
      ci_lower <- 1 - 1 / FL
      ci_upper <- 1 - 1 / FU
    }
  } else {
    # type == "agreement", model == "twoway"
    if (unit == "single") {
      icc_name <- "ICC(2,1)";  method_label <- "icc_2_1"
      coeff   <- (MSr - MSe) / (MSr + (nr - 1) * MSe + (nr / ns) * (MSc - MSe))
      a <- (nr * r0) / (ns * (1 - r0))
      b <- 1 + (nr * r0 * (ns - 1)) / (ns * (1 - r0))
      Fvalue  <- MSr / (a * MSc + b * MSe)
      a <- (nr * coeff) / (ns * (1 - coeff))
      b <- 1 + (nr * coeff * (ns - 1)) / (ns * (1 - coeff))
      v <- (a * MSc + b * MSe)^2 /
        ((a * MSc)^2 / (nr - 1) + (b * MSe)^2 / ((ns - 1) * (nr - 1)))
      df1 <- ns - 1
      df2 <- v
      FL <- stats::qf(1 - alpha / 2, ns - 1, v)
      FU <- stats::qf(1 - alpha / 2, v, ns - 1)
      ci_lower <- (ns * (MSr - FL * MSe)) /
        (FL * (nr * MSc + (nr * ns - nr - ns) * MSe) + ns * MSr)
      ci_upper <- (ns * (FU * MSr - MSe)) /
        (nr * MSc + (nr * ns - nr - ns) * MSe + ns * FU * MSr)
    } else {
      icc_name <- sprintf("ICC(2,%d)", nr);  method_label <- "icc_2_k"
      coeff   <- (MSr - MSe) / (MSr + (MSc - MSe) / ns)
      a <- r0 / (ns * (1 - r0))
      b <- 1 + (r0 * (ns - 1)) / (ns * (1 - r0))
      Fvalue  <- MSr / (a * MSc + b * MSe)
      a <- (nr * coeff) / (ns * (1 - coeff))
      b <- 1 + (nr * coeff * (ns - 1)) / (ns * (1 - coeff))
      v <- (a * MSc + b * MSe)^2 /
        ((a * MSc)^2 / (nr - 1) + (b * MSe)^2 / ((ns - 1) * (nr - 1)))
      df1 <- ns - 1
      df2 <- v
      FL <- stats::qf(1 - alpha / 2, ns - 1, v)
      FU <- stats::qf(1 - alpha / 2, v, ns - 1)
      ci_lower <- (ns * (MSr - FL * MSe)) / (FL * (MSc - MSe) + ns * MSr)
      ci_upper <- (ns * (FU * MSr - MSe)) / (MSc - MSe + ns * FU * MSr)
    }
  }

  p_value <- stats::pf(Fvalue, df1, df2, lower.tail = FALSE)

  list(
    method      = method_label,
    value       = unname(coeff),
    ci_lower    = unname(ci_lower),
    ci_upper    = unname(ci_upper),
    per_value   = NULL,
    n_observers = as.integer(nr),
    n_units     = as.integer(ns),
    n_pairable  = as.integer(ns * nr),
    model       = model,
    type        = type,
    unit        = unit,
    icc_name    = icc_name,
    F_value     = unname(Fvalue),
    df1         = df1,
    df2         = df2,
    p_value     = unname(p_value),
    r0          = r0
  )
}
