# Native classification metrics: precision, recall, F-measure.
#
# Three small internal functions that mirror `yardstick::precision()`,
# `yardstick::recall()`, and `yardstick::f_meas()` but avoid the
# tidyverse dependency stack. All four standard estimators are
# supported: "binary", "macro", "macro_weighted", "micro".
#
# Per-class confusion matrix counts and micro-/macro-averaged
# precision and recall follow the formulas in Sokolova & Lapalme
# (2009), Tables 1-3. The macro F-score implemented here is the
# *arithmetic mean of per-class F-scores* (Manning, Raghavan &
# Schutze, 2008, ch. 13), which is the convention used by yardstick
# and scikit-learn -- this differs from Sokolova & Lapalme's Table 3
# definition (which computes F-score from macro precision and macro
# recall); the two coincide only when per-class precision and recall
# are equal across classes.


#' Per-class confusion-matrix components
#'
#' Internal helper. Returns a data.frame with one row per class
#' (the union of levels in `truth` and `estimate`), giving TP, FP, FN,
#' and the truth-side count `n_truth` for that class.
#'
#' Layout convention: `table(truth, estimate)` -- rows = truth, columns
#' = estimate. Then for class c:
#' * TP_c = diagonal entry
#' * FP_c = column sum minus diagonal (predicted as c but truth is not)
#' * FN_c = row sum minus diagonal (truth is c but predicted differently)
#'
#' @keywords internal
confusion_components <- function(truth, estimate) {
  all_levels <- union(levels(as.factor(truth)), levels(as.factor(estimate)))
  truth    <- factor(truth,    levels = all_levels)
  estimate <- factor(estimate, levels = all_levels)
  cm <- table(truth = truth, estimate = estimate)
  data.frame(
    class   = all_levels,
    TP      = as.integer(diag(cm)),
    FP      = as.integer(colSums(cm) - diag(cm)),
    FN      = as.integer(rowSums(cm) - diag(cm)),
    n_truth = as.integer(rowSums(cm)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


# Safe ratio: returns NaN when denom == 0 (matching yardstick's default
# of producing NaN for undefined per-class metrics).
.safe_ratio <- function(num, denom) {
  ifelse(denom == 0, NaN, num / denom)
}


# Aggregate per-class scores into a single value under the chosen
# estimator. Used by all three metric functions below.
.aggregate_class_scores <- function(per_class, n_truth, estimator) {
  if (estimator == "macro") {
    return(mean(per_class))
  }
  if (estimator == "macro_weighted") {
    # Drop classes with zero truth representation: their weight is 0
    # but per-class score may be NaN, and NaN * 0 = NaN under naive
    # weighted.mean. yardstick's behaviour is to skip them.
    keep <- n_truth > 0
    if (!any(keep)) return(NaN)
    return(stats::weighted.mean(per_class[keep], n_truth[keep]))
  }
  cli::cli_abort("Unknown estimator: {.val {estimator}}.")
}


#' Precision
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of multi-class precision matching the four
#' yardstick estimators (`"binary"`, `"macro"`, `"macro_weighted"`,
#' `"micro"`). Per-class precision is `TP / (TP + FP)`; macro and
#' micro aggregation follow Sokolova & Lapalme (2009), Table 3 (the
#' arithmetic mean and the pooled-counts forms respectively).
#' Macro-weighted is the truth-prevalence-weighted mean of per-class
#' precisions. Returns `NaN` when the denominator is zero (no
#' instances predicted for that class), matching yardstick's default.
#'
#' @param truth Factor (or coercible) of true class labels.
#' @param estimate Factor (or coercible) of predicted class labels.
#'   Must take values from the same level set as `truth`.
#' @param estimator One of `"binary"` (exactly two classes; uses
#'   `event_level`), `"macro"` (unweighted mean of per-class
#'   precisions), `"macro_weighted"` (mean weighted by truth-class
#'   prevalence), or `"micro"` (pooled TP and FP across all classes;
#'   for single-label multi-class data this equals accuracy).
#' @param event_level For `estimator = "binary"`: which level is the
#'   positive event, `"first"` (default) or `"second"`.
#'
#' @return A single numeric value.
#'
#' @references
#' Sokolova, M., & Lapalme, G. (2009). A systematic analysis of
#' performance measures for classification tasks. *Information
#' Processing & Management*, 45(4), 427-437.
#' \doi{10.1016/j.ipm.2009.03.002}
#'
#' Manning, C. D., Raghavan, P., & Schutze, H. (2008). *Introduction
#' to Information Retrieval*, Chapter 13. Cambridge University Press.
#' (Free online: <https://nlp.stanford.edu/IR-book/>)
#'
#' @keywords internal
metric_precision <- function(truth, estimate,
                              estimator = c("binary", "macro",
                                            "macro_weighted", "micro"),
                              event_level = c("first", "second")) {
  estimator <- match.arg(estimator)
  event_level <- match.arg(event_level)

  cc <- confusion_components(truth, estimate)

  if (estimator == "binary") {
    if (nrow(cc) != 2L) {
      cli::cli_abort(c(
        "{.code estimator = \"binary\"} requires exactly 2 classes.",
        "x" = "Got {nrow(cc)} class{?es}."
      ))
    }
    idx <- if (event_level == "first") 1L else 2L
    return(.safe_ratio(cc$TP[idx], cc$TP[idx] + cc$FP[idx]))
  }

  if (estimator == "micro") {
    return(.safe_ratio(sum(cc$TP), sum(cc$TP) + sum(cc$FP)))
  }

  per_class <- .safe_ratio(cc$TP, cc$TP + cc$FP)
  .aggregate_class_scores(per_class, cc$n_truth, estimator)
}


#' Recall
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of multi-class recall (a.k.a. sensitivity).
#' Per-class recall is `TP / (TP + FN)`; the four estimators behave as
#' for [metric_precision()].
#'
#' @inheritParams metric_precision
#'
#' @return A single numeric value.
#'
#' @inherit metric_precision references
#'
#' @keywords internal
metric_recall <- function(truth, estimate,
                           estimator = c("binary", "macro",
                                         "macro_weighted", "micro"),
                           event_level = c("first", "second")) {
  estimator <- match.arg(estimator)
  event_level <- match.arg(event_level)

  cc <- confusion_components(truth, estimate)

  if (estimator == "binary") {
    if (nrow(cc) != 2L) {
      cli::cli_abort(c(
        "{.code estimator = \"binary\"} requires exactly 2 classes.",
        "x" = "Got {nrow(cc)} class{?es}."
      ))
    }
    idx <- if (event_level == "first") 1L else 2L
    return(.safe_ratio(cc$TP[idx], cc$TP[idx] + cc$FN[idx]))
  }

  if (estimator == "micro") {
    return(.safe_ratio(sum(cc$TP), sum(cc$TP) + sum(cc$FN)))
  }

  per_class <- .safe_ratio(cc$TP, cc$TP + cc$FN)
  .aggregate_class_scores(per_class, cc$n_truth, estimator)
}


#' F-measure (F-beta)
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of the F-beta score (default beta = 1, the
#' harmonic mean of precision and recall). Macro and macro-weighted
#' forms compute the (possibly weighted) arithmetic mean of per-class
#' F-beta scores -- the convention used by yardstick and scikit-learn
#' (Manning et al. 2008, ch. 13). This differs from Sokolova &
#' Lapalme (2009, Table 3) where macro F-score is computed from the
#' macro-averaged precision and recall directly; the two coincide
#' only when per-class precision and recall are equal across classes.
#' Micro pools TP, FP, and FN globally before computing F-beta.
#'
#' @inheritParams metric_precision
#' @param beta Positive numeric. `beta = 1` (default) gives the
#'   familiar F1; `beta < 1` weights precision more, `beta > 1`
#'   weights recall more.
#'
#' @return A single numeric value.
#'
#' @inherit metric_precision references
#'
#' @keywords internal
metric_f_meas <- function(truth, estimate,
                           estimator = c("binary", "macro",
                                         "macro_weighted", "micro"),
                           event_level = c("first", "second"),
                           beta = 1) {
  estimator <- match.arg(estimator)
  event_level <- match.arg(event_level)
  if (!is.numeric(beta) || length(beta) != 1L || beta <= 0) {
    cli::cli_abort("{.arg beta} must be a single positive number.")
  }

  cc <- confusion_components(truth, estimate)
  b2 <- beta^2

  # F-beta in TP/FP/FN form:
  #   F_beta = (1 + beta^2) * TP / ((1 + beta^2)*TP + beta^2*FN + FP)
  fbeta <- function(TP, FP, FN) {
    num   <- (1 + b2) * TP
    denom <- num + b2 * FN + FP
    .safe_ratio(num, denom)
  }

  if (estimator == "binary") {
    if (nrow(cc) != 2L) {
      cli::cli_abort(c(
        "{.code estimator = \"binary\"} requires exactly 2 classes.",
        "x" = "Got {nrow(cc)} class{?es}."
      ))
    }
    idx <- if (event_level == "first") 1L else 2L
    return(fbeta(cc$TP[idx], cc$FP[idx], cc$FN[idx]))
  }

  if (estimator == "micro") {
    return(fbeta(sum(cc$TP), sum(cc$FP), sum(cc$FN)))
  }

  per_class <- fbeta(cc$TP, cc$FP, cc$FN)
  .aggregate_class_scores(per_class, cc$n_truth, estimator)
}
