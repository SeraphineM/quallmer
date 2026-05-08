# Tests for metric_precision(), metric_recall(), metric_f_meas().
#
# Hand-worked binary case (Sokolova & Lapalme 2009, Manning et al. 2008):
#   truth    = A, A, A, A, A, B
#   estimate = A, A, A, B, B, B
# For class A: TP = 3, FP = 0, FN = 2, n_truth = 5
# For class B: TP = 1, FP = 2, FN = 0, n_truth = 1
# Per-class precision: A = 3/3 = 1.000, B = 1/3 ~= 0.333
# Per-class recall:    A = 3/5 = 0.600, B = 1/1 = 1.000
# Per-class F1:        A = 2*1*0.6/(1+0.6) = 0.750, B = 2*0.333*1/(0.333+1) = 0.500
# Macro precision:     (1 + 0.333)/2 = 0.667
# Macro recall:        (0.6 + 1)/2   = 0.800
# Macro F1:            (0.75 + 0.5)/2 = 0.625
# Macro-weighted precision: (5*1 + 1*0.333)/6 = 0.889
# Macro-weighted recall:    (5*0.6 + 1*1)/6   = 0.667
# Micro: pooled TP=4, FP=2, FN=2 -> precision = recall = F1 = 4/6 = 0.667

setup_binary <- function() {
  list(
    truth    = factor(c("A","A","A","A","A","B"), levels = c("A","B")),
    estimate = factor(c("A","A","A","B","B","B"), levels = c("A","B"))
  )
}

test_that("metric_precision matches hand calculations on the binary example", {
  d <- setup_binary()
  expect_equal(metric_precision(d$truth, d$estimate, "binary"),         1.0)
  expect_equal(metric_precision(d$truth, d$estimate, "macro"),          (1 + 1/3) / 2)
  expect_equal(metric_precision(d$truth, d$estimate, "macro_weighted"), (5*1 + 1*(1/3)) / 6)
  expect_equal(metric_precision(d$truth, d$estimate, "micro"),          4/6)
})

test_that("metric_recall matches hand calculations on the binary example", {
  d <- setup_binary()
  expect_equal(metric_recall(d$truth, d$estimate, "binary"),         0.6)
  expect_equal(metric_recall(d$truth, d$estimate, "macro"),          (0.6 + 1) / 2)
  expect_equal(metric_recall(d$truth, d$estimate, "macro_weighted"), (5*0.6 + 1*1) / 6)
  expect_equal(metric_recall(d$truth, d$estimate, "micro"),          4/6)
})

test_that("metric_f_meas matches hand calculations on the binary example", {
  d <- setup_binary()
  # Per-class F1: A = 0.75, B = 2*(1/3)*1 / (1/3 + 1) = 0.5
  expect_equal(metric_f_meas(d$truth, d$estimate, "binary"),         0.75)
  expect_equal(metric_f_meas(d$truth, d$estimate, "macro"),          (0.75 + 0.5) / 2)
  expect_equal(metric_f_meas(d$truth, d$estimate, "macro_weighted"), (5*0.75 + 1*0.5) / 6)
  expect_equal(metric_f_meas(d$truth, d$estimate, "micro"),          4/6)
})

test_that("F-beta with beta = 2 weights recall more than precision", {
  d <- setup_binary()
  # For class A: precision = 1, recall = 0.6
  # F2 = (1+4) * 1 * 0.6 / (4*1 + 0.6) = 3 / 4.6 = 0.652...
  expect_equal(metric_f_meas(d$truth, d$estimate, "binary", beta = 2),
               5 * 1 * 0.6 / (4 * 1 + 0.6))
})

test_that("metric_* match yardstick on a multi-class noisy example", {
  skip_if_not_installed("yardstick")
  set.seed(42)
  classes <- c("A","B","C","D")
  truth <- factor(sample(classes, 200, replace = TRUE,
                         prob = c(0.4, 0.3, 0.2, 0.1)))
  est <- truth
  est[sample(200, 80)] <- factor(sample(classes, 80, replace = TRUE),
                                  levels = classes)
  df <- data.frame(truth = truth, estimate = est)

  for (e in c("macro", "macro_weighted", "micro")) {
    expect_equal(
      metric_precision(truth, est, estimator = e),
      yardstick::precision(df, truth, estimate, estimator = e)$.estimate,
      info = paste("precision /", e)
    )
    expect_equal(
      metric_recall(truth, est, estimator = e),
      yardstick::recall(df, truth, estimate, estimator = e)$.estimate,
      info = paste("recall /", e)
    )
    expect_equal(
      metric_f_meas(truth, est, estimator = e),
      yardstick::f_meas(df, truth, estimate, estimator = e)$.estimate,
      info = paste("f_meas /", e)
    )
  }
})

test_that("metric_* return 1 for perfect agreement", {
  truth <- factor(c("A","B","C","A","B","C"))
  est   <- truth
  for (e in c("macro", "macro_weighted", "micro")) {
    expect_equal(metric_precision(truth, est, e), 1.0)
    expect_equal(metric_recall(truth, est, e),    1.0)
    expect_equal(metric_f_meas(truth, est, e),    1.0)
  }
})

test_that("metric_* return NaN for class never predicted (precision)", {
  # Truth has B but estimate never predicts B.
  truth <- factor(c("A","A","B","B"), levels = c("A","B"))
  est   <- factor(c("A","A","A","A"), levels = c("A","B"))
  # For class B: TP = 0, FP = 0 -> precision NaN. Macro should propagate.
  expect_true(is.nan(metric_precision(truth, est, "macro")))
  # Recall for B is well-defined (TP=0, FN=2 -> 0).
  expect_equal(metric_recall(truth, est, "macro"), (1 + 0) / 2)
})

test_that("metric_* errors on binary estimator with != 2 classes", {
  truth <- factor(c("A","B","C"))
  est   <- factor(c("A","B","C"))
  expect_error(metric_precision(truth, est, "binary"), "exactly 2 classes")
  expect_error(metric_recall(truth, est, "binary"),    "exactly 2 classes")
  expect_error(metric_f_meas(truth, est, "binary"),    "exactly 2 classes")
})

test_that("confusion_components matches base R table layout", {
  truth <- factor(c("A","A","B","B","C"), levels = c("A","B","C"))
  est   <- factor(c("A","B","B","C","C"), levels = c("A","B","C"))
  cc <- confusion_components(truth, est)
  # cm[truth=A, estimate=A] = 1, [A,B]=1, [B,B]=1, [B,C]=1, [C,C]=1
  expect_equal(cc$class,   c("A","B","C"))
  expect_equal(cc$TP,      c(1L, 1L, 1L))
  expect_equal(cc$FP,      c(0L, 1L, 1L))
  expect_equal(cc$FN,      c(1L, 1L, 0L))
  expect_equal(cc$n_truth, c(2L, 2L, 1L))
})
