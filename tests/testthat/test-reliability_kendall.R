# Tests for reliability_kendall_w() -- Kendall's W coefficient of concordance.
#
# Worked examples:
# - Kendall & Smith (1939, p. 276): 3 raters x 6 objects (no ties).
#   S = 25.5, W = 0.16.
# - Kendall & Gibbons (1990, Table 6.1): 4 raters x 6 objects (no ties).
#   S = 64, W = 0.229.
# - Kendall & Gibbons (1990, Example 6.1, Table 6.2): 3 raters x 10 objects
#   with multiple tied groups (X has 2 ties, Y has 3, Z has 4+3).
#   S = 591, W = 0.828.

test_that("reliability_kendall_w matches Kendall & Smith (1939) p. 276 example", {
  m <- cbind(
    Brown    = c(5, 4, 1, 6, 3, 2),
    Jones    = c(2, 3, 1, 5, 6, 4),
    Robinson = c(4, 1, 6, 3, 2, 5)
  )

  r <- reliability_kendall_w(m)

  # Book p. 276
  expect_equal(r$S,     25.5)
  expect_equal(round(r$value, 2), 0.16)
  expect_equal(r$n_observers, 3L)
  expect_equal(r$n_units, 6L)

  # Friedman chi-square = m(n-1)W
  expect_equal(r$chi_squared, 3 * 5 * r$value, tolerance = 1e-9)
  expect_equal(r$df, 5L)
})

test_that("reliability_kendall_w matches Kendall & Gibbons (1990) Table 6.1", {
  # 4 raters x 6 objects, no ties. Book: S = 64, W = 0.229.
  m <- cbind(
    W = c(5, 4, 1, 6, 3, 2),
    X = c(2, 3, 1, 5, 6, 4),
    Y = c(4, 1, 6, 3, 2, 5),
    Z = c(4, 3, 2, 5, 1, 6)
  )
  r <- reliability_kendall_w(m)
  expect_equal(r$S, 64)
  expect_equal(round(r$value, 3), 0.229)
  expect_equal(r$n_observers, 4L)
  expect_equal(r$n_units, 6L)
})

test_that("reliability_kendall_w handles multiple tied groups (K&G 1990, Ex. 6.1)", {
  # 3 raters x 10 objects with averaged ranks for ties:
  #   X: 1 tied pair at 4.5 and 1 at 7.5  -> sum (t^3 - t) = 12
  #   Y: 3 tied pairs (2.5, 4.5, 6.5)     -> sum = 18
  #   Z: 1 group of 4 at 4.5, 1 group of 3 at 8 -> sum = 60 + 24 = 84
  # Total tie correction = 114; book: S = 591, W = 0.828.
  m <- cbind(
    X = c(1.0, 4.5, 2.0, 4.5, 3.0, 7.5, 6.0, 9.0, 7.5, 10.0),
    Y = c(2.5, 1.0, 2.5, 4.5, 4.5, 8.0, 9.0, 6.5, 10.0, 6.5),
    Z = c(2.0, 1.0, 4.5, 4.5, 4.5, 4.5, 8.0, 8.0, 8.0, 10.0)
  )
  r <- reliability_kendall_w(m)
  expect_equal(r$S, 591)
  expect_equal(round(r$value, 3), 0.828)
  expect_equal(r$n_observers, 3L)
  expect_equal(r$n_units, 10L)
})

test_that("reliability_kendall_w returns 1 for perfect agreement", {
  m <- cbind(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
  r <- reliability_kendall_w(m)
  expect_equal(r$value, 1.0)
  expect_equal(r$chi_squared, 3 * 4)   # m(n-1)W
})

test_that("reliability_kendall_w invariant under monotone rescaling", {
  # Raw scores rank to the same order as their ranks; W must agree.
  raw <- cbind(c(10, 20, 5, 50, 30, 15),
               c(1.0, 2.5, 0.1, 9.9, 7.7, 1.5),
               c(40, 10, 70, 30, 20, 50))
  ranked <- apply(raw, 2L, rank, ties.method = "average")
  expect_equal(reliability_kendall_w(raw)$value,
               reliability_kendall_w(ranked)$value,
               tolerance = 1e-12)
})

test_that("reliability_kendall_w applies tie correction for tied ranks", {
  # Rater 2 has a tie at the top: ranks become 1.5, 1.5, 3, 4.
  m_ties <- cbind(c(1, 2, 3, 4),
                  c(1, 1, 3, 4))

  # Expected, by hand:
  #   ranks col 2 = (1.5, 1.5, 3, 4); R = (2.5, 3.5, 6, 8); mean = 5
  #   S = 6.25 + 2.25 + 1 + 9 = 18.5
  #   tie group of size 2 -> T = 2^3 - 2 = 6; sum_j T_j = 6
  #   denom = m^2(n^3 - n) - m * sum_T = 4 * 60 - 2 * 6 = 228
  #   W = 12 * 18.5 / 228 = 0.97368...
  r <- reliability_kendall_w(m_ties)
  expect_equal(r$S, 18.5)
  expect_equal(r$value, 12 * 18.5 / 228, tolerance = 1e-9)

  # Without ties, denominator would be 240; with correction it's smaller, so
  # corrected W exceeds the uncorrected version.
  expect_gt(r$value, 12 * 18.5 / 240)
})

test_that("reliability_kendall_w errors with too few raters or units", {
  expect_error(reliability_kendall_w(matrix(1:5, ncol = 1L)), "at least 2 raters")
  expect_error(reliability_kendall_w(matrix(1:6, nrow = 1L)), "at least 2 objects")
})

test_that("reliability_kendall_w errors on NA", {
  m <- cbind(c(1, 2, NA, 4), c(1, 2, 3, 4))
  expect_error(reliability_kendall_w(m), "NA")
})

test_that("reliability_kendall_w returns uniform output shape", {
  m <- cbind(c(1, 2, 3, 4, 5), c(2, 1, 4, 3, 5), c(1, 3, 2, 5, 4))
  r <- reliability_kendall_w(m)

  expected_names <- c("method", "value", "ci_lower", "ci_upper", "per_value",
                      "n_observers", "n_units", "n_pairable",
                      "chi_squared", "df", "p_value", "S")
  expect_named(r, expected_names)
  expect_equal(r$method, "kendall_w")
  expect_true(is.na(r$ci_lower))
  expect_null(r$per_value)
  expect_true(r$value >= 0 && r$value <= 1)
  expect_true(r$p_value >= 0 && r$p_value <= 1)
})

test_that("reliability_kendall_w p-value comes from chi-square distribution", {
  m <- cbind(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
  r <- reliability_kendall_w(m)
  # Perfect agreement: chi2 = m(n-1) = 12; p = pchisq(12, 4, lower=FALSE)
  expect_equal(r$p_value, stats::pchisq(12, df = 4, lower.tail = FALSE),
               tolerance = 1e-12)
})
