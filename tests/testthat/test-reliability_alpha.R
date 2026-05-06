# Tests for reliability_alpha() — Krippendorff's alpha for predefined units.
#
# Worked examples are taken from Krippendorff (2019), Chapter 12, "Reliability".
# Each test reconstructs the canonical Observers-by-Units form (which we then
# pass as Units-by-Observers because that's reliability_alpha()'s input shape).

# §12.3.1 (pp. 295–304): 4 observers, 12 units, 5 nominal categories,
# missing data. The 12th unit has only one pairable value and is excluded.
make_kripp_canonical <- function() {
  # Encoded categories: 1 = book, 2 = mail, 3 = phone, 4 = computer, 5 = file
  obs_x_unit <- matrix(c(
    1,  1, NA, 1,    # unit 1
    2,  2,  3, 2,    # unit 2
    3,  3,  3, 3,    # unit 3
    3,  3,  3, 3,    # unit 4
    2,  2,  2, 2,    # unit 5
    1,  2,  3, 4,    # unit 6
    4,  4,  4, 4,    # unit 7
    1,  1,  2, 1,    # unit 8
    2,  2,  2, 2,    # unit 9
    NA, 5,  5, 5,    # unit 10
    NA, NA, 1, 1,    # unit 11
    NA, 3, NA, NA    # unit 12 (m_u = 1, excluded)
  ), nrow = 4)
  # Return as units × observers (subjects × raters)
  t(obs_x_unit)
}

test_that("reliability_alpha matches Krippendorff §12.3.1 worked example (all metrics)", {
  ratings <- make_kripp_canonical()

  # Book p. 296, p. 304
  expected <- c(nominal = 0.743, ordinal = 0.815,
                interval = 0.849, ratio = 0.797)

  for (m in names(expected)) {
    r <- reliability_alpha(ratings, method = m)
    expect_equal(round(r$value, 3), expected[[m]],
                 label = paste("alpha_", m, sep = ""))
  }

  # Diagnostic counts (book p. 295: n.. = 40, N = 11, m = 4)
  r <- reliability_alpha(ratings, method = "nominal")
  expect_equal(r$n_observers, 4L)
  expect_equal(r$n_units, 11L)
  expect_equal(r$n_pairable, 40L)

  # Coincidence matrix matches book p. 298 (Observed coincidences)
  expect_equal(unname(r$coincidence["1", "1"]), 7,    tolerance = 1e-9)
  expect_equal(unname(r$coincidence["2", "2"]), 10,   tolerance = 1e-9)
  expect_equal(unname(r$coincidence["3", "3"]), 8,    tolerance = 1e-9)
  expect_equal(unname(r$coincidence["4", "4"]), 4,    tolerance = 1e-9)
  expect_equal(unname(r$coincidence["5", "5"]), 3,    tolerance = 1e-9)
  expect_equal(unname(r$coincidence["1", "2"]), 4 / 3, tolerance = 1e-9)
})

test_that("reliability_alpha matches Krippendorff §12.3.4.1 (binary, 3 observers)", {
  # Book p. 306–307: Jon, Han, Lee on 12 newspaper issues. Expected α = 0.234.
  obs_x_unit <- matrix(c(
    0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1,   # Jon
    0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1,   # Han
    0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0    # Lee
  ), nrow = 3, byrow = TRUE)

  r <- reliability_alpha(t(obs_x_unit), method = "nominal")
  expect_equal(round(r$value, 3), 0.234)
  expect_equal(r$n_observers, 3L)
  expect_equal(r$n_units, 12L)
  expect_equal(r$n_pairable, 36L)

  # Binary data: per-category alpha for "0" equals alpha for "1".
  expect_equal(r$per_value$alpha[r$per_value$value == "0"],
               r$per_value$alpha[r$per_value$value == "1"])
})

test_that("reliability_alpha matches Krippendorff §12.3.4.4 (Mary & Dave)", {
  # Book p. 310: 2 observers, 12 units, 4 nominal categories.
  # Book reports α_nominal = 0.760 (rounded).
  ratings <- cbind(
    Mary = c("a", "a", "c", "c", "c", "c", "c", "b", "b", "b", "b", "d"),
    Dave = c("a", "c", "c", "c", "c", "c", "b", "b", "b", "b", "b", "d")
  )
  ratings_int <- apply(ratings, 2L, function(col) {
    as.integer(factor(col, levels = c("a", "b", "c", "d")))
  })

  r <- reliability_alpha(ratings_int, method = "nominal")
  expect_equal(round(r$value, 2), 0.76)
  expect_equal(r$n_observers, 2L)
  expect_equal(r$n_units, 12L)
  expect_equal(r$n_pairable, 24L)

  # Marginal counts from book p. 311: a=3, b=9, c=10, d=2
  expect_equal(setNames(r$per_value$n, r$per_value$value),
               c("1" = 3L, "2" = 9L, "3" = 10L, "4" = 2L))
})

test_that("reliability_alpha returns 1 when all observers fully agree", {
  ratings <- cbind(c(1, 2, 3), c(1, 2, 3))
  r <- reliability_alpha(ratings, method = "nominal")
  expect_equal(r$value, 1.0)
})

test_that("reliability_alpha returns NULL per_value for non-nominal metrics", {
  ratings <- make_kripp_canonical()
  for (m in c("ordinal", "interval", "ratio")) {
    r <- reliability_alpha(ratings, method = m)
    expect_null(r$per_value, info = paste("method =", m))
  }
})

test_that("reliability_alpha errors with fewer than two observers", {
  ratings <- matrix(c(1, 2, 3), ncol = 1L)
  expect_error(reliability_alpha(ratings, method = "nominal"),
               "two observers")
})

test_that("reliability_alpha output shape is uniform across metrics", {
  ratings <- make_kripp_canonical()
  expected_names <- c("method", "value", "ci_lower", "ci_upper", "per_value",
                      "n_observers", "n_units", "n_pairable", "coincidence")
  for (m in c("nominal", "ordinal", "interval", "ratio")) {
    r <- reliability_alpha(ratings, method = m)
    expect_named(r, expected_names, info = paste("method =", m))
    expect_true(is.na(r$ci_lower))
    expect_true(is.na(r$ci_upper))
  }
})
