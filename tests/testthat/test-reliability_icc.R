# Tests for reliability_icc() -- intraclass correlation coefficient.
#
# Worked example: Shrout & Fleiss (1979), Tables 2-4. Six targets rated by
# four judges. ANOVA: BMS=11.24, WMS=6.26, JMS=32.49, EMS=1.02. The six
# Shrout-Fleiss ICC values from Table 4:
#   ICC(1,1)=0.17  ICC(2,1)=0.29  ICC(3,1)=0.71
#   ICC(1,4)=0.44  ICC(2,4)=0.62  ICC(3,4)=0.91

make_sf_table2 <- function() {
  rbind(
    c( 9, 2, 5, 8),
    c( 6, 1, 3, 2),
    c( 8, 4, 6, 8),
    c( 7, 1, 2, 6),
    c(10, 5, 6, 9),
    c( 6, 2, 4, 7)
  )
}

test_that("ANOVA mean squares match Shrout & Fleiss (1979) Table 3", {
  m <- make_sf_table2()
  ns <- nrow(m); nr <- ncol(m)
  SStotal <- stats::var(as.numeric(m)) * (ns * nr - 1)
  MSr     <- stats::var(rowMeans(m)) * nr
  MSw     <- mean(apply(m, 1L, stats::var))
  MSc     <- stats::var(colMeans(m)) * ns
  MSe     <- (SStotal - MSr * (ns - 1) - MSc * (nr - 1)) / ((ns - 1) * (nr - 1))

  # Table 3: BMS=11.24, WMS=6.26, JMS=32.49, EMS=1.02
  expect_equal(round(MSr, 2), 11.24)
  expect_equal(round(MSw, 2),  6.26)
  expect_equal(round(MSc, 2), 32.49)
  expect_equal(round(MSe, 2),  1.02)
})

test_that("reliability_icc matches Shrout & Fleiss (1979) Table 4 for all 6 forms", {
  m <- make_sf_table2()

  expectations <- list(
    list(label = "ICC(1,1)", expect = 0.17,
         args = list(model = "oneway", unit = "single")),
    list(label = "ICC(2,1)", expect = 0.29,
         args = list(model = "twoway", type = "agreement",   unit = "single")),
    list(label = "ICC(3,1)", expect = 0.71,
         args = list(model = "twoway", type = "consistency", unit = "single")),
    list(label = "ICC(1,4)", expect = 0.44,
         args = list(model = "oneway", unit = "average")),
    list(label = "ICC(2,4)", expect = 0.62,
         args = list(model = "twoway", type = "agreement",   unit = "average")),
    list(label = "ICC(3,4)", expect = 0.91,
         args = list(model = "twoway", type = "consistency", unit = "average"))
  )

  for (e in expectations) {
    r <- do.call(reliability_icc, c(list(ratings = m), e$args))
    expect_equal(round(r$value, 2), e$expect, info = e$label)
    expect_equal(r$icc_name, e$label)
  }
})

test_that("reliability_icc CI bounds enclose the point estimate", {
  m <- make_sf_table2()
  configs <- list(
    list(model = "oneway", unit = "single"),
    list(model = "oneway", unit = "average"),
    list(model = "twoway", type = "agreement",   unit = "single"),
    list(model = "twoway", type = "agreement",   unit = "average"),
    list(model = "twoway", type = "consistency", unit = "single"),
    list(model = "twoway", type = "consistency", unit = "average")
  )
  for (cfg in configs) {
    r <- do.call(reliability_icc, c(list(ratings = m), cfg))
    expect_true(r$ci_lower <= r$value && r$value <= r$ci_upper,
                info = r$icc_name)
  }
})

test_that("reliability_icc consistency-vs-agreement F-tests share statistic", {
  # For the two-way model the consistency and agreement F-tests of H0: ICC=0
  # both reduce to MSr/MSe (= 11.24/1.02 ~= 11.03 from the unrounded MS).
  m <- make_sf_table2()
  r_c <- reliability_icc(m, model = "twoway", type = "consistency", unit = "single")
  r_a <- reliability_icc(m, model = "twoway", type = "agreement",   unit = "single")
  expect_equal(round(r_c$F_value, 2), round(r_a$F_value, 2))
  expect_equal(round(r_c$F_value, 1), 11.0)
})

test_that("reliability_icc returns 1 for perfect agreement", {
  # Identical ratings across raters -> ICC = 1 for all forms.
  m <- cbind(c(1, 2, 3, 4, 5),
             c(1, 2, 3, 4, 5),
             c(1, 2, 3, 4, 5))
  for (cfg in list(
    list(model = "oneway", unit = "single"),
    list(model = "twoway", type = "consistency", unit = "single"),
    list(model = "twoway", type = "agreement",   unit = "single")
  )) {
    r <- do.call(reliability_icc, c(list(ratings = m), cfg))
    expect_equal(r$value, 1.0, info = r$icc_name)
  }
})

test_that("reliability_icc consistency and agreement diverge under rater bias", {
  # Adding a constant to one rater's column inflates MSc (column variance)
  # but leaves MSr/MSe unchanged. So ICC(3,1) -- which excludes column
  # variance -- is unaffected, while ICC(2,1) -- which includes it -- drops.
  m_clean <- cbind(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
  m_biased <- m_clean
  m_biased[, 2] <- m_biased[, 2] + 5  # rater 2 reads everything 5 too high

  consist <- reliability_icc(m_biased, model = "twoway",
                             type = "consistency", unit = "single")$value
  agree   <- reliability_icc(m_biased, model = "twoway",
                             type = "agreement",   unit = "single")$value
  expect_equal(consist, 1.0)
  expect_lt(agree, 1.0)
})

test_that("reliability_icc errors on NA, too few subjects, or too few raters", {
  expect_error(reliability_icc(matrix(c(1, NA, 2, 3), 2, 2)), "NA")
  expect_error(reliability_icc(matrix(1:2, 1, 2)), "at least 2 subjects")
  expect_error(reliability_icc(matrix(1:5, 5, 1)), "at least 2 raters")
})

test_that("reliability_icc returns uniform output shape", {
  m <- make_sf_table2()
  r <- reliability_icc(m, model = "twoway", type = "agreement", unit = "single")

  expected_names <- c("method", "value", "ci_lower", "ci_upper", "per_value",
                      "n_observers", "n_units", "n_pairable",
                      "model", "type", "unit", "icc_name",
                      "F_value", "df1", "df2", "p_value", "r0")
  expect_named(r, expected_names)
  expect_equal(r$method, "icc_2_1")
  expect_null(r$per_value)
  expect_equal(r$n_observers, 4L)
  expect_equal(r$n_units, 6L)
  expect_equal(r$n_pairable, 24L)
})
