# Tests for reliability_kappa() and reliability_kappa_fleiss().
#
# Worked examples:
# - Cohen (1960) Table 2: 200 subjects, 2 raters, 3 categories. kappa = 0.492,
#   sigma_kappa = 0.055, 95% CI (0.384, 0.600).
# - Fleiss (1971) Table 1: 30 subjects, 6 raters, 5 categories. kappa = 0.430,
#   sigma_kappa ~= 0.028. Per-category kappa_j = 0.248, 0.248, 0.517, 0.470, 0.565
#   (book uses 3-decimal rounded p_j; native gives 0.245, 0.245, 0.520,
#   0.471, 0.566 -- accept tolerance ~0.005).
# - Krippendorff sec.12.3.4.4 (Mary & Dave): hand-calculated kappa = 0.750.

# -- Cohen (1960) Table 2 ------------------------------------------------------

# Cross-tab from book p. 45 (Judge B rows x Judge A cols):
#         A=1  A=2  A=3
#  B=1    88   14   18    120
#  B=2    10   40   10     60
#  B=3     2    6   12     20
#         100   60   40    200
make_cohen_t2 <- function() {
  cells <- list(
    c(1, 1, 88L), c(2, 1, 14L), c(3, 1, 18L),
    c(1, 2, 10L), c(2, 2, 40L), c(3, 2, 10L),
    c(1, 3,  2L), c(2, 3,  6L), c(3, 3, 12L)
  )
  do.call(rbind, lapply(cells, function(x) {
    if (x[3] == 0L) NULL
    else matrix(rep(c(x[1], x[2]), x[3]), ncol = 2L, byrow = TRUE)
  }))
}

test_that("reliability_kappa matches Cohen (1960) Table 2", {
  m <- make_cohen_t2()
  expect_equal(nrow(m), 200L)

  r <- reliability_kappa(m, weight = "unweighted")
  expect_equal(round(r$value, 3), 0.492)
  expect_equal(r$n_observers, 2L)
  expect_equal(r$n_units, 200L)

  # sigma_kappa via Eq. 7: book reports 0.055; CI half-width = 1.96 x sigma_kappa
  se <- (r$ci_upper - r$value) / 1.96
  expect_equal(round(se, 3), 0.055)

  # 95% CI from book: (0.384, 0.600). Native value 0.599 differs by 0.001
  # because the book carries sigma_kappa ~= .055 forward; native uses unrounded SE.
  expect_equal(round(r$ci_lower, 3), 0.384)
  expect_equal(round(r$ci_upper, 2), 0.60)
})

test_that("reliability_kappa matches Mary & Dave (Krippendorff sec.12.3.4.4)", {
  md <- cbind(
    Mary = c("a", "a", "c", "c", "c", "c", "c", "b", "b", "b", "b", "d"),
    Dave = c("a", "c", "c", "c", "c", "c", "b", "b", "b", "b", "b", "d")
  )
  md_int <- apply(md, 2L, function(col) {
    as.integer(factor(col, levels = c("a", "b", "c", "d")))
  })

  r <- reliability_kappa(md_int)
  expect_equal(r$value, 0.75, tolerance = 1e-9)

  # Per-category marginals: a=3, b=9, c=10, d=2 (sum across both raters)
  expect_equal(setNames(r$per_value$n, r$per_value$value),
               c("1" = 3L, "2" = 9L, "3" = 10L, "4" = 2L))
})

test_that("reliability_kappa weighted (squared) reduces correctly on perfect agreement", {
  m <- cbind(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5))
  for (w in c("unweighted", "equal", "squared")) {
    r <- reliability_kappa(m, weight = w)
    expect_equal(r$value, 1.0, info = paste("weight =", w))
  }
})

test_that("reliability_kappa errors on wrong number of raters", {
  m1 <- matrix(1:5, ncol = 1L)
  m3 <- matrix(1:9, ncol = 3L)
  expect_error(reliability_kappa(m1), "exactly 2 raters")
  expect_error(reliability_kappa(m3), "exactly 2 raters")
})

test_that("reliability_kappa errors on NA input", {
  m <- cbind(c(1, 2, NA), c(1, 2, 1))
  expect_error(reliability_kappa(m), "NA")
})

test_that("reliability_kappa returns uniform output shape", {
  m <- make_cohen_t2()
  r <- reliability_kappa(m)
  expected_names <- c("method", "value", "ci_lower", "ci_upper", "per_value",
                      "n_observers", "n_units", "n_pairable")
  expect_named(r, expected_names)
  expect_equal(r$method, "kappa_cohen")
  expect_true(is.data.frame(r$per_value))

  # Weighted: ci/per_value not provided
  rw <- reliability_kappa(m, weight = "squared")
  expect_named(rw, expected_names)
  expect_true(is.na(rw$ci_lower))
  expect_null(rw$per_value)
})


# -- Fleiss (1971) Table 1 -----------------------------------------------------

# n_ij counts (subject x category): how many of the 6 raters placed each
# subject into each category. Categories: 1=Depression, 2=Personality
# disorder, 3=Schizophrenia, 4=Neurosis, 5=Other.
make_fleiss_t1 <- function() {
  rbind(
    c(0,0,0,6,0),  # 1
    c(0,3,0,0,3),  # 2
    c(0,1,4,0,1),  # 3
    c(0,0,0,0,6),  # 4
    c(0,3,0,3,0),  # 5
    c(2,0,4,0,0),  # 6
    c(0,0,4,0,2),  # 7
    c(2,0,3,1,0),  # 8
    c(2,0,0,4,0),  # 9
    c(0,0,0,0,6),  # 10
    c(1,0,0,5,0),  # 11
    c(1,1,0,4,0),  # 12
    c(0,3,3,0,0),  # 13
    c(1,0,0,5,0),  # 14
    c(0,2,0,3,1),  # 15
    c(0,0,5,0,1),  # 16
    c(3,0,0,1,2),  # 17
    c(5,1,0,0,0),  # 18
    c(0,2,0,4,0),  # 19
    c(1,0,2,0,3),  # 20
    c(0,0,0,0,6),  # 21
    c(0,1,0,5,0),  # 22
    c(0,2,0,1,3),  # 23
    c(2,0,0,4,0),  # 24
    c(1,0,0,4,1),  # 25
    c(0,5,0,1,0),  # 26
    c(4,0,0,0,2),  # 27
    c(0,2,0,4,0),  # 28
    c(1,0,5,0,0),  # 29
    c(0,0,0,0,6)   # 30
  )
}

# Re-expand n_ij counts back to a subjects x raters matrix. Rater identity
# is irrelevant for Fleiss' kappa; this just gives the function the correct
# per-subject category multiplicities.
expand_to_ratings <- function(nij) {
  N <- nrow(nij); k <- ncol(nij); n <- sum(nij[1L, ])
  out <- matrix(0L, nrow = N, ncol = n)
  for (i in seq_len(N)) out[i, ] <- rep.int(seq_len(k), nij[i, ])
  out
}

test_that("Fleiss Table 1 reconstruction matches book-reported sums", {
  nij <- make_fleiss_t1()
  expect_true(all(rowSums(nij) == 6L))                          # n = 6 raters/subject
  expect_equal(colSums(nij), c(26L, 26L, 30L, 55L, 43L))        # book p. 379
  expect_equal(colSums(nij^2), c(72L, 72L, 120L, 229L, 187L))   # book Table 2
  expect_equal(sum(nij^2), 680L)                                # book p. 379
})

test_that("reliability_kappa_fleiss matches Fleiss (1971) Table 1", {
  ratings <- expand_to_ratings(make_fleiss_t1())
  r <- reliability_kappa_fleiss(ratings)

  expect_equal(round(r$value, 3), 0.430)
  expect_equal(r$n_observers, 6L)
  expect_equal(r$n_units, 30L)
  expect_equal(r$n_pairable, 180L)

  se <- (r$ci_upper - r$value) / 1.96
  expect_equal(round(se, 3), 0.028)

  # Per-category kappa_j -- book reports 0.248, 0.248, 0.517, 0.470, 0.565 using
  # rounded p_j; native uses exact p_j so values drift up to ~0.003.
  expected_kappa <- c("1" = 0.248, "2" = 0.248, "3" = 0.517,
                      "4" = 0.470, "5" = 0.565)
  observed <- setNames(r$per_value$kappa, r$per_value$value)
  expect_true(all(abs(observed - expected_kappa) < 0.005),
              info = paste0("max abs diff: ",
                            signif(max(abs(observed - expected_kappa)), 3)))

  # Marginal counts come straight from column totals
  expect_equal(setNames(r$per_value$n, r$per_value$value),
               c("1" = 26L, "2" = 26L, "3" = 30L, "4" = 55L, "5" = 43L))
})

test_that("reliability_kappa_fleiss returns 1 for perfect agreement", {
  m <- matrix(rep(c(1, 2, 3), each = 4L), nrow = 3L, byrow = TRUE)
  r <- reliability_kappa_fleiss(m)
  expect_equal(r$value, 1.0)
})

test_that("reliability_kappa_fleiss errors on NA input", {
  m <- rbind(c(1, 1, NA), c(2, 2, 2))
  expect_error(reliability_kappa_fleiss(m), "NA")
})

test_that("reliability_kappa_fleiss returns uniform output shape", {
  ratings <- expand_to_ratings(make_fleiss_t1())
  r <- reliability_kappa_fleiss(ratings)
  expected_names <- c("method", "value", "ci_lower", "ci_upper", "per_value",
                      "n_observers", "n_units", "n_pairable")
  expect_named(r, expected_names)
  expect_equal(r$method, "kappa_fleiss")
  expect_false(is.na(r$ci_lower))    # analytic SE always populated
  expect_false(is.na(r$ci_upper))
  expect_true(is.data.frame(r$per_value))
})
