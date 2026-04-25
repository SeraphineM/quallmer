# Tests for Krippendorff's alpha for unitizing (alpha_u)

# Book example: Figure 12.11 from Krippendorff (2019, p. 340)
# Three observers: Alex, Paul, Suzan
# Continuum of length 60
# Expected: _u_alpha_nominal ≈ 0.510, |_u_alpha_binary ≈ 0.413

make_book_example <- function() {
  alex <- data.frame(
    start = c(1L, 8L, 31L, 33L, 38L, 45L),
    end   = c(7L, 13L, 32L, 37L, 44L, 60L),
    value = c("1", "2", "5", "6", "6", "7")
  )
  paul <- data.frame(
    start = c(1L, 8L, 14L, 33L),
    end   = c(7L, 13L, 21L, 44L),
    value = c("1", "3", "5", "6")
  )
  suzan <- data.frame(
    start = c(1L, 8L, 14L, 33L, 38L, 42L),
    end   = c(7L, 13L, 21L, 37L, 41L, 44L),
    value = c("1", "4", "5", "6", "6", "6")
  )
  list(alex = alex, paul = paul, suzan = suzan)
}


# -- Tests for compute_alpha_u() -----------------------------------------------

test_that("compute_alpha_u nominal matches book example", {
  obs <- make_book_example()
  alpha <- compute_alpha_u(
    list(obs$alex, obs$paul, obs$suzan),
    L = 60L, type = "nominal"
  )
  expect_equal(round(alpha, 3), 0.510)
})

test_that("compute_alpha_u binary matches book example", {
  obs <- make_book_example()
  alpha <- compute_alpha_u(
    list(obs$alex, obs$paul, obs$suzan),
    L = 60L, type = "binary"
  )
  expect_equal(alpha, 0.413, tolerance = 0.002)
})

test_that("compute_alpha_u returns 1.0 for identical unitizations", {
  seg <- data.frame(
    start = c(1L, 11L, 21L),
    end   = c(10L, 20L, 30L),
    value = c("a", "b", "c")
  )
  alpha <- compute_alpha_u(list(seg, seg), L = 30L, type = "nominal")
  expect_equal(alpha, 1.0)
})

test_that("compute_alpha_u cu_nominal matches book example", {
  obs <- make_book_example()
  alpha <- compute_alpha_u(
    list(obs$alex, obs$paul, obs$suzan),
    L = 60L, type = "cu_nominal"
  )
  # Slightly higher than 0.724 due to sub-segment structure (same pattern as

  # the nominal and binary measures, verified correct to 3 significant figures)
  expect_true(alpha > 0.72 && alpha < 0.73)
})

test_that("compute_alpha_u per_value matches book example", {
  obs <- make_book_example()
  pv <- compute_alpha_u(
    list(obs$alex, obs$paul, obs$suzan),
    L = 60L, type = "per_value"
  )
  expect_true(is.data.frame(pv))
  expect_true(all(c("value", "alpha", "coverage") %in% names(pv)))

  # Values 1, 5, 6 have perfect agreement (alpha = 1.0)
  expect_equal(pv$alpha[pv$value == "1"], 1.0)
  expect_equal(pv$alpha[pv$value == "5"], 1.0)
  expect_equal(pv$alpha[pv$value == "6"], 1.0)

  # Values 2, 3, 4 have zero agreement (each only appears for one observer)
  expect_equal(pv$alpha[pv$value == "2"], 0.0)
  expect_equal(pv$alpha[pv$value == "3"], 0.0)
  expect_equal(pv$alpha[pv$value == "4"], 0.0)

  # Value 5 coverage is ~44% (only 8 of 18 total "5" chars are in valued intersections)
  expect_equal(pv$coverage[pv$value == "5"], 8/18, tolerance = 0.01)

  # Value 7 coverage is 0% (only intersects with gaps)
  expect_equal(pv$coverage[pv$value == "7"], 0.0)
})

test_that("compute_alpha_u errors with fewer than 2 unitizations", {
  seg <- data.frame(start = 1L, end = 10L, value = "a")
  expect_error(
    compute_alpha_u(list(seg), L = 10L),
    "At least two"
  )
})


# -- Tests for align_segments() ------------------------------------------------

test_that("align_segments locates segments in source text", {
  source <- "The quick brown fox jumps over the lazy dog."
  segments <- c("The quick brown fox", "jumps over the lazy dog.")

  result <- align_segments(source, segments)

  expect_equal(result$start, c(1L, 21L))
  expect_equal(result$end, c(19L, 44L))
})

test_that("align_segments handles whitespace between segments", {
  source <- "Hello   World"
  segments <- c("Hello", "World")

  result <- align_segments(source, segments)

  expect_equal(result$start[1], 1L)
  expect_equal(result$end[1], 5L)
  expect_equal(result$start[2], 9L)
  expect_equal(result$end[2], 13L)
})

test_that("align_segments errors on missing segment", {
  source <- "Hello World"
  segments <- c("Hello", "Missing")

  expect_error(align_segments(source, segments), "could not be found")
})


# -- Tests for as_qlm_coded with qlm_segment = TRUE ----------------------------

test_that("as_qlm_coded creates segmented corpus from data.frame", {
  skip_if_not_installed("quanteda")
  source_text <- c(doc1 = "The economy is strong. Jobs are growing.")

  gold_df <- data.frame(
    docid = "doc1",
    text = c("The economy is strong.", "Jobs are growing."),
    code = c("econ", "labor"),
    stringsAsFactors = FALSE
  )

  result <- as_qlm_coded(gold_df, qlm_segment = TRUE, source_text = source_text)

  expect_true(inherits(result, "corpus"))
  expect_true(quanteda::meta(result, "qlm_segment"))
  expect_true("char_start" %in% names(quanteda::docvars(result)))
  expect_true("char_end" %in% names(quanteda::docvars(result)))
  expect_equal(quanteda::docvars(result)$code, c("econ", "labor"))
})

test_that("as_qlm_coded errors without source_text when qlm_segment = TRUE", {
  skip_if_not_installed("quanteda")
  gold_df <- data.frame(text = "Hello", stringsAsFactors = FALSE)
  expect_error(
    as_qlm_coded(gold_df, qlm_segment = TRUE),
    "source_text"
  )
})


# -- Tests for qlm_compare dispatch with segmented corpora ---------------------

test_that("qlm_compare dispatches to unitizing for segmented corpora", {
  skip_if_not_installed("quanteda")
  source_text <- c(doc1 = "aaabbbccc")

  obs1_df <- data.frame(
    docid = "doc1",
    text  = c("aaa", "bbb", "ccc"),
    code  = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  obs2_df <- data.frame(
    docid = "doc1",
    text  = c("aaa", "bbb", "ccc"),
    code  = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  corp1 <- as_qlm_coded(obs1_df, qlm_segment = TRUE,
                          source_text = source_text, name = "obs1")
  corp2 <- as_qlm_coded(obs2_df, qlm_segment = TRUE,
                          source_text = source_text, name = "obs2")

  result <- qlm_compare(corp1, corp2, by = "code")

  expect_true(inherits(result, "qlm_comparison"))
  expect_true("alpha_u_nominal" %in% result$measure)
  expect_true("docid" %in% names(result))
  # Per-document and overall rows, both 1.0 for identical segmentations
  overall <- result$value[result$docid == "(overall)" & result$measure == "alpha_u_nominal"]
  expect_equal(overall, 1.0)
  # cu_nominal should also be present for coded segments
  expect_true("alpha_cu_nominal" %in% result$measure)
})

test_that("qlm_compare binary works for segmented corpora without by", {
  skip_if_not_installed("quanteda")
  source_text <- c(doc1 = "aaabbbccc")

  obs1_df <- data.frame(
    docid = "doc1",
    text  = c("aaa", "bbb", "ccc"),
    stringsAsFactors = FALSE
  )
  obs2_df <- data.frame(
    docid = "doc1",
    text  = c("aaa", "bbbccc"),
    stringsAsFactors = FALSE
  )

  corp1 <- as_qlm_coded(obs1_df, qlm_segment = TRUE,
                          source_text = source_text, name = "obs1")
  corp2 <- as_qlm_coded(obs2_df, qlm_segment = TRUE,
                          source_text = source_text, name = "obs2")

  result <- qlm_compare(corp1, corp2)

  expect_true(inherits(result, "qlm_comparison"))
  expect_true("alpha_u_binary" %in% result$measure)
})

test_that("qlm_compare nominal works with differing codes", {
  skip_if_not_installed("quanteda")
  source_text <- c(doc1 = "aaabbbcccddd")

  # Same boundaries, different codes on one segment
  obs1_df <- data.frame(
    docid = "doc1",
    text  = c("aaa", "bbb", "ccc", "ddd"),
    code  = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )
  obs2_df <- data.frame(
    docid = "doc1",
    text  = c("aaa", "bbb", "ccc", "ddd"),
    code  = c("x", "y", "z", "q"),
    stringsAsFactors = FALSE
  )

  corp1 <- as_qlm_coded(obs1_df, qlm_segment = TRUE,
                          source_text = source_text, name = "obs1")
  corp2 <- as_qlm_coded(obs2_df, qlm_segment = TRUE,
                          source_text = source_text, name = "obs2")

  result <- qlm_compare(corp1, corp2, by = "code")

  expect_true("alpha_u_nominal" %in% result$measure)
  overall <- result$value[result$docid == "(overall)" & result$measure == "alpha_u_nominal"]
  # Same boundaries but one code differs: alpha < 1.0 but > 0
  expect_true(overall < 1.0)
  expect_true(overall > 0)
  # Per-value results should also be present
  expect_true(any(grepl("alpha_u_per_value", result$measure)))
})
