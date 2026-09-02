# Helper function to extract metric value from qlm_comparison result
get_comparison_metric <- function(result, metric_name, variable = NULL) {
  if (!is.null(variable)) {
    rows <- result[result$variable == variable & result$measure == metric_name, ]
  } else {
    rows <- result[result$measure == metric_name, ]
  }
  if (nrow(rows) == 0) return(NA_real_)
  rows$value[1]
}

test_that("qlm_compare validates inputs correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results1 <- data.frame(id = 1:3, score = c(1, 2, 3))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Should error with only one object
  expect_error(
    qlm_compare(mock_coded1, by = "score"),
    "At least two.*qlm_coded.*objects"
  )

  # Should error with non-qlm_coded objects
  expect_error(
    qlm_compare(mock_coded1, list(a = 1), by = "score"),
    "must be.*qlm_coded.*objects"
  )
})


test_that("qlm_compare checks for 'by' variable", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results1 <- data.frame(id = 1:3, score = c(1, 2, 3))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:3, score = c(1, 2, 2))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Should error if 'by' variable doesn't exist
  expect_error(
    qlm_compare(mock_coded1, mock_coded2, by = "nonexistent"),
    "Variable.*nonexistent.*not found"
  )
})


test_that("qlm_compare works with matching units", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create two coded objects with same units
  mock_results1 <- data.frame(id = 1:5, score = c(1, 2, 3, 1, 2))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:5, score = c(1, 2, 2, 1, 3))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Compare using interval level
  comparison <- qlm_compare(mock_coded1, mock_coded2, by = "score", level = "interval")

  expect_true(inherits(comparison, "qlm_comparison"))
  expect_true(all(comparison$level == "interval"))
  expect_true(is.numeric(get_comparison_metric(comparison, "alpha_interval")))
  expect_true(is.numeric(get_comparison_metric(comparison, "icc")))
  expect_true(is.numeric(get_comparison_metric(comparison, "r")))
  expect_true(is.numeric(get_comparison_metric(comparison, "percent_agreement")))
  expect_equal(attr(comparison, "n"), 5)
  expect_equal(attr(comparison, "raters"), 2)
})


test_that("qlm_compare handles Cohen's kappa for 2 raters", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    category = ellmer::type_string("Category")
  )
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create categorical data
  mock_results1 <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:10, category = c(rep("A", 8), "B", "B"))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Compare using nominal level (should compute Cohen's kappa for 2 raters)
  comparison <- qlm_compare(mock_coded1, mock_coded2,
                           by = "category",
                           level = "nominal")

  expect_true(all(comparison$level == "nominal"))
  expect_equal(attr(comparison, "raters"), 2)
  # kappa_type is not returned in the data frame, just check kappa exists
  expect_true(is.numeric(get_comparison_metric(comparison, "kappa")))
  expect_true(is.numeric(get_comparison_metric(comparison, "alpha_nominal")))
  expect_true(is.numeric(get_comparison_metric(comparison, "percent_agreement")))
})


test_that("qlm_compare handles Fleiss' kappa for 3+ raters", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    category = ellmer::type_string("Category")
  )
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create three coded objects
  mock_results1 <- data.frame(id = 1:8, category = rep(c("A", "B"), 4))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:8),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 8),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:8, category = c(rep("A", 6), "B", "B"))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:8),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 8),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results3 <- data.frame(id = 1:8, category = c(rep("A", 7), "B"))
  mock_coded3 <- new_qlm_coded(
    results = mock_results3,
    codebook = codebook,
    data = paste0("text", 1:8),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 8),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Compare using nominal level (should compute Fleiss' kappa for 3 raters)
  comparison <- qlm_compare(mock_coded1, mock_coded2, mock_coded3,
                           by = "category",
                           level = "nominal")

  expect_true(all(comparison$level == "nominal"))
  expect_equal(attr(comparison, "raters"), 3)
  # kappa_type not returned in data frame
  expect_true(is.numeric(get_comparison_metric(comparison, "kappa")))
  expect_true(is.numeric(get_comparison_metric(comparison, "alpha_nominal")))
  expect_true(is.numeric(get_comparison_metric(comparison, "percent_agreement")))
})


test_that("qlm_compare computes percent agreement", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create two coded objects with high agreement
  mock_results1 <- data.frame(id = 1:5, score = c(1, 2, 3, 1, 2))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:5, score = c(1, 2, 3, 1, 3))  # 4/5 agree
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Compare using nominal level (includes percent agreement)
  comparison <- qlm_compare(mock_coded1, mock_coded2,
                           by = "score",
                           level = "nominal")

  expect_true(all(comparison$level == "nominal"))
  expect_equal(get_comparison_metric(comparison, "percent_agreement"), 0.8)  # 4 out of 5
  expect_true(is.numeric(get_comparison_metric(comparison, "kappa")))
  expect_true(is.numeric(get_comparison_metric(comparison, "alpha_nominal")))
})


test_that("qlm_compare handles mismatched units", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create objects with different units
  mock_results1 <- data.frame(id = 1:3, score = c(1, 2, 3))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 4:6, score = c(1, 2, 3))  # Different IDs
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = c("text4", "text5", "text6"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Should error with no common units
  expect_error(
    qlm_compare(mock_coded1, mock_coded2, by = "score", level = "interval"),
    "No valid comparisons could be computed"
  )
})


test_that("print.qlm_comparison displays correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results1 <- data.frame(id = 1:5, score = c(1, 2, 3, 1, 2))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:5, score = c(1, 2, 2, 1, 3))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  comparison <- qlm_compare(mock_coded1, mock_coded2, by = "score", level = "interval")

  # Just verify print doesn't error (cli output isn't captured well)
  expect_no_error(print(comparison))

  # Verify structure
  expect_true(inherits(comparison, "qlm_comparison"))
  expect_equal(unique(comparison$variable), "score")
  expect_equal(unique(comparison$level), "interval")
  expect_equal(attr(comparison, "n"), 5)
  expect_equal(attr(comparison, "raters"), 2)
})

test_that("qlm_compare accepts plain data.frames for all arguments", {
  skip_if_not_installed("ellmer")

  # Create two plain data.frames (simulating human coders)
  coder1 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))
  coder2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Should work with plain data.frames
  comparison <- qlm_compare(coder1, coder2, by = category, level = "nominal")

  expect_true(inherits(comparison, "qlm_comparison"))
  expect_equal(get_comparison_metric(comparison, "percent_agreement"), 1.0)  # Perfect agreement
  expect_equal(attr(comparison, "n"), 10)
  expect_equal(attr(comparison, "raters"), 2)
})

test_that("qlm_compare works with plain data.frames and imperfect agreement", {
  skip_if_not_installed("ellmer")

  # Create two plain data.frames with different values
  coder1 <- data.frame(.id = 1:10, category = c(rep("A", 7), rep("B", 3)))
  coder2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  comparison <- qlm_compare(coder1, coder2, by = category, level = "nominal")

  expect_true(inherits(comparison, "qlm_comparison"))
  pct_agree <- get_comparison_metric(comparison, "percent_agreement")
  expect_true(pct_agree < 1.0)
  expect_true(is.numeric(get_comparison_metric(comparison, "alpha_nominal")))
  expect_true(is.numeric(get_comparison_metric(comparison, "kappa")))
})

test_that("qlm_compare works with three plain data.frames", {
  skip_if_not_installed("ellmer")

  # Create three plain data.frames
  coder1 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))
  coder2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))
  coder3 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  comparison <- qlm_compare(coder1, coder2, coder3, by = category, level = "nominal")

  expect_true(inherits(comparison, "qlm_comparison"))
  expect_equal(attr(comparison, "raters"), 3)
  # kappa_type not returned in data frame
})

test_that("qlm_compare supports non-standard evaluation for by argument", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create two coded objects
  mock_results1 <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mock_results2 <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Test with unquoted variable name (NSE)
  comparison_nse <- qlm_compare(mock_coded1, mock_coded2, by = category, level = "nominal")

  # Test with quoted variable name (traditional)
  comparison_quoted <- qlm_compare(mock_coded1, mock_coded2, by = "category", level = "nominal")

  # Both should work and produce identical results
  expect_true(inherits(comparison_nse, "qlm_comparison"))
  expect_true(inherits(comparison_quoted, "qlm_comparison"))
  expect_equal(get_comparison_metric(comparison_nse, "alpha_nominal"),
               get_comparison_metric(comparison_quoted, "alpha_nominal"))
  expect_equal(get_comparison_metric(comparison_nse, "kappa"),
               get_comparison_metric(comparison_quoted, "kappa"))
  expect_equal(get_comparison_metric(comparison_nse, "percent_agreement"),
               get_comparison_metric(comparison_quoted, "percent_agreement"))
  expect_equal(attr(comparison_nse, "n"), attr(comparison_quoted, "n"))
  expect_equal(attr(comparison_nse, "raters"), attr(comparison_quoted, "raters"))
})


test_that("qlm_compare suppresses per-category rows by default", {
  coder1 <- data.frame(.id = 1:10, category = c(rep("A", 6), rep("B", 4)))
  coder2 <- data.frame(.id = 1:10, category = c(rep("A", 5), rep("B", 5)))

  default_result <- qlm_compare(coder1, coder2, by = category, level = "nominal")
  expect_false(any(grepl("^alpha_per_value\\[", default_result$measure)))
  expect_false(any(grepl("^kappa_per_value\\[", default_result$measure)))
  # Overall measures still present
  expect_true("alpha_nominal" %in% default_result$measure)
  expect_true("kappa" %in% default_result$measure)
})


test_that("qlm_compare returns per-category rows when by_category = TRUE", {
  coder1 <- data.frame(.id = 1:10, category = c(rep("A", 6), rep("B", 4)))
  coder2 <- data.frame(.id = 1:10, category = c(rep("A", 5), rep("B", 5)))

  result <- qlm_compare(coder1, coder2, by = category, level = "nominal",
                       by_category = TRUE)
  expect_true(any(grepl("^alpha_per_value\\[", result$measure)))
  expect_true(any(grepl("^kappa_per_value\\[", result$measure)))
  # docid carries marginal n for per-category rows
  pv_rows <- result[grepl("^alpha_per_value\\[", result$measure), ]
  expect_true(all(grepl("^\\(n=", pv_rows$docid)))
})


test_that("qlm_compare by_category has no effect on non-nominal levels", {
  coder1 <- data.frame(.id = 1:10, score = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
  coder2 <- data.frame(.id = 1:10, score = c(1, 2, 3, 4, 5, 2, 2, 3, 4, 5))

  ordinal_default <- qlm_compare(coder1, coder2, by = score, level = "ordinal")
  ordinal_by_cat  <- qlm_compare(coder1, coder2, by = score, level = "ordinal",
                                 by_category = TRUE)
  expect_identical(ordinal_default$measure, ordinal_by_cat$measure)
  expect_false(any(grepl("per_value", ordinal_default$measure)))
  expect_false(any(grepl("per_value", ordinal_by_cat$measure)))

  interval_default <- qlm_compare(coder1, coder2, by = score, level = "interval")
  interval_by_cat  <- qlm_compare(coder1, coder2, by = score, level = "interval",
                                  by_category = TRUE)
  expect_identical(interval_default$measure, interval_by_cat$measure)
  expect_false(any(grepl("per_value", interval_default$measure)))
  expect_false(any(grepl("per_value", interval_by_cat$measure)))
})


test_that("qlm_compare validates by_category argument", {
  coder1 <- data.frame(.id = 1:5, category = c("A", "B", "A", "B", "A"))
  coder2 <- data.frame(.id = 1:5, category = c("A", "B", "A", "B", "A"))

  expect_error(
    qlm_compare(coder1, coder2, by = category, level = "nominal",
                by_category = "yes"),
    "by_category"
  )
  expect_error(
    qlm_compare(coder1, coder2, by = category, level = "nominal",
                by_category = NA),
    "by_category"
  )
})


# ---- agreement at exactly the tolerance boundary (#121) ---------------------

# Cases about the predicate itself are tested directly: routing them through
# qlm_compare() would need >= 2 units purely to keep ICC quiet, which would
# obscure what each case is actually pinning.

compare_pair <- function(a, b, tolerance, level = "interval") {
  x <- as_qlm_coded(data.frame(.id = seq_along(a), g = a), id = .id, name = "A")
  y <- as_qlm_coded(data.frame(.id = seq_along(b), g = b), id = .id, name = "B")
  res <- as.data.frame(
    qlm_compare(x, y, by = "g", level = level, tolerance = tolerance)
  )
  res$value[res$measure == "percent_agreement"]
}


test_that("a difference of exactly the tolerance counts as agreement (#121)", {
  # The reported case, end to end. Correct agreement is 4/4; a raw `<=` on the
  # subtraction gave 0.5.
  a <- c(1.1, 1.2, 1.3, 0.9)
  b <- c(1.0, 1.1, 1.2, 0.9)

  expect_equal(compare_pair(a, b, tolerance = 0.1), 1)
})


test_that("each at-tolerance pair agrees individually (#121)", {
  # Pairwise, because only two of the three failed: a test built solely on
  # 1.2/1.1 would have passed against the broken comparison.
  expect_true(agrees_within_tolerance(c(1.1, 1.0), 0.1))
  expect_true(agrees_within_tolerance(c(1.2, 1.1), 0.1))
  expect_true(agrees_within_tolerance(c(1.3, 1.2), 0.1))
})


test_that("a difference beyond the tolerance still disagrees (#121)", {
  # The guard that the comparison was corrected rather than loosened.
  expect_false(agrees_within_tolerance(c(1.0 + 0.1 + 1e-6, 1.0), 0.1))
  expect_false(agrees_within_tolerance(c(1.2, 1.0), 0.1))
  expect_equal(compare_pair(c(1.2, 1.1), c(1.0, 1.2), tolerance = 0.1), 0.5)
})


test_that("other decimal grids work at their own increment (#121)", {
  expect_equal(compare_pair(c(0.25, 0.75), c(0.5, 1.0), tolerance = 0.25), 1)
  expect_equal(compare_pair(c(1.5, 2.5), c(1.0, 2.0), tolerance = 0.5), 1)
  expect_equal(compare_pair(c(0.3, 0.6), c(0.2, 0.5), tolerance = 0.1), 1)
})


test_that("tolerance = 0 means numerical equality, not bit identity (#121)", {
  # 0.1 + 0.2 is 0.30000000000000004, the same number for any measurement
  # purpose.
  expect_true(agrees_within_tolerance(c(0.1 + 0.2, 0.3), 0))
  expect_equal(compare_pair(c(0.1 + 0.2, 1), c(0.3, 1), tolerance = 0), 1)

  # A real difference is still a difference. This is what rules out
  # sqrt(.Machine$double.eps): at about 1.5e-8 it would call this agreement,
  # over-reporting, which is the more damaging direction for a reliability
  # statistic.
  expect_false(agrees_within_tolerance(c(0, 1e-9), 0))
  expect_equal(compare_pair(c(1, 2), c(1, 3), tolerance = 0), 0.5)
})


test_that("the boundary holds at large magnitudes too (#121)", {
  # The epsilon scales with the values, so it tracks the precision actually
  # available there rather than assuming ratings are of order 1.
  expect_true(agrees_within_tolerance(c(1e6 + 0.1, 1e6), 0.1))
  expect_false(agrees_within_tolerance(c(1e6 + 0.2, 1e6), 0.1))
  expect_true(agrees_within_tolerance(c(1e8 + 0.1, 1e8), 0.1))
  expect_false(agrees_within_tolerance(c(1e8 + 0.2, 1e8), 0.1))
})


test_that("agrees_within_tolerance() handles more than two raters (#121)", {
  # The comparison is on max - min, so a third rater inside the band must not
  # change the verdict.
  expect_true(agrees_within_tolerance(c(1.0, 1.05, 1.1), 0.1))
  expect_false(agrees_within_tolerance(c(1.0, 1.05, 1.2), 0.1))
  expect_true(agrees_within_tolerance(1.0, 0))
})


test_that("nominal comparison is unaffected by the tolerance change (#121)", {
  expect_equal(compare_pair(c("a", "b"), c("a", "b"), tolerance = 0, level = "nominal"), 1)
  expect_equal(compare_pair(c("a", "b"), c("a", "c"), tolerance = 0, level = "nominal"), 0.5)
})
