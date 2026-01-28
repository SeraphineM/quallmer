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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
  skip_if_not_installed("irr")

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
