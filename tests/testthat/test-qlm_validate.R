# Helper function to extract metric value from qlm_validation result
get_metric <- function(result, metric_name, variable = NULL) {
  if (!is.null(variable)) {
    rows <- result[result$variable == variable & result$measure == metric_name, ]
  } else {
    rows <- result[result$measure == metric_name, ]
  }
  if (nrow(rows) == 0) return(NA_real_)
  rows$value[1]
}

test_that("qlm_validate validates inputs correctly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  # Create mock qlm_coded object
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  # Create gold standard
  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Should error with non-qlm_coded object
  expect_error(
    qlm_validate(list(a = 1), gold = gold, by = "category"),
    "must be data frames.*qlm_coded.*object"
  )

  # Should error with non-data.frame gold
  expect_error(
    qlm_validate(mock_coded, gold = list(a = 1), by = "category"),
    "must be a data frame"
  )

  # Should error if .id missing from gold
  gold_no_id <- data.frame(category = rep(c("A", "B"), 5))
  expect_error(
    qlm_validate(mock_coded, gold = gold_no_id, by = "category"),
    "must contain.*\\.id.*column"
  )

  # Should error if 'by' variable doesn't exist in x
  expect_error(
    qlm_validate(mock_coded, gold = gold, by = "nonexistent"),
    "Variable.*nonexistent.*not found"
  )

  # Should error if 'by' variable doesn't exist in gold
  gold_wrong_var <- data.frame(.id = 1:10, other = rep(c("A", "B"), 5))
  expect_error(
    qlm_validate(mock_coded, gold = gold_wrong_var, by = "category"),
    "Variable.*category.*not found.*gold"
  )
})

test_that("qlm_validate handles mismatched IDs", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = rep(c("A", "B"), length.out = 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  # Gold with different IDs
  gold <- data.frame(.id = 6:10, category = rep(c("A", "B"), length.out = 5))

  # Should error with no matching IDs
  expect_error(
    qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal"),
    "No valid validations could be computed"
  )
})

test_that("qlm_validate warns about NA values", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, category = c(rep(c("A", "B"), 4), NA, NA))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Should warn about NA values
  expect_warning(
    qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal"),
    "Missing values in variable"
  )
})

test_that("qlm_validate computes metrics correctly - perfect predictions", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Perfect predictions
  mock_results <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  # Check structure
  expect_true(inherits(validation, "qlm_validation"))
  expect_true(inherits(validation, "tbl_df"))

  # Check metrics via data frame
  expect_equal(get_metric(validation, "accuracy"), 1.0)
  expect_equal(get_metric(validation, "precision"), 1.0)
  expect_equal(get_metric(validation, "recall"), 1.0)
  expect_equal(get_metric(validation, "f1"), 1.0)

  # Check metadata via attributes
  expect_equal(attr(validation, "n"), 10)

  # Check data frame has expected columns
  expect_true(all(c("variable", "level", "measure", "value", "class") %in% names(validation)))
})

test_that("qlm_validate computes metrics correctly - imperfect predictions", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Imperfect predictions
  mock_results <- data.frame(
    id = 1:10,
    category = c(rep("A", 7), rep("B", 3))  # 7A, 3B
  )
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))  # 5A, 5B

  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  # Should have lower metrics than perfect
  accuracy <- get_metric(validation, "accuracy")
  expect_true(accuracy < 1.0)
  expect_true(accuracy >= 0.5)  # Should be at least as good as random
  expect_true(is.numeric(get_metric(validation, "precision")))
  expect_true(is.numeric(get_metric(validation, "recall")))
  expect_true(is.numeric(get_metric(validation, "f1")))
  expect_true(is.numeric(get_metric(validation, "kappa")))
})

test_that("qlm_validate handles average parameter correctly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:12, category = rep(c("A", "B", "C"), 4))
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:12),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 12),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  gold <- data.frame(.id = 1:12, category = rep(c("A", "B", "C"), 4))

  # Test different averaging methods
  val_macro <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal", average = "macro")
  # Check that class column shows <macro> for nominal global metrics
  expect_true(all(val_macro$class[!is.na(val_macro$class)] == "<macro>"))

  val_micro <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal", average = "micro")
  expect_true(all(val_micro$class[!is.na(val_micro$class)] == "<micro>"))

  val_weighted <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal", average = "weighted")
  expect_true(all(val_weighted$class[!is.na(val_weighted$class)] == "<weighted>"))

  val_none <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal", average = "none")
  # With average = "none", should have per-class metrics (class names) and global metrics (NA)
  expect_true(any(is.na(val_none$class)))  # Global metrics
  expect_true(any(!is.na(val_none$class) & val_none$class %in% c("A", "B", "C")))  # Per-class metrics
  # Check that per-class rows have measure names like precision, recall, f1
  per_class <- val_none[!is.na(val_none$class) & val_none$class != "<macro>", ]
  expect_true(any(per_class$measure == "precision"))
  expect_true(any(per_class$measure == "recall"))
  expect_true(any(per_class$measure == "f1"))
})

test_that("qlm_validate handles multiclass correctly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # 4-class problem
  mock_results <- data.frame(
    id = 1:20,
    category = rep(c("A", "B", "C", "D"), 5)
  )
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:20),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 20),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  gold <- data.frame(.id = 1:20, category = rep(c("A", "B", "C", "D"), 5))

  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  # Check that it's a valid result
  expect_true(inherits(validation, "qlm_validation"))
  expect_true(nrow(validation) > 0)
  # With perfect agreement, all metrics should be 1.0
  expect_equal(get_metric(validation, "accuracy"), 1.0)
})

test_that("print.qlm_validation displays correctly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  # Just verify that print doesn't error and produces output
  # (cli output isn't captured by capture.output)
  expect_no_error(print(validation))

  # Verify structure
  expect_true(inherits(validation, "qlm_validation"))
  expect_equal(unique(validation$variable), "category")
  expect_equal(unique(validation$level), "nominal")
  expect_equal(attr(validation, "n"), 10)
})

test_that("print.qlm_validation displays per-class metrics correctly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:12, category = rep(c("A", "B", "C"), 4))
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:12),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 12),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  gold <- data.frame(.id = 1:12, category = rep(c("A", "B", "C"), 4))

  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal", average = "none")

  # Just verify that print doesn't error
  # (cli output isn't captured by capture.output)
  expect_no_error(print(validation))

  # Verify that per-class metrics are present in the data frame
  expect_true(any(!is.na(validation$class) & validation$class %in% c("A", "B", "C")))
  per_class_rows <- validation[!is.na(validation$class) & validation$class != "<macro>", ]
  expect_true(nrow(per_class_rows) > 0)
  expect_true("precision" %in% per_class_rows$measure)
  expect_true("recall" %in% per_class_rows$measure)
  expect_true("f1" %in% per_class_rows$measure)
})

test_that("qlm_validate handles partial overlap of IDs", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  # Gold with partial overlap (IDs 5-15)
  gold <- data.frame(.id = 5:15, category = rep(c("A", "B"), length.out = 11))

  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  # Should only include overlapping IDs (5-10 = 6 units)
  expect_equal(attr(validation, "n"), 6)
})

test_that("qlm_validate handles all NAs error", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, category = rep(NA_character_, 10))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Should warn about NAs and error when all values are NA
  expect_error(
    suppressWarnings(qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")),
    "No valid validations could be computed"
  )
})

test_that("qlm_validate accepts qlm_coded object as gold standard", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create first qlm_coded object (predictions)
  mock_results1 <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model1"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create second qlm_coded object (gold standard)
  mock_results2 <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model2"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Should accept qlm_coded object as gold
  validation <- qlm_validate(mock_coded1, gold = mock_coded2, by = "category", level = "nominal")

  expect_true(inherits(validation, "qlm_validation"))
  expect_equal(get_metric(validation, "accuracy"), 1.0)  # Perfect match
  expect_equal(attr(validation, "n"), 10)
})

test_that("qlm_validate with qlm_coded gold handles imperfect predictions", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create predictions
  mock_results1 <- data.frame(
    id = 1:10,
    category = c(rep("A", 7), rep("B", 3))
  )
  mock_coded1 <- new_qlm_coded(
    results = mock_results1,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model1"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create gold standard (different predictions)
  mock_results2 <- data.frame(
    id = 1:10,
    category = rep(c("A", "B"), 5)
  )
  mock_coded2 <- new_qlm_coded(
    results = mock_results2,
    codebook = codebook,
    data = paste0("text", 1:10),
    input_type = "text",
    chat_args = list(name = "test/model2"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 10),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  validation <- qlm_validate(mock_coded1, gold = mock_coded2, by = "category", level = "nominal")

  expect_true(inherits(validation, "qlm_validation"))
  accuracy <- get_metric(validation, "accuracy")
  expect_true(accuracy < 1.0)
  expect_true(accuracy >= 0.5)
  expect_true(is.numeric(get_metric(validation, "precision")))
  expect_true(is.numeric(get_metric(validation, "recall")))
  expect_true(is.numeric(get_metric(validation, "f1")))
  expect_true(is.numeric(get_metric(validation, "kappa")))
})

test_that("qlm_validate ordinal level computes only appropriate metrics", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(rating = ellmer::type_integer("Rating"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create mock ratings data (1-5 scale)
  mock_results <- data.frame(
    id = 1:20,
    rating = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 2, 3, 4, 5, 1, 3, 4, 5, 1, 2)
  )
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:20),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 20),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Gold standard - slightly different ratings
  gold <- data.frame(
    .id = 1:20,
    rating = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 4, 2, 3, 3, 5, 1, 3, 4, 4, 2, 2)
  )

  # Validate with ordinal level
  validation <- qlm_validate(mock_coded, gold = gold, by = "rating", level = "ordinal")

  expect_true(inherits(validation, "qlm_validation"))
  expect_true(all(validation$level == "ordinal"))

  # Should have rho, tau, mae for ordinal data
  expect_true(is.numeric(get_metric(validation, "rho")))
  expect_true(is.numeric(get_metric(validation, "tau")))
  expect_true(is.numeric(get_metric(validation, "mae")))

  # Should NOT have nominal metrics - check that they're not in measure column
  expect_false("accuracy" %in% validation$measure)
  expect_false("precision" %in% validation$measure)
  expect_false("recall" %in% validation$measure)
  expect_false("f1" %in% validation$measure)
  expect_false("kappa" %in% validation$measure)

  # Should NOT have interval metrics
  expect_false("r" %in% validation$measure)
  expect_false("icc" %in% validation$measure)
  expect_false("rmse" %in% validation$measure)
})

test_that("qlm_validate ordinal correlation measures work correctly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(rating = ellmer::type_integer("Rating"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Create mock data with strong positive correlation
  mock_results <- data.frame(
    id = 1:10,
    rating = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)  # Predictions
  )
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  # Gold standard - same rankings, slightly different values
  gold <- data.frame(
    .id = 1:10,
    rating = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 5)  # Truth
  )

  # Validate with ordinal
  validation_ordinal <- qlm_validate(mock_coded, gold = gold, by = "rating", level = "ordinal")

  # Should have high correlations due to similar rankings
  rho <- get_metric(validation_ordinal, "rho")
  tau <- get_metric(validation_ordinal, "tau")
  mae <- get_metric(validation_ordinal, "mae")

  expect_true(is.numeric(rho))
  expect_true(is.numeric(tau))
  expect_true(rho > 0.8)
  expect_true(tau > 0.6)

  # MAE should be small
  expect_true(is.numeric(mae))
  expect_true(mae < 1.0)
})

test_that("qlm_validate nominal level computes all metrics", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(
    id = 1:20,
    category = rep(c("A", "B", "C"), length.out = 20)
  )
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:20),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 20),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  gold <- data.frame(
    .id = 1:20,
    category = rep(c("A", "B", "C"), length.out = 20)
  )

  # Validate with nominal level
  validation <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  expect_true(inherits(validation, "qlm_validation"))
  expect_true(all(validation$level == "nominal"))

  # Should have all metrics for nominal data
  expect_true(is.numeric(get_metric(validation, "accuracy")))
  expect_true(is.numeric(get_metric(validation, "precision")))
  expect_true(is.numeric(get_metric(validation, "recall")))
  expect_true(is.numeric(get_metric(validation, "f1")))
  expect_true(is.numeric(get_metric(validation, "kappa")))
})

test_that("qlm_validate prints appropriate terminology for ordinal vs nominal", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(rating = ellmer::type_integer("Rating"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, rating = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, rating = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))

  # Ordinal validation should have appropriate metrics in data frame
  validation_ordinal <- qlm_validate(mock_coded, gold = gold, by = "rating", level = "ordinal")
  expect_no_error(print(validation_ordinal))
  # Check metrics in data frame
  expect_true("rho" %in% validation_ordinal$measure)
  expect_true("tau" %in% validation_ordinal$measure)
  expect_true("mae" %in% validation_ordinal$measure)
  expect_true(all(validation_ordinal$level == "ordinal"))

  # Nominal validation should have different metrics
  validation_nominal <- qlm_validate(mock_coded, gold = gold, by = "rating", level = "nominal")
  expect_no_error(print(validation_nominal))
  expect_true("accuracy" %in% validation_nominal$measure)
  expect_true(all(validation_nominal$level == "nominal"))
})


test_that("qlm_validate accepts plain data.frames for both arguments", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  # Create two plain data.frames (simulating human coders)
  coder1 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))
  coder2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Should work with plain data.frames
  validation <- qlm_validate(coder1, gold = coder2, by = category, level = "nominal")

  expect_true(inherits(validation, "qlm_validation"))
  expect_equal(get_metric(validation, "accuracy"), 1.0)  # Perfect agreement
  expect_equal(attr(validation, "n"), 10)
})

test_that("qlm_validate works with plain data.frames and imperfect agreement", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  # Create two plain data.frames with different values
  coder1 <- data.frame(.id = 1:10, category = c(rep("A", 7), rep("B", 3)))
  coder2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  validation <- qlm_validate(coder1, gold = coder2, by = category, level = "nominal")

  expect_true(inherits(validation, "qlm_validation"))
  accuracy <- get_metric(validation, "accuracy")
  expect_true(accuracy < 1.0)
  expect_true(accuracy >= 0.5)
  expect_true(is.numeric(get_metric(validation, "precision")))
  expect_true(is.numeric(get_metric(validation, "recall")))
  expect_true(is.numeric(get_metric(validation, "f1")))
  expect_true(is.numeric(get_metric(validation, "kappa")))
})

test_that("qlm_validate supports non-standard evaluation for by argument", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("yardstick")

  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:10, category = rep(c("A", "B"), 5))
  mock_coded <- new_qlm_coded(
    results = mock_results,
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

  gold <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Test with unquoted variable name (NSE)
  validation_nse <- qlm_validate(mock_coded, gold = gold, by = category, level = "nominal")

  # Test with quoted variable name (traditional)
  validation_quoted <- qlm_validate(mock_coded, gold = gold, by = "category", level = "nominal")

  # Both should work and produce identical results
  expect_true(inherits(validation_nse, "qlm_validation"))
  expect_true(inherits(validation_quoted, "qlm_validation"))
  expect_equal(get_metric(validation_nse, "accuracy"), get_metric(validation_quoted, "accuracy"))
  expect_equal(get_metric(validation_nse, "precision"), get_metric(validation_quoted, "precision"))
  expect_equal(get_metric(validation_nse, "recall"), get_metric(validation_quoted, "recall"))
  expect_equal(get_metric(validation_nse, "f1"), get_metric(validation_quoted, "f1"))
  expect_equal(get_metric(validation_nse, "kappa"), get_metric(validation_quoted, "kappa"))
  expect_equal(attr(validation_nse, "n"), attr(validation_quoted, "n"))
  expect_equal(unique(validation_nse$variable), unique(validation_quoted$variable))
})
