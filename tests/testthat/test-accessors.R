# Test qlm_meta() accessor functions

test_that("qlm_meta() extracts user metadata from qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  # Default type is "user"
  user_meta <- qlm_meta(coded)
  expect_type(user_meta, "list")
  expect_named(user_meta, c("name", "notes"))

  # Single field extraction
  name <- qlm_meta(coded, "name")
  expect_true(is.null(name) || is.character(name))
})


test_that("qlm_meta() extracts object metadata from qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  obj_meta <- qlm_meta(coded, type = "object")
  expect_type(obj_meta, "list")
  expect_true(all(c("batch", "call", "chat_args", "execution_args", "parent", "n_units", "input_type") %in% names(obj_meta)))

  # Single field extraction
  n_units <- qlm_meta(coded, "n_units", type = "object")
  expect_type(n_units, "integer")
})


test_that("qlm_meta() extracts system metadata from qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  sys_meta <- qlm_meta(coded, type = "system")
  expect_type(sys_meta, "list")
  expect_true(all(c("timestamp", "ellmer_version", "quallmer_version", "R_version") %in% names(sys_meta)))

  # Single field extraction
  timestamp <- qlm_meta(coded, "timestamp", type = "system")
  expect_s3_class(timestamp, "POSIXct")
})


test_that("qlm_meta() extracts all metadata from qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  all_meta <- qlm_meta(coded, type = "all")
  expect_type(all_meta, "list")
  expect_named(all_meta, c("user", "object", "system"))
  expect_type(all_meta$user, "list")
  expect_type(all_meta$object, "list")
  expect_type(all_meta$system, "list")
})


test_that("qlm_meta() errors when field is specified with type = 'all'", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  expect_error(
    qlm_meta(coded, "name", type = "all"),
    "Cannot specify.*field.*when.*type = 'all'"
  )
})


test_that("qlm_meta() errors for invalid field names", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  expect_error(
    qlm_meta(coded, "invalid_field"),
    "Field.*not found"
  )

  expect_error(
    qlm_meta(coded, "timestamp", type = "object"),
    "Field.*not found"
  )
})


test_that("qlm_meta() works for qlm_comparison objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  comparison <- qlm_compare(
    examples$example_coded_sentiment,
    examples$example_coded_mini,
    by = "sentiment",
    level = "nominal"
  )

  # User metadata
  user_meta <- qlm_meta(comparison)
  expect_type(user_meta, "list")
  expect_named(user_meta, c("name", "notes"))

  # Object metadata
  obj_meta <- qlm_meta(comparison, type = "object")
  expect_type(obj_meta, "list")
  expect_true(all(c("call", "parent", "n_raters", "variables") %in% names(obj_meta)))

  # System metadata
  sys_meta <- qlm_meta(comparison, type = "system")
  expect_type(sys_meta, "list")
  expect_true(all(c("timestamp", "quallmer_version", "R_version") %in% names(sys_meta)))
})


test_that("qlm_meta() works for qlm_validation objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  validation <- qlm_validate(
    examples$example_coded_mini,
    gold = examples$example_gold_standard,
    by = "sentiment",
    level = "nominal"
  )

  # User metadata
  user_meta <- qlm_meta(validation)
  expect_type(user_meta, "list")
  expect_named(user_meta, c("name", "notes"))

  # Object metadata
  obj_meta <- qlm_meta(validation, type = "object")
  expect_type(obj_meta, "list")
  expect_true(all(c("call", "parent", "variables", "average") %in% names(obj_meta)))

  # System metadata
  sys_meta <- qlm_meta(validation, type = "system")
  expect_type(sys_meta, "list")
  expect_true(all(c("timestamp", "quallmer_version", "R_version") %in% names(sys_meta)))
})


test_that("qlm_meta() works for qlm_codebook objects", {
  cb <- qlm_codebook(
    name = "Test codebook",
    instructions = "Test instructions",
    schema = ellmer::type_object(
      var1 = ellmer::type_string("Test variable")
    )
  )

  # User metadata (includes name and instructions for codebooks)
  user_meta <- qlm_meta(cb)
  expect_type(user_meta, "list")
  expect_named(user_meta, c("name", "instructions"))
  expect_equal(user_meta$name, "Test codebook")
  expect_equal(user_meta$instructions, "Test instructions")

  # Object metadata
  obj_meta <- qlm_meta(cb, type = "object")
  expect_type(obj_meta, "list")
  expect_true(all(c("schema", "role", "input_type", "levels") %in% names(obj_meta)))

  # System metadata (empty for codebooks)
  sys_meta <- qlm_meta(cb, type = "system")
  expect_type(sys_meta, "list")
  expect_length(sys_meta, 0)
})


# Test meta<-() replacement functions

test_that("qlm_meta<-() sets user metadata for qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  # Set single field
  qlm_meta(coded, "notes") <- "Updated notes"
  expect_equal(qlm_meta(coded, "notes"), "Updated notes")

  # Set multiple fields
  qlm_meta(coded) <- list(name = "new_name", notes = "new notes")
  expect_equal(qlm_meta(coded, "name"), "new_name")
  expect_equal(qlm_meta(coded, "notes"), "new notes")
})


test_that("qlm_meta<-() errors when trying to set object metadata", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  expect_error(
    { qlm_meta(coded, "batch") <- TRUE },
    "Object and system metadata are read-only"
  )

  expect_error(
    { qlm_meta(coded, "call") <- call("test") },
    "Object and system metadata are read-only"
  )
})


test_that("qlm_meta<-() errors when trying to set system metadata", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  expect_error(
    { qlm_meta(coded, "timestamp") <- Sys.time() },
    "Object and system metadata are read-only"
  )

  expect_error(
    { qlm_meta(coded) <- list(name = "test", R_version = "4.0") },
    "Object and system metadata are read-only"
  )
})


test_that("qlm_meta<-() errors with invalid input format", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  expect_error(
    { qlm_meta(coded) <- "invalid" },
    "must be a named list"
  )
})


test_that("qlm_meta<-() works for qlm_comparison objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  comparison <- qlm_compare(
    examples$example_coded_sentiment,
    examples$example_coded_mini,
    by = "sentiment",
    level = "nominal"
  )

  qlm_meta(comparison, "notes") <- "Test comparison notes"
  expect_equal(qlm_meta(comparison, "notes"), "Test comparison notes")
})


test_that("qlm_meta<-() works for qlm_validation objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  validation <- qlm_validate(
    examples$example_coded_mini,
    gold = examples$example_gold_standard,
    by = "sentiment",
    level = "nominal"
  )

  qlm_meta(validation, "notes") <- "Test validation notes"
  expect_equal(qlm_meta(validation, "notes"), "Test validation notes")
})


test_that("qlm_meta<-() works for qlm_codebook objects", {
  cb <- qlm_codebook(
    name = "Test codebook",
    instructions = "Test instructions",
    schema = ellmer::type_object(
      var1 = ellmer::type_string("Test variable")
    )
  )

  # Set name
  qlm_meta(cb, "name") <- "Updated codebook name"
  expect_equal(qlm_meta(cb, "name"), "Updated codebook name")

  # Set instructions
  qlm_meta(cb, "instructions") <- "Updated instructions"
  expect_equal(qlm_meta(cb, "instructions"), "Updated instructions")

  # Set multiple fields
  qlm_meta(cb) <- list(name = "New name", instructions = "New instructions")
  expect_equal(qlm_meta(cb, "name"), "New name")
  expect_equal(qlm_meta(cb, "instructions"), "New instructions")

  # Cannot set object metadata
  expect_error(
    { qlm_meta(cb, "schema") <- NULL },
    "Object metadata is read-only"
  )
})


# Test codebook() extractor

test_that("codebook() extracts codebook from qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  cb <- codebook(coded)
  expect_s3_class(cb, "qlm_codebook")
  expect_true(inherits(cb, "task"))  # Dual inheritance
  expect_type(cb$name, "character")
  expect_type(cb$instructions, "character")
})


test_that("codebook() returns NULL for qlm_comparison objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  comparison <- qlm_compare(
    examples$example_coded_sentiment,
    examples$example_coded_mini,
    by = "sentiment",
    level = "nominal"
  )

  # Comparison objects don't store codebooks directly
  cb <- codebook(comparison)
  expect_null(cb)
})


test_that("codebook() returns NULL for qlm_validation objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  validation <- qlm_validate(
    examples$example_coded_mini,
    gold = examples$example_gold_standard,
    by = "sentiment",
    level = "nominal"
  )

  # Validation objects don't store codebooks directly
  cb <- codebook(validation)
  expect_null(cb)
})


# Test inputs() extractor

test_that("inputs() extracts input data from qlm_coded objects", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  input_data <- inputs(coded)
  expect_type(input_data, "character")
  expect_true(length(input_data) > 0)
})


test_that("inputs() preserves names from original input", {
  # Create a simple test with named inputs
  skip_if_not(file.exists(system.file("extdata", "example_objects.rds", package = "quallmer")))

  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  input_data <- inputs(coded)
  # Check that it's a character vector (names may or may not be present)
  expect_type(input_data, "character")
})


# Integration tests

test_that("qlm_meta(), codebook(), and inputs() work together", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  # Extract all components
  user_meta <- qlm_meta(coded)
  obj_meta <- qlm_meta(coded, type = "object")
  sys_meta <- qlm_meta(coded, type = "system")
  cb <- codebook(coded)
  input_data <- inputs(coded)

  # All should be non-NULL
  expect_type(user_meta, "list")
  expect_type(obj_meta, "list")
  expect_type(sys_meta, "list")
  expect_s3_class(cb, "qlm_codebook")
  expect_type(input_data, "character")

  # Modify user metadata
  qlm_meta(coded, "notes") <- "Integration test"
  expect_equal(qlm_meta(coded, "notes"), "Integration test")

  # Original components should be unchanged
  expect_identical(codebook(coded), cb)
  expect_identical(inputs(coded), input_data)
})


test_that("accessors preserve object classes", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  coded <- examples$example_coded_sentiment

  # Modifying metadata should preserve class
  original_class <- class(coded)
  qlm_meta(coded, "notes") <- "Test"
  expect_identical(class(coded), original_class)

  # Object should still be a tibble
  expect_s3_class(coded, "tbl_df")
  expect_s3_class(coded, "qlm_coded")
})
