test_that("qlm_trail() requires at least one object", {
  expect_error(
    qlm_trail(),
    "At least one object must be provided"
  )
})


test_that("qlm_trail() validates object types", {
  bad_obj <- list(foo = "bar")
  class(bad_obj) <- "not_a_quallmer_object"

  expect_error(
    qlm_trail(bad_obj),
    "All objects must be quallmer objects"
  )
})


test_that("qlm_trail() extracts single coded object info", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")

  attr(coded, "run") <- list(
    name = "run1",
    call = quote(qlm_code(data, codebook)),
    parent = NULL,
    metadata = list(
      timestamp = as.POSIXct("2024-01-01 12:00:00"),
      n_units = 3
    ),
    chat_args = list(name = "openai/gpt-4o"),
    codebook = list(name = "sentiment")
  )

  trail <- qlm_trail(coded)

  expect_s3_class(trail, "qlm_trail")
  expect_true(trail$complete)
  expect_length(trail$runs, 1)
  expect_equal(names(trail$runs)[1], "run1")
  expect_null(trail$runs[[1]]$parent)
})


test_that("qlm_trail() reconstructs chain from multiple objects", {
  coded1 <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded1) <- c("qlm_coded", "data.frame")
  attr(coded1, "run") <- list(
    name = "run1",
    call = quote(qlm_code(data, codebook)),
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00")),
    chat_args = list(name = "openai/gpt-4o"),
    codebook = list(name = "sentiment")
  )

  coded2 <- data.frame(.id = 1:3, polarity = c("pos", "pos", "pos"))
  class(coded2) <- c("qlm_coded", "data.frame")
  attr(coded2, "run") <- list(
    name = "run2",
    call = quote(qlm_replicate(coded1)),
    parent = "run1",
    metadata = list(timestamp = as.POSIXct("2024-01-01 13:00:00")),
    chat_args = list(name = "anthropic/claude-sonnet-4"),
    codebook = list(name = "sentiment")
  )

  trail <- qlm_trail(coded2, coded1)

  expect_s3_class(trail, "qlm_trail")
  expect_true(trail$complete)
  expect_length(trail$runs, 2)

  # Should be ordered parent first
  expect_equal(names(trail$runs), c("run1", "run2"))
  expect_null(trail$runs$run1$parent)
  expect_equal(trail$runs$run2$parent, "run1")
})


test_that("qlm_trail() handles incomplete chains", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run2",
    parent = "run1",  # Parent not in provided objects
    metadata = list(timestamp = as.POSIXct("2024-01-01 13:00:00")),
    chat_args = list(name = "openai/gpt-4o"),
    codebook = list(name = "sentiment")
  )

  trail <- qlm_trail(coded)

  expect_s3_class(trail, "qlm_trail")
  expect_false(trail$complete)  # Should be marked incomplete
  expect_length(trail$runs, 1)
})


test_that("qlm_trail() handles comparison objects with multiple parents", {
  coded1 <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded1) <- c("qlm_coded", "data.frame")
  attr(coded1, "run") <- list(
    name = "run1",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"))
  )

  coded2 <- data.frame(.id = 1:3, polarity = c("pos", "pos", "pos"))
  class(coded2) <- c("qlm_coded", "data.frame")
  attr(coded2, "run") <- list(
    name = "run2",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 13:00:00"))
  )

  comp <- list(measure = "alpha", value = 0.8)
  class(comp) <- "qlm_comparison"
  attr(comp, "run") <- list(
    name = "comparison_abc123",
    parent = c("run1", "run2"),
    metadata = list(timestamp = as.POSIXct("2024-01-01 14:00:00"))
  )

  trail <- qlm_trail(comp, coded1, coded2)

  expect_s3_class(trail, "qlm_trail")
  expect_true(trail$complete)
  expect_length(trail$runs, 3)

  expect_equal(trail$runs$comparison_abc123$parent, c("run1", "run2"))
})


test_that("qlm_trail() handles validation objects", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run1",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"))
  )

  valid <- list(accuracy = 0.9)
  class(valid) <- "qlm_validation"
  attr(valid, "run") <- list(
    name = "validation_xyz789",
    parent = "run1",
    metadata = list(timestamp = as.POSIXct("2024-01-01 14:00:00"))
  )

  trail <- qlm_trail(valid, coded)

  expect_s3_class(trail, "qlm_trail")
  expect_true(trail$complete)
  expect_length(trail$runs, 2)
  expect_equal(trail$runs$validation_xyz789$parent, "run1")
})


test_that("qlm_trail() handles NULL run names", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = NULL,  # Missing name
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"))
  )

  trail <- qlm_trail(coded)

  expect_s3_class(trail, "qlm_trail")
  # Should have generated a fallback name
  expect_equal(names(trail$runs)[1], "run_1")
})


test_that("print.qlm_trail() handles single run", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run1",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00")),
    chat_args = list(name = "openai/gpt-4o")
  )

  trail <- qlm_trail(coded)

  output <- capture.output(print(trail))
  expect_true(any(grepl("quallmer audit trail", output)))
  expect_true(any(grepl("run1", output)))
})


test_that("print.qlm_trail() handles multiple runs", {
  coded1 <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded1) <- c("qlm_coded", "data.frame")
  attr(coded1, "run") <- list(
    name = "run1",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00")),
    chat_args = list(name = "openai/gpt-4o"),
    codebook = list(name = "sentiment")
  )

  coded2 <- data.frame(.id = 1:3, polarity = c("pos", "pos", "pos"))
  class(coded2) <- c("qlm_coded", "data.frame")
  attr(coded2, "run") <- list(
    name = "run2",
    parent = "run1",
    metadata = list(timestamp = as.POSIXct("2024-01-01 13:00:00")),
    chat_args = list(name = "anthropic/claude-sonnet-4"),
    codebook = list(name = "sentiment")
  )

  trail <- qlm_trail(coded2, coded1)

  output <- capture.output(print(trail))
  expect_true(any(grepl("2 runs", output)))
  expect_true(any(grepl("run1", output)))
  expect_true(any(grepl("run2", output)))
  expect_true(any(grepl("original", output)))
  expect_true(any(grepl("parent: run1", output)))
})


test_that("print.qlm_trail() warns about incomplete chains", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run2",
    parent = "run1",  # Missing
    metadata = list(timestamp = as.POSIXct("2024-01-01 13:00:00"))
  )

  trail <- qlm_trail(coded)

  output <- capture.output(print(trail))
  expect_true(any(grepl("full chain", output)))
})


test_that("qlm_trail() handles complex branching workflow", {
  coded1 <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded1) <- c("qlm_coded", "data.frame")
  attr(coded1, "run") <- list(
    name = "run1",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"))
  )

  coded2 <- data.frame(.id = 1:3, polarity = c("pos", "pos", "pos"))
  class(coded2) <- c("qlm_coded", "data.frame")
  attr(coded2, "run") <- list(
    name = "run2",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 13:00:00"))
  )

  coded3 <- data.frame(.id = 1:3, polarity = c("neg", "neg", "pos"))
  class(coded3) <- c("qlm_coded", "data.frame")
  attr(coded3, "run") <- list(
    name = "run3",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 14:00:00"))
  )

  comp1 <- list(measure = "alpha", value = 0.8)
  class(comp1) <- "qlm_comparison"
  attr(comp1, "run") <- list(
    name = "comp1",
    parent = c("run1", "run2"),
    metadata = list(timestamp = as.POSIXct("2024-01-01 15:00:00"))
  )

  valid1 <- list(accuracy = 0.7)
  class(valid1) <- "qlm_validation"
  attr(valid1, "run") <- list(
    name = "valid1",
    parent = c("run3", "run1"),
    metadata = list(timestamp = as.POSIXct("2024-01-01 16:00:00"))
  )

  trail <- qlm_trail(coded1, coded2, coded3, comp1, valid1)

  expect_s3_class(trail, "qlm_trail")
  expect_true(trail$complete)
  expect_length(trail$runs, 5)

  expect_true("run1" %in% names(trail$runs))
  expect_true("run2" %in% names(trail$runs))
  expect_true("run3" %in% names(trail$runs))
  expect_true("comp1" %in% names(trail$runs))
  expect_true("valid1" %in% names(trail$runs))
})


# Tests for path parameter (saving)

test_that("qlm_trail() saves RDS and QMD when path provided", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run1",
    parent = NULL,
    call = quote(qlm_code(data, codebook)),
    metadata = list(
      timestamp = as.POSIXct("2024-01-01 12:00:00"),
      n_units = 3,
      quallmer_version = "0.2",
      ellmer_version = "0.4.0",
      R_version = "4.3.0"
    ),
    chat_args = list(name = "openai/gpt-4o"),
    codebook = list(name = "sentiment", instructions = "Code sentiment")
  )

  temp_dir <- tempdir()
  temp_path <- file.path(temp_dir, "test_trail")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  trail <- qlm_trail(coded, path = temp_path)

  # Check files were created
  expect_true(file.exists(paste0(temp_path, ".rds")))
  expect_true(file.exists(paste0(temp_path, ".qmd")))

  # Verify RDS content
  loaded <- readRDS(paste0(temp_path, ".rds"))
  expect_s3_class(loaded, "qlm_trail")
  expect_equal(loaded$runs, trail$runs)

  # Verify QMD content
  content <- readLines(paste0(temp_path, ".qmd"))
  expect_true(any(grepl("quallmer audit trail", content)))
  expect_true(any(grepl("Trail summary", content)))
  expect_true(any(grepl("Instrument development", content)))
  expect_true(any(grepl("Process notes", content)))
  expect_true(any(grepl("run1", content)))
})


test_that("qlm_trail() without path returns trail without saving", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run1",
    parent = NULL,
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"))
  )

  trail <- qlm_trail(coded)

  expect_s3_class(trail, "qlm_trail")
  # No files should be created - just returns trail object
})


test_that("qlm_trail() report includes comparison metrics", {
  coded1 <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded1) <- c("qlm_coded", "data.frame")
  attr(coded1, "run") <- list(name = "run1", parent = NULL)

  coded2 <- data.frame(.id = 1:3, polarity = c("pos", "pos", "pos"))
  class(coded2) <- c("qlm_coded", "data.frame")
  attr(coded2, "run") <- list(name = "run2", parent = NULL)

  comparison <- list(
    level = "nominal",
    subjects = 3,
    raters = 2,
    alpha_nominal = 0.85,
    kappa = 0.82,
    kappa_type = "Cohen's",
    percent_agreement = 0.90
  )
  class(comparison) <- "qlm_comparison"
  attr(comparison, "run") <- list(
    name = "comparison1",
    parent = c("run1", "run2")
  )

  temp_dir <- tempdir()
  temp_path <- file.path(temp_dir, "test_trail_comp")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  trail <- qlm_trail(coded1, coded2, comparison, path = temp_path)

  content <- readLines(paste0(temp_path, ".qmd"))

  # Check for comparison section
  expect_true(any(grepl("Data reconstruction", content)))
  expect_true(any(grepl("Comparisons", content)))
  expect_true(any(grepl("Krippendorff", content)))
  expect_true(any(grepl("0\\.85", content)))
})


test_that("qlm_trail() report includes validation metrics", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(name = "run1", parent = NULL)

  validation <- list(
    level = "nominal",
    n = 3,
    classes = c("pos", "neg"),
    average = "macro",
    accuracy = 0.90,
    precision = 0.88,
    recall = 0.85,
    f1 = 0.86,
    kappa = 0.80,
    rho = NULL,
    tau = NULL,
    r = NULL,
    icc = NULL,
    mae = NULL,
    rmse = NULL
  )
  class(validation) <- "qlm_validation"
  attr(validation, "run") <- list(
    name = "validation1",
    parent = "run1"
  )

  temp_dir <- tempdir()
  temp_path <- file.path(temp_dir, "test_trail_val")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  trail <- qlm_trail(coded, validation, path = temp_path)

  content <- readLines(paste0(temp_path, ".qmd"))

  expect_true(any(grepl("Data reconstruction", content)))
  expect_true(any(grepl("Validations", content)))
  expect_true(any(grepl("0\\.90", content)))  # accuracy
})


test_that("qlm_trail() handles multiple objects with path", {
  coded1 <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded1) <- c("qlm_coded", "data.frame")
  attr(coded1, "run") <- list(
    name = "run1",
    parent = NULL,
    call = quote(qlm_code(data, codebook))
  )

  coded2 <- data.frame(.id = 1:3, polarity = c("pos", "pos", "pos"))
  class(coded2) <- c("qlm_coded", "data.frame")
  attr(coded2, "run") <- list(
    name = "run2",
    parent = "run1",
    call = quote(qlm_replicate(coded1))
  )

  temp_dir <- tempdir()
  temp_path <- file.path(temp_dir, "test_trail_multi")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  trail <- qlm_trail(coded1, coded2, path = temp_path)

  expect_s3_class(trail, "qlm_trail")
  expect_length(trail$runs, 2)

  # Verify saved trail has both runs
  loaded <- readRDS(paste0(temp_path, ".rds"))
  expect_length(loaded$runs, 2)
})


test_that("qlm_trail() report includes codebook information", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "run1",
    parent = NULL,
    codebook = list(
      name = "Sentiment Codebook",
      instructions = "Code text as positive or negative"
    )
  )

  temp_dir <- tempdir()
  temp_path <- file.path(temp_dir, "test_trail_cb")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  trail <- qlm_trail(coded, path = temp_path)

  content <- readLines(paste0(temp_path, ".qmd"))

  expect_true(any(grepl("Instrument development", content)))
  expect_true(any(grepl("Sentiment Codebook", content)))
  expect_true(any(grepl("positive or negative", content)))
})
