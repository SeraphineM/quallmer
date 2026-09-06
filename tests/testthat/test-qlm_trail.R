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

  # A realistic long-format qlm_comparison tibble (matches qlm_compare() output)
  comparison <- tibble::tibble(
    variable = "polarity",
    level    = "nominal",
    measure  = c("percent_agreement", "alpha_nominal", "kappa"),
    value    = c(0.90, 0.85, 0.82),
    rater1   = "run1",
    rater2   = "run2"
  )
  class(comparison) <- c("qlm_comparison", class(comparison))
  attr(comparison, "raters") <- 2L
  attr(comparison, "n") <- 3L
  attr(comparison, "run") <- list(
    name = "comparison1",
    parent = c("run1", "run2"),
    metadata = list(n_raters = 2L, variables = "polarity")
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
  expect_true(any(grepl("0\\.8500", content)))
})


test_that("qlm_trail() report includes validation metrics", {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(name = "run1", parent = NULL)

  # A realistic long-format qlm_validation tibble (matches qlm_validate() output)
  validation <- tibble::tibble(
    variable = "polarity",
    level    = "nominal",
    measure  = c("accuracy", "precision", "recall", "f1", "kappa"),
    value    = c(0.90, 0.88, 0.85, 0.86, 0.80),
    class    = NA_character_,
    rater    = "run1"
  )
  class(validation) <- c("qlm_validation", class(validation))
  attr(validation, "n") <- 3L
  attr(validation, "run") <- list(
    name = "validation1",
    parent = "run1",
    metadata = list(variables = "polarity", average = "macro")
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
  expect_true(any(grepl("0\\.9000", content)))  # accuracy
})


# Regression test for issue #93: trail must accept real qlm_compare() and
# qlm_validate() output without warnings or fatal errors.
test_that("qlm_trail() accepts real qlm_comparison and qlm_validation objects (#93)", {
  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))

  # In-memory trail with real comparison + validation
  expect_no_warning(
    expect_no_error(
      trail <- qlm_trail(
        examples$example_comparison,
        examples$example_validation,
        examples$example_coded_sentiment,
        examples$example_coded_mini,
        examples$example_gold_standard
      )
    )
  )
  expect_s3_class(trail, "qlm_trail")

  # Stored objects must round-trip with classes and metadata intact
  comp_stored <- trail$runs[[which(vapply(trail$runs,
                                          function(r) !is.null(r$comparison),
                                          logical(1)))]]$comparison
  expect_s3_class(comp_stored, "qlm_comparison")
  expect_identical(attr(comp_stored, "n"), attr(examples$example_comparison, "n"))

  val_stored <- trail$runs[[which(vapply(trail$runs,
                                         function(r) !is.null(r$validation),
                                         logical(1)))]]$validation
  expect_s3_class(val_stored, "qlm_validation")

  # Saved trail report must render without the "condition has length > 1" crash
  temp_path <- tempfile("trail_issue93")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })
  expect_no_error(
    qlm_trail(
      examples$example_comparison,
      examples$example_validation,
      examples$example_coded_sentiment,
      examples$example_coded_mini,
      examples$example_gold_standard,
      path = temp_path
    )
  )

  content <- readLines(paste0(temp_path, ".qmd"))
  expect_true(any(grepl("Krippendorff", content)))
  expect_true(any(grepl("Cohen's kappa|kappa", content)))
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


# ---- sampling parameters in the trail (#127) --------------------------------

# Minimum coded object for which generate_trail_report() emits both a metadata
# block and a replication block for one run.
trail_params_fixture <- function(chat_args, name = "run1") {
  coded <- data.frame(.id = 1:3, polarity = c("pos", "neg", "pos"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = name,
    parent = NULL,
    call = quote(qlm_code(data, codebook)),
    metadata = list(
      timestamp = as.POSIXct("2024-01-01 12:00:00"),
      n_units = 3
    ),
    chat_args = chat_args,
    codebook = list(name = "sentiment", instructions = "Code sentiment")
  )
  coded
}

trail_params_report <- function(chat_args, name = "run1") {
  path <- file.path(tempdir(), "test_trail_params")
  withr::defer_parent(unlink(paste0(path, c(".rds", ".qmd"))))
  qlm_trail(trail_params_fixture(chat_args, name), path = path)
  readLines(paste0(path, ".qmd"))
}

# The generated block is only useful if the sampling settings land where
# qlm_code() actually reads them, so assert on the parsed call rather than on
# the text: correct output contains "temperature =" too, nested inside
# ellmer::params().
qlm_code_call <- function(content, run_name = "run1") {
  start <- grep(paste0("^#### Replicate: ", run_name, "$"), content)[1]
  fences <- grep("^```", content[start:length(content)]) + start - 1L
  block <- content[(fences[1] + 1L):(fences[2] - 1L)]
  exprs <- as.list(parse(text = paste(block, collapse = "\n")))
  hits <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("qlm_code"))
  }, exprs)
  as.list(hits[[1]][[3]])[-1]
}


test_that("qlm_trail() report records the image resolution a codebook codes at (#177)", {
  coded <- trail_params_fixture(list(name = "openai/gpt-4o"))
  attr(coded, "run")$codebook <- list(
    name = "posters", instructions = "Read the posters",
    input_type = "image", image_file_resize = "1024x1024>",
    image_url_detail = "high"
  )
  path <- file.path(tempdir(), "test_trail_resize")
  withr::defer(unlink(paste0(path, c(".rds", ".qmd"))))
  qlm_trail(coded, path = path)
  content <- readLines(paste0(path, ".qmd"))

  expect_true(any(grepl(
    '**Input:** image files, resized with `image_file_resize = "1024x1024>"`; image URLs with `image_url_detail = "high"`',
    content, fixed = TRUE
  )))

  # A run saved before the field existed was coded at "low", and says so
  attr(coded, "run")$codebook$image_file_resize <- NULL
  attr(coded, "run")$codebook$image_url_detail <- NULL
  qlm_trail(coded, path = path)
  content <- readLines(paste0(path, ".qmd"))
  expect_true(any(grepl('`image_file_resize = "low"`; image URLs with `image_url_detail = "auto"`', content, fixed = TRUE)))

  # A text codebook has no such line
  content <- trail_params_report(list(name = "openai/gpt-4o"))
  expect_false(any(grepl("**Input:**", content, fixed = TRUE)))
})


test_that("qlm_trail() report keeps apart same-named codebooks that differ in image settings (#177)", {
  low <- trail_params_fixture(list(name = "openai/gpt-4o"), name = "low_run")
  attr(low, "run")$codebook <- list(
    name = "posters", instructions = "Read the posters",
    input_type = "image", image_file_resize = "low", image_url_detail = "auto"
  )
  high <- trail_params_fixture(list(name = "openai/gpt-4o"), name = "high_run")
  attr(high, "run")$codebook <- list(
    name = "posters", instructions = "Read the posters",
    input_type = "image", image_file_resize = "high", image_url_detail = "auto"
  )
  path <- file.path(tempdir(), "test_trail_variants")
  withr::defer(unlink(paste0(path, c(".rds", ".qmd"))))
  qlm_trail(low, high, path = path)
  content <- readLines(paste0(path, ".qmd"))

  # Both settings are reported, under the instrument and under each run
  expect_length(grep('image_file_resize = "low"', content, fixed = TRUE), 2L)
  expect_length(grep('image_file_resize = "high"', content, fixed = TRUE), 2L)
  expect_true(any(grepl("^### posters$", content)))
  expect_true(any(grepl("^### posters \\(variant 2\\)$", content)))
  # and each run's own line follows its codebook reference
  run_lines <- grep("^\\*\\*Codebook:\\*\\* posters$", content)
  expect_length(run_lines, 2L)
  expect_true(grepl('"low"', content[run_lines[1] + 1L], fixed = TRUE))
  expect_true(grepl('"high"', content[run_lines[2] + 1L], fixed = TRUE))

  # Identical codebooks under two runs are still one entry
  again <- trail_params_fixture(list(name = "openai/gpt-4o"), name = "low_again")
  attr(again, "run")$codebook <- attr(low, "run")$codebook
  path2 <- file.path(tempdir(), "test_trail_same_codebook")
  withr::defer(unlink(paste0(path2, c(".rds", ".qmd"))))
  qlm_trail(low, again, path = path2)
  content <- readLines(paste0(path2, ".qmd"))
  expect_false(any(grepl("(variant ", content, fixed = TRUE)))
  expect_length(grep("^### posters$", content), 1L)
  expect_length(grep("^\\*\\*Codebook:\\*\\* posters$", content), 2L)
})


test_that("qlm_trail() report reads sampling settings from chat_args$params (#127)", {
  content <- trail_params_report(list(
    name = "openai/gpt-4o-mini",
    params = list(temperature = 0, top_p = 0.95)
  ))

  expect_true(any(grepl(
    "**Parameters:** temperature = 0, top_p = 0.95",
    content, fixed = TRUE
  )))

  args <- qlm_code_call(content)
  expect_false("temperature" %in% names(args))
  expect_true("params" %in% names(args))
  expect_equal(deparse(args$params[[1]]), "ellmer::params")

  pargs <- as.list(args$params)[-1]
  expect_equal(pargs$temperature, 0)
  expect_equal(pargs$top_p, 0.95)
})


test_that("qlm_trail() normalises a legacy chat_args$temperature into params (#127)", {
  content <- trail_params_report(list(
    name = "openai/gpt-4o-mini",
    temperature = 0.3
  ))

  # The obsolete form is read but never shown or emitted.
  expect_false(any(grepl("**Temperature:**", content, fixed = TRUE)))
  expect_true(any(grepl("**Parameters:** temperature = 0.3", content, fixed = TRUE)))

  args <- qlm_code_call(content)
  expect_false("temperature" %in% names(args))
  pargs <- as.list(args$params)[-1]
  expect_equal(pargs$temperature, 0.3)
})


test_that("qlm_trail() prefers params over a legacy temperature (#127)", {
  content <- trail_params_report(list(
    name = "openai/gpt-4o-mini",
    params = list(temperature = 0),
    temperature = 0.9
  ))

  pargs <- as.list(qlm_code_call(content)$params)[-1]
  expect_equal(pargs$temperature, 0)
})


test_that("qlm_trail() serialises parameter values one at a time (#127)", {
  content <- trail_params_report(list(
    name = "openai/gpt-4o-mini",
    params = list(
      temperature = 0,
      label = 'a "quoted" phrase',
      stop = c("END", "STOP")
    )
  ))

  # unlist() would flatten the vector into separate entries here.
  expect_true(any(grepl('stop = c("END", "STOP")', content, fixed = TRUE)))

  pargs <- as.list(qlm_code_call(content)$params)[-1]
  expect_equal(eval(pargs$label), 'a "quoted" phrase')
  expect_equal(eval(pargs$stop), c("END", "STOP"))
})


test_that("qlm_trail() omits parameters entirely when a run recorded none (#127)", {
  content <- trail_params_report(list(name = "openai/gpt-4o-mini"))

  expect_false(any(grepl("**Parameters:**", content, fixed = TRUE)))
  expect_false("params" %in% names(qlm_code_call(content)))
})


test_that("qlm_trail() reproducibility advice uses the form that works (#127)", {
  content <- trail_params_report(list(name = "openai/gpt-4o-mini"))

  expect_true(any(grepl(
    "Use `params = ellmer::params(temperature = 0)` for more deterministic",
    content, fixed = TRUE
  )))
})


# ---- provider and endpoint setup section (#130) -----------------------------

endpoint_fixture <- function(name, base_url = NULL, run = "run1") {
  coded <- data.frame(.id = 1:2, polarity = c("pos", "neg"))
  class(coded) <- c("qlm_coded", "data.frame")
  chat_args <- list(name = name)
  if (!is.null(base_url)) chat_args$base_url <- base_url
  attr(coded, "run") <- list(
    name = run, parent = NULL, call = quote(qlm_code(x, cb)),
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"), n_units = 2),
    chat_args = chat_args,
    codebook = list(name = "sentiment", instructions = "Code sentiment")
  )
  coded
}

setup_section <- function(...) {
  path <- file.path(tempdir(), "test_trail_endpoints")
  withr::defer_parent(unlink(paste0(path, c(".rds", ".qmd"))))
  qlm_trail(..., path = path)
  content <- readLines(paste0(path, ".qmd"))
  start <- grep("^### Provider and endpoint setup$", content)
  if (length(start) == 0) return(character(0))
  rest <- content[start:length(content)]
  rest[seq_len(grep("^### ", rest)[2] - 1L)]
}


test_that("endpoints on the same host but different ports stay distinct (#130)", {
  section <- setup_section(
    endpoint_fixture("vllm/a", "http://localhost:8000/v1", "one"),
    endpoint_fixture("vllm/b", "http://localhost:1234/v1", "two")
  )

  expect_true(any(grepl("localhost:8000/v1", section, fixed = TRUE)))
  expect_true(any(grepl("localhost:1234/v1", section, fixed = TRUE)))
  expect_equal(sum(grepl("^# vllm at", section)), 2)
})


test_that("endpoints on the same host but different paths stay distinct (#130)", {
  section <- setup_section(
    endpoint_fixture("openai_compatible/a", "https://gw.example.com/team-a/v1", "one"),
    endpoint_fixture("openai_compatible/b", "https://gw.example.com/team-b/v1", "two")
  )

  expect_true(any(grepl("gw.example.com/team-a/v1", section, fixed = TRUE)))
  expect_true(any(grepl("gw.example.com/team-b/v1", section, fixed = TRUE)))
  expect_equal(sum(grepl("^# openai_compatible at", section)), 2)
})


test_that("OpenAI-compatible endpoints are no longer collapsed into one (#130)", {
  # The worst of the three symptoms: Qwen and Kimi both arrive as
  # `openai_compatible`, and the report used to name only that prefix.
  section <- setup_section(
    endpoint_fixture("openai_compatible/qwen3-max",
                     "https://dashscope-intl.aliyuncs.com/compatible-mode/v1", "one"),
    endpoint_fixture("openai_compatible/kimi-k3", "https://api.moonshot.ai/v1", "two")
  )

  expect_true(any(grepl("dashscope-intl.aliyuncs.com", section, fixed = TRUE)))
  expect_true(any(grepl("api.moonshot.ai", section, fixed = TRUE)))
})


test_that("a credential in the URL does not appear in the endpoint label (#130)", {
  # Scoped to the label. The recorded call and chat_args are redacted when
  # the trail is built (#154); this asserts only what this section controls.
  section <- setup_section(endpoint_fixture(
    "openai_compatible/m", "https://user:tok3n@api.example.com/v1?api_key=abc#frag"
  ))

  expect_false(any(grepl("tok3n", section, fixed = TRUE)))
  expect_false(any(grepl("api_key=abc", section, fixed = TRUE)))
  expect_false(any(grepl("user:", section, fixed = TRUE)))
  expect_false(any(grepl("#frag", section, fixed = TRUE)))
  expect_true(any(grepl("api.example.com/v1", section, fixed = TRUE)))
})


test_that("Ollama is only told it needs no key when the endpoint is local (#130)", {
  # ellmer reads OLLAMA_API_KEY through ollama_credentials() and takes
  # OLLAMA_BASE_URL, so a blanket "running locally" is wrong behind a proxy.
  local_section <- setup_section(
    endpoint_fixture("ollama/llama3.2", "http://localhost:11434")
  )
  expect_true(any(grepl("no API key for a local endpoint", local_section, fixed = TRUE)))
  expect_true(any(grepl("?ellmer::chat_ollama", local_section, fixed = TRUE)))

  remote_section <- setup_section(
    endpoint_fixture("ollama/llama3.2", "https://ollama.corp.example.com")
  )
  expect_false(any(grepl("no API key", remote_section, fixed = TRUE)))
  expect_true(any(grepl("?ellmer::chat_ollama", remote_section, fixed = TRUE)))
})


test_that("an unrecorded endpoint is not claimed to be local (#130)", {
  # ellmer resolves Ollama's default to Sys.getenv("OLLAMA_BASE_URL",
  # "http://localhost:11434"), so an absent base_url may well have been a
  # remote server. Claiming no key is needed there would reproduce exactly the
  # stale guidance this replaced.
  absent_section <- setup_section(endpoint_fixture("ollama/llama3.2"))

  expect_false(any(grepl("no API key", absent_section, fixed = TRUE)))
  expect_true(any(grepl("?ellmer::chat_ollama", absent_section, fixed = TRUE)))
  expect_false(is_local_endpoint(NA_character_))
})


test_that("IPv6 loopback counts as local, with or without a port (#130)", {
  # Unbracketing before stripping the port turned `::1` into `:`, so neither
  # form was recognised.
  expect_true(is_local_endpoint("http://[::1]:11434"))
  expect_true(is_local_endpoint("http://[::1]"))
  expect_true(is_local_endpoint("http://[::1]/v1"))

  # Link-local is not loopback.
  expect_false(is_local_endpoint("http://[fe80::1]:11434"))

  # And the IPv4 forms still work.
  expect_true(is_local_endpoint("http://localhost:11434"))
  expect_true(is_local_endpoint("http://127.0.0.1"))
  expect_false(is_local_endpoint("http://localhost.evil.com"))
})


test_that("every ellmer provider gets a real pointer, not a generic non-answer (#130)", {
  section <- setup_section(
    endpoint_fixture("google_gemini/gemini-2.5-flash", run = "one"),
    endpoint_fixture("deepseek/deepseek-chat", run = "two"),
    endpoint_fixture("aws_bedrock/claude", run = "three")
  )

  expect_true(any(grepl("?ellmer::chat_google_gemini", section, fixed = TRUE)))
  expect_true(any(grepl("?ellmer::chat_deepseek", section, fixed = TRUE)))
  expect_true(any(grepl("?ellmer::chat_aws_bedrock", section, fixed = TRUE)))
  expect_false(any(grepl("Configure API credentials as needed", section, fixed = TRUE)))
})


test_that("a provider the installed ellmer lacks gets no broken help link (#130)", {
  # `google` is the shape of a trail recorded before qlm_code() began
  # rejecting prefixes ellmer cannot dispatch on. `?ellmer::chat_google` does
  # not exist, so it must not be offered.
  section <- setup_section(endpoint_fixture("google/gemini-2.5-flash"))

  expect_false(any(grepl("?ellmer::chat_google", section, fixed = TRUE)))
  expect_true(any(grepl("not one the installed ellmer offers", section, fixed = TRUE)))
  expect_true(any(grepl("ellmer.tidyverse.org/reference/index.html", section, fixed = TRUE)))
})


test_that("a missing or unusable base_url degrades to the provider alone (#130)", {
  for (value in list(NULL, NA_character_, "", 42, c("a", "b"))) {
    section <- setup_section(endpoint_fixture("openai/gpt-4o-mini", base_url = value))
    expect_true(any(grepl("^# openai$", section)), info = paste(class(value), length(value)))
    expect_false(any(grepl(" at NA", section, fixed = TRUE)))
    expect_true(any(grepl("?ellmer::chat_openai", section, fixed = TRUE)))
  }
})


test_that("a run with no recorded model name does not break the section (#130)", {
  coded <- data.frame(.id = 1:2, polarity = c("pos", "neg"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = "nameless", parent = NULL, call = quote(qlm_code(x, cb)),
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"), n_units = 2),
    chat_args = list(),
    codebook = list(name = "sentiment", instructions = "Code sentiment")
  )

  # qlm_trail() reports where it saved, so silence is not the assertion here.
  expect_no_error(section <- setup_section(coded))
  expect_length(section, 0)
})


# ---- credentials never reach the trail (#154) --------------------------------

credential_fixture <- function(run = "run1") {
  coded <- data.frame(.id = 1:2, polarity = c("pos", "neg"))
  class(coded) <- c("qlm_coded", "data.frame")
  attr(coded, "run") <- list(
    name = run, parent = NULL,
    call = quote(qlm_code(
      x, cb, model = "openai_compatible/m",
      base_url = "https://user:s3cret@host/v1?api_key=qs3cret&api-version=2024",
      api_key = "sk-lit3ral",
      api_headers = c(Authorization = "Bearer hdr3cret", `anthropic-beta` = "tools-2024"),
      credentials = function() "cb3cret"
    )),
    metadata = list(timestamp = as.POSIXct("2024-01-01 12:00:00"), n_units = 2),
    chat_args = list(
      name = "openai_compatible/m",
      base_url = "https://user:s3cret@host/v1?api_key=qs3cret&api-version=2024",
      api_key = "sk-lit3ral",
      api_headers = c(Authorization = "Bearer hdr3cret", `anthropic-beta` = "tools-2024"),
      credentials = local({ captured <- "env3cret"; function() "cb3cret" })
    ),
    codebook = list(name = "sentiment", instructions = "Code sentiment")
  )
  coded
}

secrets <- c("s3cret", "qs3cret", "sk-lit3ral", "hdr3cret", "cb3cret", "env3cret")

expect_no_secret <- function(text) {
  for (secret in secrets) {
    expect_false(any(grepl(secret, text, fixed = TRUE)), label = secret)
  }
}

test_that("qlm_trail() writes no credential into the report or the .rds (#154)", {
  path <- file.path(tempdir(), "test_trail_credentials")
  withr::defer(unlink(paste0(path, c(".rds", ".qmd"))))

  expect_message(
    qlm_trail(credential_fixture(), path = path),
    "Credential values recorded for \"run1\""
  )

  report <- readLines(paste0(path, ".qmd"))
  expect_no_secret(report)
  # The call is still there, with the shape of what was passed
  expect_true(any(grepl('api_key = "<redacted>"', report, fixed = TRUE)))
  expect_true(any(grepl('Authorization = "<redacted>"', report, fixed = TRUE)))
  expect_true(any(grepl("api-version=2024", report, fixed = TRUE)))
  expect_true(any(grepl('`anthropic-beta` = "tools-2024"', report, fixed = TRUE)))

  saved <- readRDS(paste0(path, ".rds"))
  run <- saved$runs[[1]]
  expect_no_secret(deparse(run$call))
  expect_no_secret(unlist(run$chat_args[c("base_url", "api_key", "api_headers")]))
  # The mirror and the object it mirrors agree
  meta <- attr(run$coded, "meta")$object
  expect_identical(meta$chat_args[c("base_url", "api_key", "api_headers")],
                   run$chat_args[c("base_url", "api_key", "api_headers")])
  expect_identical(meta$call, run$call)
  # A callback that returns a literal is gone, and so is its environment:
  # the whole file holds neither the literal nor what the closure captured
  expect_equal(run$chat_args$credentials, "<redacted>")
  expect_true(any(grepl('credentials = "<redacted>"', report, fixed = TRUE)))
  rds_file <- paste0(path, ".rds")
  bytes <- memDecompress(readBin(rds_file, "raw", file.size(rds_file)), "gzip")
  for (secret in secrets) {
    expect_length(grepRaw(secret, bytes, fixed = TRUE), 0)
  }
})

test_that("a Sys.getenv() credentials callback survives the trail (#154)", {
  coded <- credential_fixture()
  attr(coded, "run")$chat_args$credentials <- function() Sys.getenv("MY_KEY")
  attr(coded, "run")$call <- quote(qlm_code(x, cb, credentials = function() Sys.getenv("MY_KEY")))
  trail <- suppressMessages(qlm_trail(coded))
  run <- trail$runs[[1]]
  expect_true(is.function(run$chat_args$credentials))
  expect_identical(body(run$chat_args$credentials), quote(Sys.getenv("MY_KEY")))
  expect_identical(run$call, quote(qlm_code(x, cb, credentials = function() Sys.getenv("MY_KEY"))))
})

test_that("the returned trail is redacted like the files (#154)", {
  trail <- suppressMessages(qlm_trail(credential_fixture()))
  run <- trail$runs[[1]]
  expect_no_secret(deparse(run$call))
  expect_no_secret(unlist(run$chat_args[c("base_url", "api_key", "api_headers")]))
  expect_equal(run$chat_args$api_key, "<redacted>")
  expect_equal(run$chat_args$base_url, "https://host/v1?api_key=<redacted>&api-version=2024")
})

test_that("a run with nothing to redact is untouched and gets no message (#154)", {
  coded <- endpoint_fixture("openai/gpt-4o", "https://api.example.com/v1?api-version=2024")
  expect_silent(trail <- qlm_trail(coded))
  run <- trail$runs[[1]]
  expect_identical(run$call, quote(qlm_code(x, cb)))
  expect_identical(run$chat_args$base_url, "https://api.example.com/v1?api-version=2024")
  expect_identical(attr(run$coded, "meta")$object$chat_args, run$chat_args)
})

test_that("redaction names every affected run once (#154)", {
  expect_message(
    qlm_trail(credential_fixture("one"), credential_fixture("two")),
    "\"one\" and \"two\""
  )
})


# cost provenance (#135) -------------------------------------------------------

test_that("qlm_trail() report states where a cost came from and reproduces the rates (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1:2, score = c(0.5, 0.8), cost = c(2, 3)),
    codebook = codebook,
    data = c("a", "b"),
    input_type = "text",
    chat_args = list(name = "deepseek/deepseek-chat"),
    execution_args = list(include_tokens = TRUE, include_cost = TRUE),
    batch = FALSE,
    metadata = list(
      n_units = 2,
      prices = c(input = 1, output = 10, cached_input = 0.1),
      cost_note = "from supplied rates: $1 input, $10 output, $0.1 cached input, per million tokens"
    ),
    name = "run1",
    call = quote(qlm_code())
  )

  path <- file.path(tempdir(), "test_trail_prices")
  withr::defer(unlink(paste0(path, c(".rds", ".qmd"))))
  qlm_trail(coded, path = path)
  content <- readLines(paste0(path, ".qmd"))

  expect_true(any(grepl("^\\*\\*Cost:\\*\\* from supplied rates: \\$1 input", content)))

  # The replication call carries the rates, so the script costs the run the same way
  args <- qlm_code_call(content)
  expect_equal(eval(args$prices), c(input = 1, output = 10, cached_input = 0.1))
})

test_that("qlm_trail() report says why a cost is NA (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1:2, score = c(0.5, 0.8), cost = c(NA_real_, NA_real_)),
    codebook = codebook,
    data = c("a", "b"),
    input_type = "text",
    chat_args = list(name = "deepseek/deepseek-chat"),
    execution_args = list(include_cost = TRUE),
    batch = FALSE,
    metadata = list(n_units = 2, cost_note = "NA (ellmer has no prices for DeepSeek models)"),
    name = "run1",
    call = quote(qlm_code())
  )

  path <- file.path(tempdir(), "test_trail_cost_na")
  withr::defer(unlink(paste0(path, c(".rds", ".qmd"))))
  qlm_trail(coded, path = path)
  content <- readLines(paste0(path, ".qmd"))

  expect_true(any(grepl(
    "^\\*\\*Cost:\\*\\* NA \\(ellmer has no prices for DeepSeek models\\)$", content
  )))
  expect_null(qlm_code_call(content)$prices)
})


# File inputs (#124) -----------------------------------------------------------

test_that("the report names a file-input run's files by hash (#124)", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run <- audio_run(paths)
  temp_path <- tempfile("trail_audio")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  suppressMessages(trail <- qlm_trail(run, path = temp_path))
  content <- readLines(paste0(temp_path, ".qmd"))

  expect_true(any(grepl("**Input files (audio):** 2 files, SHA-256 recorded", content, fixed = TRUE)))
  expect_true(any(grepl(hash_file(paths[["a"]]), content, fixed = TRUE)))
  expect_true(any(grepl(paste0("| b | ", basename(paths[["b"]]), " | 10 | `"), content, fixed = TRUE)))
  expect_false(any(grepl("accepted by", content, fixed = TRUE)))

  # The trail object keeps what the report was built from
  expect_equal(trail$runs[["audio_run"]]$input_files$.id, c("a", "b"))

  # A text run says nothing about files
  text_path <- tempfile("trail_text")
  withr::defer({
    unlink(paste0(text_path, ".rds"))
    unlink(paste0(text_path, ".qmd"))
  })
  text_run <- new_qlm_coded(
    results = data.frame(id = "a", score = 1),
    codebook = qlm_codebook("T", "P", ellmer::type_object(score = ellmer::type_number("s"))),
    data = c(a = "some text"), input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"), execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 1),
    name = "t", call = quote(qlm_code(...))
  )
  suppressMessages(qlm_trail(text_run, path = text_path))
  expect_false(any(grepl("Input files", readLines(paste0(text_path, ".qmd")), fixed = TRUE)))
})


test_that("the report marks unrecorded hashes, and a backfill pass's model (#124)", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run <- audio_run(paths, failed = "b")
  meta_attr <- attr(run, "meta")
  meta_attr$user$input_files$sha256[1] <- NA_character_
  meta_attr$user$input_files$size[1] <- NA_real_
  meta_attr$object$backfill <- list(backfill_pass(
    model = "google_gemini/gemini-4-ultra", overrides = list(), attempted = "b",
    recovered = "b"
  ))
  attr(run, "meta") <- meta_attr
  temp_path <- tempfile("trail_pass")
  withr::defer({
    unlink(paste0(temp_path, ".rds"))
    unlink(paste0(temp_path, ".qmd"))
  })

  suppressMessages(qlm_trail(run, path = temp_path))
  content <- readLines(paste0(temp_path, ".qmd"))
  expect_true(any(grepl("gemini-4-ultra", content, fixed = TRUE)))
  expect_true(any(grepl("| a | ", content, fixed = TRUE) & grepl("not recorded", content, fixed = TRUE)))
  expect_true(any(grepl(hash_file(paths[["b"]]), content, fixed = TRUE)))
})


test_that("the report shows a video run's three kinds of input (#179)", {
  clip <- video_file(as.raw(1:10))
  downloaded <- video_file(as.raw(11:30))
  x <- c(clip = clip, ad = "https://user:pw@archive.org/download/ad/ad.mp4",
         zoo = "https://www.youtube.com/watch?v=jNQXAC9IVRw")
  run <- media_run(x, input_type = "video", local = c(clip, downloaded, NA))
  temp_path <- tempfile("trail_video")
  withr::defer(unlink(paste0(temp_path, c(".rds", ".qmd"))))

  suppressMessages(qlm_trail(run, path = temp_path))
  content <- readLines(paste0(temp_path, ".qmd"))

  expect_true(any(grepl("**Input files (video):** 3 files", content, fixed = TRUE)))
  expect_true(any(grepl(paste0("| clip | ", basename(clip), " | 10 | `", hash_file(clip)), content, fixed = TRUE)))
  # The downloaded URL is recorded without its credentials, with the hash of its bytes
  expect_true(any(grepl(paste0("| ad | https://archive.org/download/ad/ad.mp4 | 20 | `", hash_file(downloaded)), content, fixed = TRUE)))
  expect_false(any(grepl("user:pw", content, fixed = TRUE)))
  expect_true(any(grepl("| zoo | https://www.youtube.com/watch?v=jNQXAC9IVRw |  | fetched by the provider |", content, fixed = TRUE)))
})
