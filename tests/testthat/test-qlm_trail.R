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
