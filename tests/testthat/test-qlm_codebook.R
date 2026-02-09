test_that("qlm_codebook creates a valid codebook object", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Score"),
    explanation = ellmer::type_string("Explanation")
  )

  codebook <- qlm_codebook(
    name = "Test Codebook",
    instructions = "Rate the test.",
    schema = type_obj
  )

  expect_true(is.list(codebook))
  expect_equal(codebook$name, "Test Codebook")
  expect_equal(codebook$instructions, "Rate the test.")
  expect_equal(codebook$schema, type_obj)
  expect_null(codebook$role)
  expect_equal(codebook$input_type, "text")
})


test_that("qlm_codebook has dual class inheritance", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Score")
  )

  codebook <- qlm_codebook(
    name = "Test",
    instructions = "Test prompt",
    schema = type_obj
  )

  # Should have both classes
  expect_true(inherits(codebook, "qlm_codebook"))
  expect_true(inherits(codebook, "task"))

  # Class order matters for method dispatch
  expect_equal(class(codebook), c("qlm_codebook", "task"))
})


test_that("qlm_codebook validates input_type", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Valid input types
  cb_text <- qlm_codebook("Test", "Prompt", type_obj, input_type = "text")
  expect_equal(cb_text$input_type, "text")

  cb_image <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")
  expect_equal(cb_image$input_type, "image")

  # Invalid input type should error
  expect_error(
    qlm_codebook("Test", "Prompt", type_obj, input_type = "invalid"),
    "'arg' should be one of"
  )
})


test_that("as_qlm_codebook converts task objects", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Suppress deprecation warning for this test
  withr::local_options(lifecycle_verbosity = "quiet")

  # Create old-style task object
  old_task <- task(
    name = "Old Task",
    system_prompt = "Old prompt",
    type_def = type_obj
  )

  # Convert to qlm_codebook
  converted <- as_qlm_codebook(old_task)

  # Should now have both classes
  expect_true(inherits(converted, "qlm_codebook"))
  expect_true(inherits(converted, "task"))

  # Should preserve content
  expect_equal(converted$name, "Old Task")
  expect_equal(converted$system_prompt, "Old prompt")
  expect_equal(converted$type_def, type_obj)
})


test_that("as_qlm_codebook is idempotent for qlm_codebook objects", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Converting a qlm_codebook should return it unchanged
  converted <- as_qlm_codebook(codebook)

  expect_identical(converted, codebook)
})


test_that("print.qlm_codebook works", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  codebook <- qlm_codebook(
    name = "Test Codebook",
    instructions = "This is a test prompt for printing",
    schema = type_obj,
    role = "You are an expert annotator."
  )

  # Capture print output
  output <- capture.output(print(codebook))

  expect_true(any(grepl("quallmer codebook", output)))
  expect_true(any(grepl("Test Codebook", output)))
  expect_true(any(grepl("Input type", output)))
  expect_true(any(grepl("Role", output)))
  expect_true(any(grepl("Instructions", output)))
})


test_that("qlm_codebook role parameter works correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Without role
  cb_no_role <- qlm_codebook(
    name = "Test",
    instructions = "Rate the text.",
    schema = type_obj
  )

  expect_null(cb_no_role$role)
  expect_equal(cb_no_role$instructions, "Rate the text.")

  # With role
  cb_with_role <- qlm_codebook(
    name = "Test",
    instructions = "Rate the text.",
    schema = type_obj,
    role = "You are an expert."
  )

  expect_equal(cb_with_role$role, "You are an expert.")
  expect_equal(cb_with_role$instructions, "Rate the text.")
  # Note: system_prompt is constructed in qlm_code(), not stored in codebook
})


test_that("predefined codebooks are qlm_codebook objects", {
  skip_if_not_installed("ellmer")

  # Predefined codebook should be qlm_codebook object
  expect_true(inherits(data_codebook_sentiment, "qlm_codebook"))
})
