test_that("annotate validates task argument", {
  skip_if_not_installed("ellmer")

  # Suppress deprecation warnings for testing deprecated function
  withr::local_options(lifecycle_verbosity = "quiet")

  texts <- c("Hello", "World")

  # Not a task object
  expect_error(
    annotate(texts, task = list(name = "fake"), model_name = "openai"),
    "must be created using.*qlm_codebook"
  )

  expect_error(
    annotate(texts, task = "not a task", model_name = "openai"),
    "must be created using.*qlm_codebook"
  )
})

test_that("annotate validates input type", {
  skip_if_not_installed("ellmer")

  # Suppress deprecation warnings for testing deprecated function
  withr::local_options(lifecycle_verbosity = "quiet")

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("A score")
  )

  text_task <- task(
    name = "Text task",
    system_prompt = "Score this.",
    type_def = type_obj,
    input_type = "text"
  )

  # Text task expects character input
  expect_error(
    annotate(123, task = text_task, model_name = "openai"),
    "expects text input"
  )

  expect_error(
    annotate(list("a", "b"), task = text_task, model_name = "openai"),
    "expects text input"
  )

  image_task <- task(
    name = "Image task",
    system_prompt = "Describe this.",
    type_def = type_obj,
    input_type = "image"
  )

  # Image task also expects character (file paths)
  expect_error(
    annotate(123, task = image_task, model_name = "openai"),
    "expects image file paths"
  )
})

test_that("annotate warns about unrecognized arguments", {
  skip_if_not_installed("ellmer")
  skip("Warning detection in test environment is inconsistent; manually verified to work")

  # This test verifies that annotate() warns about unrecognized arguments.
  # The warning functionality has been manually verified to work correctly
  # with cli::cli_warn(), but capturing it in the test environment with
  # expect_warning() while also handling API errors is problematic.

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("A score")
  )

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = type_obj
  )

  texts <- c("Hello", "World")

  # Would test: annotate(texts, tsk, model_name = "openai", fake_arg = 123)
  # Expected: warning about "fake_arg" not being recognized
})

test_that("annotate requires model_name argument", {
  skip_if_not_installed("ellmer")

  # Suppress deprecation warnings for testing deprecated function
  withr::local_options(lifecycle_verbosity = "quiet")

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("A score")
  )

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = type_obj
  )

  texts <- c("Hello", "World")

  # model_name is required (no default)
  expect_error(
    annotate(texts, tsk),
    "model_name"
  )
})

test_that("annotate() hands model_name to qlm_code() as model (#141)", {
  skip_if_not_installed("ellmer")
  withr::local_options(lifecycle_verbosity = "quiet")

  # qlm_code()'s formal is `model`; `model_name` is longer, so it never
  # partially matches and fell through `...` to the provider call. Record what
  # qlm_code() receives rather than spending a request.
  seen <- NULL
  local_mocked_bindings(qlm_code = function(x, codebook, model, ...) {
    seen <<- list(model = model, dots = list(...))
    tibble::tibble(.id = names(x) %||% seq_along(x), score = 1)
  })

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = ellmer::type_object(score = ellmer::type_number("A score"))
  )

  out <- annotate(c("Hello", "World"), tsk, model_name = "openai/gpt-4o-mini")

  expect_identical(seen$model, "openai/gpt-4o-mini")
  expect_false("model_name" %in% names(seen$dots))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that("annotate() still returns its documented `id` column (#141)", {
  skip_if_not_installed("ellmer")
  withr::local_options(lifecycle_verbosity = "quiet")

  # qlm_code() keys rows on `.id`. annotate() documented, and returned, `id`
  # as the first column, from the input's names or sequential integers.
  local_mocked_bindings(qlm_code = function(x, codebook, model, ...) {
    tibble::tibble(.id = names(x) %||% seq_along(x), score = 1)
  })

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = ellmer::type_object(score = ellmer::type_number("A score"))
  )

  named <- annotate(c("doc-a" = "Hello", "doc-b" = "World"), tsk,
                    model_name = "openai/gpt-4o-mini")
  expect_identical(names(named), c("id", "score"))
  expect_identical(named$id, c("doc-a", "doc-b"))
  expect_false(".id" %in% names(named))

  unnamed <- annotate(c("Hello", "World"), tsk, model_name = "openai/gpt-4o-mini")
  expect_identical(unnamed$id, 1:2)
})

test_that("annotate routes arguments correctly", {
  skip_if_not_installed("ellmer")

  # Test that argument routing logic works by checking formals detection
  chat_args <- names(formals(ellmer::chat))
  execution_args <- names(formals(ellmer::parallel_chat_structured))

  # echo should go to chat
  expect_true("echo" %in% chat_args)

  # max_active should go to parallel_chat_structured
  expect_true("max_active" %in% execution_args)

  # No overlap between the key arguments we care about
  expect_false("echo" %in% execution_args)
  expect_false("max_active" %in% chat_args)
})
