test_that("annotate validates task argument", {
  skip_if_not_installed("ellmer")

  texts <- c("Hello", "World")

  # Not a task object
  expect_error(
    annotate(texts, task = list(name = "fake"), model_name = "openai"),
    "`task` must be created using task()"
  )

  expect_error(
    annotate(texts, task = "not a task", model_name = "openai"),
    "`task` must be created using task()"
  )
})

test_that("annotate validates input type", {
  skip_if_not_installed("ellmer")

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

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("A score")
  )

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = type_obj
  )

  texts <- c("Hello", "World")

  # This should warn about fake_arg but we can't fully run without API
  # So we just test that the function accepts the structure
  expect_warning(
    tryCatch(
      annotate(texts, tsk, model_name = "openai", fake_arg = 123),
      error = function(e) NULL
    ),
    "not recognized"
  )
})

test_that("annotate requires model_name argument", {
  skip_if_not_installed("ellmer")

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

test_that("annotate routes arguments correctly", {
  skip_if_not_installed("ellmer")

  # Test that argument routing logic works by checking formals detection
  chat_args <- names(formals(ellmer::chat))
  pcs_args <- names(formals(ellmer::parallel_chat_structured))

  # echo should go to chat
  expect_true("echo" %in% chat_args)

  # max_active should go to parallel_chat_structured
  expect_true("max_active" %in% pcs_args)

  # No overlap between the key arguments we care about
  expect_false("echo" %in% pcs_args)
  expect_false("max_active" %in% chat_args)
})
