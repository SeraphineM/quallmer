test_that("task creates a valid task object", {
  skip_if_not_installed("ellmer")

  # Minimal type_object
  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Numeric score between -1 and 1")
  )

  # Define a simple task
  tsk <- task(
    name = "Test Task",
    system_prompt = "Rate the sentiment of a text between -1 and 1.",
    type_def = type_obj
  )

  # Check structure - task is now a pure data structure
  expect_true(is.list(tsk))
  expect_true(inherits(tsk, "task"))
  expect_true("name" %in% names(tsk))
  expect_true("system_prompt" %in% names(tsk))
  expect_true("type_def" %in% names(tsk))
  expect_true("input_type" %in% names(tsk))
  expect_equal(tsk$name, "Test Task")
  expect_equal(tsk$input_type, "text")

  # Check S7 class using the actual class name
  task_class <- class(type_obj)
  expect_true(inherits(tsk$type_def, task_class[1]))
})

test_that("task validates input_type", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    label = ellmer::type_string("A label")
  )

  # Default is "text"
  tsk_text <- task(
    name = "Text task",
    system_prompt = "Label this text.",
    type_def = type_obj
  )
  expect_equal(tsk_text$input_type, "text")

  # Explicit "image"
  tsk_image <- task(
    name = "Image task",
    system_prompt = "Describe this image.",
    type_def = type_obj,
    input_type = "image"
  )
  expect_equal(tsk_image$input_type, "image")

  # Invalid input_type

  expect_error(
    task(
      name = "Bad task",
      system_prompt = "Test",
      type_def = type_obj,
      input_type = "audio"
    ),
    "should be one of"
  )
})
