test_that("define_task creates a valid task object", {
  skip_if_not_installed("ellmer")

  # Minimal type_object
  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Numeric score between -1 and 1")
  )

  # Define a simple task
  task <- define_task(
    name = "Test Task",
    system_prompt = "Rate the sentiment of a text between -1 and 1.",
    type_def = type_obj
  )

  # Check structure
  expect_true(is.list(task))
  expect_true("run" %in% names(task))
  expect_true("system_prompt" %in% names(task))
  expect_true("type_def" %in% names(task))
  expect_equal(task$name, "Test Task")

  # Check S7 class using the actual class name
  task_class <- class(type_obj)
  expect_true(inherits(task$type_def, task_class[1]))  # first class should be "TypeObject"
})
