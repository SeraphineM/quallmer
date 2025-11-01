test_that("define_task creates a valid task object", {
  # Minimal type object
  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Numeric score between -1 and 1")
  )

  # Define a simple task
  task <- define_task(
    name = "Test Task",
    system_prompt = "Rate the sentiment of a text between -1 and 1.",
    type_object = type_obj
  )

  # Check structure
  expect_true(is.list(task))
  expect_true("run" %in% names(task))
  expect_true("system_prompt" %in% names(task))
  expect_true("type_object" %in% names(task))
  expect_equal(task$name, "Test Task")
})
