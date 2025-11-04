test_that("annotate runs a task and returns a data frame", {
  skip_if_not_installed("ellmer")

  # Dummy data
  texts <- c("I love democracy.", "This is terrible.")

  # Define a simple dummy task that doesn't actually call OpenAI
  type_def <- ellmer::type_object(
    score = ellmer::type_number("Sentiment score")
  )

  mock_task <- define_task(
    name = "Mock Sentiment",
    system_prompt = "Return +1 for positive text, -1 for negative text.",
    type_def = type_def
  )

  # Mock run function to simulate ellmer call
  mock_task$run <- function(.data, ...) {
    data.frame(
      id = seq_along(.data),
      score = ifelse(grepl("love", .data), 1, -1)
    )
  }

  # Test annotate()
  results <- annotate(texts, task = mock_task, verbose = FALSE)

  # Basic checks
  expect_s3_class(results, "data.frame")
  expect_true(all(c("id", "score") %in% names(results)))
  expect_equal(nrow(results), length(texts))
  expect_equal(results$score, c(1, -1))
})
