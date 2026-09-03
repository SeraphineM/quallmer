test_that("trail_record() reaches qlm_code() through annotate() (#141)", {
  skip_if_not_installed("ellmer")
  withr::local_options(lifecycle_verbosity = "quiet")

  # trail_record() has no route to results except annotate(), so the wrong
  # argument name there stopped this path entirely. Exercise the default
  # annotate_fun with qlm_code() recorded rather than called.
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
  setting <- trail_settings(provider = "openai", model = "gpt-4o-mini",
                            temperature = 0)
  # Non-sequential identifiers, so that annotate()'s own sequential `id`
  # column cannot pass for the caller's when id_col is also "id".
  d <- data.frame(id = c("doc-a", "doc-b"), text = c("great", "awful"))

  rec <- trail_record(d, "text", tsk, setting, id_col = "id")

  expect_s3_class(rec, "trail_record")
  expect_identical(seen$model, "openai/gpt-4o-mini")
  expect_false("model_name" %in% names(seen$dots))
  expect_identical(rec$annotations$id, c("doc-a", "doc-b"))
})

test_that("trail_record() keeps annotate()'s id beside a differently named id_col (#141)", {
  skip_if_not_installed("ellmer")
  withr::local_options(lifecycle_verbosity = "quiet")

  local_mocked_bindings(qlm_code = function(x, codebook, model, ...) {
    tibble::tibble(.id = names(x) %||% seq_along(x), score = 1)
  })

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = ellmer::type_object(score = ellmer::type_number("A score"))
  )
  setting <- trail_settings(provider = "openai", model = "gpt-4o-mini")
  d <- data.frame(doc = c("doc-a", "doc-b"), text = c("great", "awful"))

  rec <- trail_record(d, "text", tsk, setting, id_col = "doc")

  expect_identical(rec$annotations$doc, c("doc-a", "doc-b"))
  expect_identical(rec$annotations$id, 1:2)
})
