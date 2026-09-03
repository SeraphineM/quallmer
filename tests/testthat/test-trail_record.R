test_that("trail_record() reaches qlm_code() through annotate() (#141)", {
  skip_if_not_installed("ellmer")
  withr::local_options(lifecycle_verbosity = "quiet")

  # trail_record() has no route to results except annotate(), so the wrong
  # argument name there stopped this path entirely. Exercise the default
  # annotate_fun with qlm_code() recorded rather than called.
  seen <- NULL
  local_mocked_bindings(qlm_code = function(x, codebook, model, ...) {
    seen <<- list(model = model, dots = list(...))
    tibble::tibble(.id = seq_along(x), score = 1)
  })

  tsk <- task(
    name = "Test",
    system_prompt = "Test prompt",
    type_def = ellmer::type_object(score = ellmer::type_number("A score"))
  )
  setting <- trail_settings(provider = "openai", model = "gpt-4o-mini",
                            temperature = 0)
  d <- data.frame(id = 1:2, text = c("great", "awful"))

  rec <- trail_record(d, "text", tsk, setting, id_col = "id")

  expect_s3_class(rec, "trail_record")
  expect_identical(seen$model, "openai/gpt-4o-mini")
  expect_false("model_name" %in% names(seen$dots))
  expect_equal(rec$annotations$id, 1:2)
})
