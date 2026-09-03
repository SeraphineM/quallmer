test_that("qlm_code validates codebook argument", {
  skip_if_not_installed("ellmer")

  # Should error on invalid codebook objects
  expect_error(
    qlm_code(c("test"), codebook = list(name = "fake"), model = "test"),
    "must be created using.*qlm_codebook"
  )

  expect_error(
    qlm_code(c("test"), codebook = "not valid", model = "test"),
    "must be created using.*qlm_codebook"
  )
})


test_that("qlm_code accepts both task and qlm_codebook objects", {
  skip_if_not_installed("ellmer")

  withr::local_options(lifecycle_verbosity = "quiet")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Should accept qlm_codebook
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  expect_true(inherits(codebook, "qlm_codebook"))

  # Should accept old task (will be converted internally)
  old_task <- task("Test", "Prompt", type_obj)
  expect_true(inherits(old_task, "task"))

  # Both should pass validation (we can't test execution without APIs)
  # but we can verify they're accepted as valid input types
})


test_that("qlm_code validates input type matches codebook", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Text codebook expects character input
  text_codebook <- qlm_codebook("Test", "Prompt", type_obj, input_type = "text")

  # Should error on non-character input
  expect_error(
    qlm_code(x = 123, codebook = text_codebook, model = "test"),
    "expects text input.*character vector"
  )

  expect_error(
    qlm_code(x = list("a", "b"), codebook = text_codebook, model = "test"),
    "expects text input.*character vector"
  )

  # Image codebook also expects character input (file paths)
  image_codebook <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")

  expect_error(
    qlm_code(x = 123, codebook = image_codebook, model = "test"),
    "expects image file paths.*character vector"
  )
})


test_that("qlm_code returns qlm_coded object structure", {
  skip_if_not_installed("ellmer")

  # We can't test actual execution, but we can verify the structure
  # by examining what new_qlm_coded creates

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  # Add id column to mock_results
  mock_results$id <- 1:2

  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(
      timestamp = Sys.time(),
      n_units = 2,
      ellmer_version = "0.4.0",
      quallmer_version = "0.2.0",
      R_version = "4.3.0"
    ),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify structure - qlm_coded is now a data.frame with attributes
  expect_true(inherits(mock_coded, "qlm_coded"))
  expect_true(inherits(mock_coded, "data.frame"))
  expect_true(is.data.frame(mock_coded))

  # Verify data frame columns (id renamed to .id)
  expect_true(".id" %in% names(mock_coded))
  expect_true("score" %in% names(mock_coded))

  # Verify attributes with new hierarchical structure
  expect_true(!is.null(attr(mock_coded, "data")))
  expect_equal(attr(mock_coded, "meta")$object$input_type, "text")
  meta_attr <- attr(mock_coded, "meta")
  expect_true(!is.null(meta_attr))
  expect_identical(attr(mock_coded, "codebook"), codebook)
  expect_true(is.list(meta_attr$object$chat_args))
  expect_true(is.list(meta_attr$object$execution_args))
  expect_false(meta_attr$object$batch)  # batch flag should be FALSE by default
  expect_true(is.list(meta_attr$system))
  expect_equal(meta_attr$user$name, "original")
  expect_null(meta_attr$object$parent)
})


test_that("qlm_code routes arguments correctly", {
  skip_if_not_installed("ellmer")

  # Test that argument routing logic doesn't crash
  # (Can't test actual routing without API calls)

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Get valid argument names
  chat_args <- names(formals(ellmer::chat))
  pcs_args <- names(formals(ellmer::parallel_chat_structured))

  expect_true(length(chat_args) > 0)
  expect_true(length(pcs_args) > 0)

  # Verify some expected arguments exist
  expect_true("name" %in% chat_args)
  expect_true("system_prompt" %in% chat_args)
  expect_true("chat" %in% pcs_args)
  expect_true("prompts" %in% pcs_args)
  expect_true("type" %in% pcs_args)
})


test_that("qlm_code works with predefined codebooks", {
  skip_if_not_installed("ellmer")

  # Predefined codebook should be valid
  expect_true(inherits(data_codebook_sentiment, "qlm_codebook"))
})


test_that("print.qlm_coded displays correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test Codebook", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:3, score = c(0.5, -0.3, 0.8))

  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Test that print works without error (delegates to tibble print)
  expect_no_error(print(mock_coded))

  # Verify it's a tibble
  expect_true(tibble::is_tibble(mock_coded))
})


test_that("qlm_code routes all execution arguments to execution_args", {
  skip_if_not_installed("ellmer")

  # Get valid argument names from both functions
  pcs_arg_names <- names(formals(ellmer::parallel_chat_structured))
  batch_arg_names <- names(formals(ellmer::batch_chat_structured))

  # All of these should be routed to execution_args
  expect_true("path" %in% batch_arg_names)  # batch-specific
  expect_true("wait" %in% batch_arg_names)  # batch-specific
  expect_true("ignore_hash" %in% batch_arg_names)  # batch-specific
  expect_true("max_active" %in% pcs_arg_names)  # parallel-specific
  expect_true("rpm" %in% pcs_arg_names)  # parallel-specific
  expect_true("on_error" %in% pcs_arg_names)  # parallel-specific

  # Shared args
  expect_true("convert" %in% pcs_arg_names)
  expect_true("convert" %in% batch_arg_names)
  expect_true("include_tokens" %in% pcs_arg_names)
  expect_true("include_tokens" %in% batch_arg_names)
})


test_that("new_qlm_coded stores batch flag and execution_args", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  # Test with batch=TRUE and mixed execution args (parallel + batch)
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(path = "/tmp/batch", wait = TRUE, max_active = 5, convert = TRUE),
    batch = TRUE,
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "batch_test",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify batch flag is stored
  meta_attr <- attr(mock_coded, "meta")
  expect_true(meta_attr$object$batch)

  # Verify execution_args contains all args (both parallel and batch specific)
  expect_true(is.list(meta_attr$object$execution_args))
  expect_equal(meta_attr$object$execution_args$path, "/tmp/batch")
  expect_true(meta_attr$object$execution_args$wait)
  expect_equal(meta_attr$object$execution_args$max_active, 5)
  expect_true(meta_attr$object$execution_args$convert)
})


test_that("new_qlm_coded maintains backward compatibility with pcs_args", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  # Test with old pcs_args parameter
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    pcs_args = list(max_active = 5),
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "compat_test",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify pcs_args are converted to execution_args
  meta_attr <- attr(mock_coded, "meta")
  expect_true(is.list(meta_attr$object$execution_args))
  expect_equal(meta_attr$object$execution_args$max_active, 5)
})


test_that("qlm_code passes provider-specific arguments to ellmer::chat", {

  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Track what arguments are passed to ellmer::chat
  chat_args_received <- NULL
  mock_chat <- function(...) {
    chat_args_received <<- list(...)
    structure(list(), class = "ellmer_chat")
  }
  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "ellmer::parallel_chat_structured", mock_results)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  # Call with a provider-specific argument (like base_url for openai_compatible)
  f(c("text1", "text2"), codebook, model = "openai_compatible/test-model",
           base_url = "https://my-api.com/v1")

  # Verify the provider-specific argument was passed through to ellmer::chat
  expect_true("base_url" %in% names(chat_args_received))
  expect_equal(chat_args_received$base_url, "https://my-api.com/v1")
})


test_that("qlm_code uses parallel_chat_structured when batch=FALSE", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Mock the functions
  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  mock_pcs <- mockery::mock(mock_results, cycle = TRUE)
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "ellmer::parallel_chat_structured", mock_pcs)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f(c("text1", "text2"), codebook,
                     model = "openai_compatible/test-model", batch = FALSE)

  # Verify parallel_chat_structured was called
  mockery::expect_called(mock_pcs, 1)

  # Verify result structure
  expect_s3_class(result, "qlm_coded")
  expect_false(attr(result, "meta")$object$batch)
})


test_that("qlm_code uses batch_chat_structured when batch=TRUE", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Mock the functions
  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  mock_bcs <- mockery::mock(mock_results, cycle = TRUE)
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "ellmer::batch_chat_structured", mock_bcs)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  # Use an execution arg that's valid (convert is in both parallel and batch)
  result <- suppressWarnings(
    f(c("text1", "text2"), codebook,
      model = "openai_compatible/test-model", batch = TRUE,
      convert = TRUE)
  )

  # Verify batch_chat_structured was called
  mockery::expect_called(mock_bcs, 1)

  # Verify result structure
  expect_s3_class(result, "qlm_coded")
  expect_true(attr(result, "meta")$object$batch)
  expect_equal(attr(result, "meta")$object$execution_args$convert, TRUE)
})


test_that("qlm_code builds metadata correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Mock the functions
  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- data.frame(id = 1:3, score = c(0.5, 0.8, 0.2))

  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "ellmer::parallel_chat_structured", mock_results)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f(c("text1", "text2", "text3"), codebook,
              model = "openai_compatible/test-model")

  meta_attr <- attr(result, "meta")

  # Verify metadata structure
  expect_true(is.list(meta_attr$system))
  expect_true("timestamp" %in% names(meta_attr$system))
  expect_equal(meta_attr$object$n_units, 3)
  expect_true("ellmer_version" %in% names(meta_attr$system))
  expect_true("quallmer_version" %in% names(meta_attr$system))
  expect_true("R_version" %in% names(meta_attr$system))

  # Verify timestamp is recent
  expect_true(inherits(meta_attr$system$timestamp, "POSIXct"))
  expect_true(difftime(Sys.time(), meta_attr$system$timestamp, units = "secs") < 1)
})


test_that("qlm_code stores notes in metadata", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test Codebook", "Test instructions", type_obj)

  # Create a qlm_coded object with notes
  result <- new_qlm_coded(
    results = data.frame(id = 1:3, score = c(0.5, -0.3, 0.8)),
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(
      timestamp = Sys.time(),
      n_units = 3,
      notes = "Test run with temperature 0.5"
    ),
    name = "test_run",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify notes are stored in metadata
  meta_attr <- attr(result, "meta")
  expect_equal(meta_attr$user$notes, "Test run with temperature 0.5")

  # Test print output includes notes
  output <- capture.output(print(result))
  expect_true(any(grepl("Notes:.*Test run with temperature 0.5", output)))
})


test_that("default_structured_mode skips the structured call only where it cannot work", {
  # DeepSeek's API answers `response_format` with HTTP 400, so attempting it is
  # a guaranteed-wasted round trip
  expect_equal(default_structured_mode("deepseek/deepseek-v4-pro"), "json")
  expect_equal(default_structured_mode("deepseek"), "json")

  expect_equal(default_structured_mode("openai/gpt-4o-mini"), "auto")
  expect_equal(default_structured_mode("anthropic/claude-sonnet-4-5"), "auto")
  expect_equal(default_structured_mode("openai_compatible/kimi-k3"), "auto")
})


test_that("qlm_code delegates to the handler and records the backend", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  handler_args <- NULL
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           max_retries = 2L, model_hint = NULL, ...) {
    handler_args <<- list(model = model, batch = batch, max_retries = max_retries,
                          chat_args = chat_args, execution_args = execution_args)
    results <- tibble::tibble(score = c(0.5, 0.8))
    attr(results, "qlm_backend_meta") <- list(backend = "json_mode", n_invalid = 0)
    results
  }

  f <- qlm_code
  mockery::stub(f, "code_handler_json", fake_handler)

  result <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
              max_retries = 5, max_active = 3)

  expect_s3_class(result, "qlm_coded")
  expect_equal(result$.id, 1:2)
  expect_equal(result$score, c(0.5, 0.8))
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  expect_equal(qlm_meta(result, type = "user")$n_invalid, 0)

  # max_retries reaches the handler as a formal; max_active still routes to execution
  expect_equal(handler_args$max_retries, 5)
  expect_equal(handler_args$execution_args, list(max_active = 3))
  expect_length(handler_args$chat_args, 0)
})


test_that("qlm_code still uses parallel_chat_structured for other providers", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  mock_pcs <- mockery::mock(data.frame(score = c(0.5, 0.8)), cycle = TRUE)
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "ellmer::parallel_chat_structured", mock_pcs)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini")

  mockery::expect_called(mock_pcs, 1)
  # Every run now says which path produced it
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_equal(qlm_meta(result, type = "object")$structured, "auto")
})


test_that("qlm_code errors on max_retries only where JSON mode cannot be reached", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # `structured` forbids the JSON path, so repair attempts can never happen
  expect_error(
    qlm_code(c("a"), codebook, model = "openai/gpt-4o-mini",
             structured = "structured", max_retries = 2),
    "not supported with"
  )
  # Even when the value happens to equal the default
  expect_error(
    qlm_code(c("a"), codebook, model = "openai/gpt-4o-mini",
             structured = "structured", max_retries = 2L),
    "not supported with"
  )

  # Under "auto" the JSON path is reachable, so the argument applies
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "ellmer::parallel_chat_structured", data.frame(score = 0.5))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  expect_s3_class(
    f(c("a"), codebook, model = "openai/gpt-4o-mini", max_retries = 4),
    "qlm_coded"
  )
})


test_that("qlm_code passes its max_retries default to the handler", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  seen <- NULL
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           max_retries, model_hint = NULL, ...) {
    seen <<- max_retries
    tibble::tibble(score = 0.5)
  }
  f <- qlm_code
  mockery::stub(f, "code_handler_json", fake_handler)

  f("a", codebook, model = "deepseek/deepseek-chat")
  expect_equal(seen, 2L)
})


test_that("qlm_code requires a single model string", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  expect_error(
    qlm_code(c("a"), codebook, model = c("openai", "anthropic")),
    "must be a single string"
  )
})


# structured = c("auto", "structured", "json") --------------------------------

# A try_structured_call() with the ellmer calls stubbed out. `results` is
# returned by the structured call; `errors` is a character vector of messages
# to throw, one per attempt, NA meaning "succeed".
structured_stub <- function(results = data.frame(score = 0.5), errors = NULL,
                            calls = NULL) {
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) structure(list(), class = "Chat"))
  mockery::stub(tsc, "warn_unenforced_schema", function(...) invisible(NULL))
  i <- 0L
  mockery::stub(tsc, "ellmer::parallel_chat_structured", function(chat, prompts, type, ...) {
    i <<- i + 1L
    if (!is.null(calls)) {
      calls$n <- i
    }
    err <- if (!is.null(errors) && i <= length(errors)) errors[[i]] else NA_character_
    if (!is.na(err)) stop(err, call. = FALSE)
    results
  })
  tsc
}

json_stub <- function(calls = NULL) {
  function(x, codebook, model, chat_args, execution_args, batch, max_retries,
           model_hint = NULL, ...) {
    if (!is.null(calls)) {
      calls$json <- TRUE
      calls$max_retries <- max_retries
      calls$model_hint <- model_hint
    }
    results <- tibble::tibble(score = rep(0.99, length(x)))
    attr(results, "qlm_backend_meta") <- list(backend = "json_mode", n_invalid = 0)
    results
  }
}

structured_test_codebook <- function() {
  qlm_codebook("Test", "Prompt", ellmer::type_object(score = ellmer::type_number("Score")))
}


test_that("structured = 'structured' never reaches the JSON path", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub())
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
              structured = "structured")

  expect_null(calls$json)
  expect_equal(result$score, 0.5)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("structured = 'json' never attempts the structured call", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
              structured = "json")

  expect_null(calls$n)
  expect_true(calls$json)
  expect_equal(result$score, 0.99)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  expect_equal(qlm_meta(result, type = "object")$structured, "json")
})


test_that("structured = 'auto' falls back to JSON mode when the call errors", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(errors = "HTTP 400 Bad Request.", calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f("a", structured_test_codebook(), model = "openai_compatible/kimi-k3",
                base_url = "https://example.com/v1"),
    "falling back to JSON mode"
  )

  expect_true(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  # The reason is kept, so a run that switched path can say why
  expect_match(qlm_meta(result, type = "user")$fallback_reason, "400")
})


test_that("structured = 'auto' falls back when every required field is NA", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # HTTP 200, but the endpoint accepted the schema and ignored it
  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(results = data.frame(score = c(NA_real_, NA_real_))))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(), model = "openai_compatible/x",
                base_url = "https://example.com/v1"),
    "no usable values"
  )
  expect_true(calls$json)
})


test_that("a partly incomplete structured result warns without falling back", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(results = data.frame(score = c(1, NA_real_, 3))))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b", "c"), structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "1 row from the structured call is missing"
  )
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("structured = 'auto' still falls back when the endpoint answered in prose", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # Every row carries the extraction error qlm_code() records from ellmer's
  # warning: the endpoint answered, but not with the schema
  prose <- tibble::tibble(
    score = c(NA_real_, NA_real_),
    .error = list(
      extraction_error("Data extraction failed: no JSON responses found."),
      extraction_error("Data extraction failed: no JSON responses found.")
    )
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = prose))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(), model = "openai_compatible/x",
                base_url = "https://example.com/v1"),
    "no usable values"
  )
  expect_true(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
})


test_that("a structured run the provider rejects outright names the model (#133)", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # ellmer's parallel path hands back every rejected request as a row whose
  # .error is the HTTP condition, all fields NA
  http_400 <- function() {
    structure(
      list(message = "HTTP 400 Bad Request.", status = 400L, call = NULL),
      class = c("httr2_http_400", "httr2_http", "httr2_error", "rlang_error", "error", "condition")
    )
  }
  rejected <- tibble::tibble(
    score = c(NA_real_, NA_real_),
    .error = list(http_400(), http_400())
  )
  expect_true(all_rejected(rejected))
  expect_false(all_rejected(tibble::tibble(score = NA_real_, .error = list(simpleError("cut off")))))
  expect_false(all_rejected(tibble::tibble(score = c(NA_real_, 1), .error = list(http_400(), NULL))))
  expect_false(all_rejected(tibble::tibble(score = NA_real_)))

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = rejected))
  mockery::stub(f, "code_handler_json", json_stub(calls))
  mockery::stub(f, "model_name_hint", function(model, chat_args) {
    c("i" = "\"gpt-4o-mimi\" is not a model that \"openai\" lists.")
  })

  # The provider confirms the name is wrong: stop, rather than send it again
  # in JSON mode
  expect_error(
    f(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mimi"),
    "is not a model that"
  )
  expect_null(calls$json)

  # Without that confirmation, a rejection may be the provider refusing the
  # schema-constrained request itself, so the fallback runs as before
  g <- qlm_code
  mockery::stub(g, "try_structured_call", structured_stub(results = rejected))
  mockery::stub(g, "code_handler_json", json_stub(calls))
  mockery::stub(g, "model_name_hint", function(model, chat_args) character())
  expect_warning(
    g(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mimi"),
    "falling back to JSON mode"
  )
  expect_true(calls$json)
  # The provider was asked once; the JSON path is told the answer, not sent to ask again
  expect_identical(calls$model_hint, character())

  # And under structured = "structured" the rejection is the reported failure
  expect_error(
    g(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mimi",
      structured = "structured"),
    "HTTP 400 Bad Request"
  )
})


test_that("a wholly failed structured run is reported, not re-coded in JSON mode", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # Every row carries a reason -- here, as ellmer will report a cut-off
  # response once it consults the finish reason on the parallel path
  failed <- tibble::tibble(score = NA_real_, .error = list(simpleError("cut off")))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = failed))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini")

  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_equal(qlm_failures(result)$reason, "cut off")
})


# Truncated structured responses ----------------------------------------------

test_that("declared_max_tokens reads params() and nothing else", {
  expect_equal(declared_max_tokens(list(params = ellmer::params(max_tokens = 200))), 200)
  expect_null(declared_max_tokens(list()))
  expect_null(declared_max_tokens(list(params = ellmer::params(temperature = 0))))
  expect_null(declared_max_tokens(list(params = "not a list")))
  expect_null(declared_max_tokens(list(params = list(max_tokens = -1))))
  # api_args is forwarded unread
  expect_null(declared_max_tokens(list(api_args = list(max_tokens = 5))))
})

test_that("mark_truncated_rows flags only rows that spent the whole limit and hold nothing", {
  schema <- structured_test_codebook()$schema
  results <- tibble::tibble(
    score = c(NA_real_, 0.4, NA_real_),
    input_tokens = c(10, 10, 10),
    output_tokens = c(100, 100, 30),
    cached_input_tokens = c(0, 0, 0)
  )

  expect_warning(
    out <- mark_truncated_rows(results, schema, cap = 100),
    "1 response from the structured call used the whole"
  )
  errors <- out$.error
  expect_s3_class(errors[[1]], "simpleError")  # spent it all, nothing back
  expect_null(errors[[2]])                      # spent it all, but answered
  expect_null(errors[[3]])                      # nothing back, but well under the limit
  expect_match(conditionMessage(errors[[1]]), "max_tokens limit of 100")
  expect_match(conditionMessage(errors[[1]]), "params\\(max_tokens = \\)")
  # ellmer's column order: coded values, .error, usage
  expect_equal(
    names(out),
    c("score", ".error", "input_tokens", "output_tokens", "cached_input_tokens")
  )
})

test_that("mark_truncated_rows keeps a request failure but replaces a parse symptom", {
  schema <- structured_test_codebook()$schema
  results <- tibble::tibble(
    score = c(NA_real_, NA_real_, NA_real_),
    # A failed request spends nothing; an extraction failure at the limit is
    # what a cut-off response looks like after with_extraction_errors()
    .error = list(simpleError("HTTP 500"), simpleError("parse error: premature EOF"), NULL),
    input_tokens = c(0, 3, 3), output_tokens = c(0, 100, 100), cached_input_tokens = c(0, 0, 0)
  )

  expect_warning(out <- mark_truncated_rows(results, schema, cap = 100), "2 responses")
  expect_equal(conditionMessage(out$.error[[1]]), "HTTP 500")
  expect_match(conditionMessage(out$.error[[2]]), "most likely cut off")
  expect_match(conditionMessage(out$.error[[3]]), "most likely cut off")
})

test_that("mark_truncated_rows removes token columns it asked for itself", {
  schema <- structured_test_codebook()$schema
  results <- tibble::tibble(
    score = 0.4, input_tokens = 1, output_tokens = 5, cached_input_tokens = 0, cost = 0.01
  )

  expect_equal(names(mark_truncated_rows(results, schema, cap = 100, keep_tokens = FALSE)),
               c("score", "cost"))
  expect_equal(names(mark_truncated_rows(results, schema, cap = 100, keep_tokens = TRUE)),
               names(results))
})

test_that("mark_truncated_rows is a no-op without a declared limit or token counts", {
  schema <- structured_test_codebook()$schema
  results <- tibble::tibble(score = NA_real_)

  expect_identical(mark_truncated_rows(results, schema, cap = NULL), results)
  expect_identical(mark_truncated_rows(results, schema, cap = 100), results)
  # convert = FALSE would hand back a list; there is no table to mark
  expect_identical(mark_truncated_rows(list(1), schema, cap = 100), list(1))
  expect_identical(mark_truncated_rows(tibble::tibble(), schema, cap = 100), tibble::tibble())
})

test_that("mark_truncated_rows reads arrays and nested objects as blank when empty", {
  # The shape from the issue: the content sits inside a type_array(), so a
  # cut-off response converts to a zero-row tibble, not NA
  schema <- ellmer::type_object(
    domains = ellmer::type_array(ellmer::type_object(name = ellmer::type_string())),
    meta = ellmer::type_object(a = ellmer::type_string(), b = ellmer::type_integer())
  )
  converted <- ellmer:::convert_from_type(
    list(
      NULL,
      list(domains = list(list(name = "x")), meta = list(a = "q", b = 1L)),
      list(domains = list(), meta = list(a = "q", b = 1L))
    ),
    ellmer::type_array(schema)
  )
  expect_equal(NROW(converted$domains[[1]]), 0)
  converted$input_tokens <- c(5, 5, 5)
  converted$output_tokens <- c(4096, 4096, 4096)
  converted$cached_input_tokens <- c(0, 0, 0)

  expect_warning(out <- mark_truncated_rows(converted, schema, cap = 4096), "1 response")
  expect_s3_class(out$.error[[1]], "simpleError")
  expect_null(out$.error[[2]])
  # An empty array beside a filled nested object is an answer, not a blank
  expect_null(out$.error[[3]])
})

test_that("a declared max_tokens lets the structured path flag a cut-off response", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) structure(list(), class = "Chat"))
  mockery::stub(tsc, "warn_unenforced_schema", function(...) invisible(NULL))
  mockery::stub(tsc, "ellmer::parallel_chat_structured", function(chat, prompts, type, ...) {
    calls$args <- list(...)
    tibble::tibble(
      score = c(NA_real_, 0.7),
      input_tokens = c(50, 40), output_tokens = c(100, 12), cached_input_tokens = c(0, 0)
    )
  })
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(),
                model = "anthropic/claude-sonnet-5",
                params = ellmer::params(max_tokens = 100)),
    "used the whole"
  )

  # Token counts were requested for the check, and not handed on
  expect_true(isTRUE(calls$args$include_tokens))
  expect_false("output_tokens" %in% names(result))
  # The cut-off row is a failure with a reason, not grounds for a JSON re-run
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  failures <- qlm_failures(result)
  expect_equal(nrow(failures), 1)
  expect_match(failures$reason, "max_tokens")
  expect_equal(result$score[[2]], 0.7)
})

test_that("without a declared limit the structured path asks for nothing extra", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) structure(list(), class = "Chat"))
  mockery::stub(tsc, "warn_unenforced_schema", function(...) invisible(NULL))
  mockery::stub(tsc, "ellmer::parallel_chat_structured", function(chat, prompts, type, ...) {
    calls$args <- list(...)
    tibble::tibble(score = 0.7)
  })
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f("a", structured_test_codebook(), model = "anthropic/claude-sonnet-5")

  expect_null(calls$args$include_tokens)
  expect_equal(result$score, 0.7)
})


test_that("structured = 'structured' aborts rather than falling back", {
  skip_if_not_installed("mockery")

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(errors = "the endpoint said no"))

  expect_error(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
      structured = "structured"),
    "the endpoint said no"
  )
})


test_that("auto cannot fall back under batch, and says so", {
  skip_if_not_installed("mockery")

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(errors = "batch call failed"))

  expect_error(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini", batch = TRUE),
    "has no batch path"
  )
})


test_that("the DashScope json-word rejection retries structured before falling back", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  json_word <- paste(
    "<400> InternalError.Algo.InvalidParameter: 'messages' must contain the word",
    "'json' in some form, to use 'response_format' of type 'json_object'"
  )
  # Fails once with the json-word error, succeeds on the retry
  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(errors = json_word, calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai_compatible/qwen-flash",
              base_url = "https://example.com/v1")

  # Two structured attempts, and enforcement is kept rather than abandoned
  expect_equal(calls$n, 2)
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("an unrelated error falls back without a second structured attempt", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(errors = "HTTP 500 Internal Server Error.", calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  suppressWarnings(
    f("a", structured_test_codebook(), model = "openai_compatible/x",
      base_url = "https://example.com/v1")
  )

  expect_equal(calls$n, 1)
  expect_true(calls$json)
})


test_that("max_retries reaches the JSON path under auto", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(errors = "nope"))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  suppressWarnings(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini", max_retries = 7)
  )
  expect_equal(calls$max_retries, 7)
})


test_that("the enforcement note fires for unverifiable endpoints only", {
  skip_if_not_installed("mockery")
  # The note is once-per-session; disable that so the test does not depend on
  # whether something earlier in the suite already consumed it
  withr::local_options(quallmer.quiet_schema_note = FALSE,
                       rlib_message_verbosity = "verbose")

  fake_chat <- function(provider_class) {
    list(get_provider = function() structure(list(), class = c(provider_class, "S7_object")))
  }

  # A provider whose own chat_body() uses an enforced mechanism: nothing to say
  expect_silent(
    warn_unenforced_schema(fake_chat("ellmer::ProviderOpenAI"), "openai/gpt-4o-mini")
  )
  expect_silent(
    warn_unenforced_schema(fake_chat("ellmer::ProviderAnthropic"), "anthropic/claude")
  )

  # Anything on the generic OpenAI-compatible path is unverified
  expect_message(
    warn_unenforced_schema(fake_chat("ellmer::ProviderOpenAICompatible"),
                           "openai_compatible/kimi-k3"),
    "may accept the output schema without enforcing it"
  )
})


test_that("the enforcement note can be silenced", {
  withr::local_options(quallmer.quiet_schema_note = TRUE)
  fake <- list(get_provider = function() {
    structure(list(), class = c("ellmer::ProviderOpenAICompatible", "S7_object"))
  })

  expect_silent(warn_unenforced_schema(fake, "openai_compatible/kimi-k3"))
})


test_that("the enforcement note survives a provider it cannot inspect", {
  withr::local_options(quallmer.quiet_schema_note = FALSE)
  broken <- list(get_provider = function() stop("no provider"))

  expect_silent(warn_unenforced_schema(broken, "some/model"))
})


test_that("auto validates locally when a failed structured call would be invisible", {
  skip_if_not_installed("mockery")

  # Required properties are all arrays, so there is no scalar field whose
  # absence would reveal a failed structured call
  array_codebook <- qlm_codebook(
    "Test", "Prompt",
    ellmer::type_object(
      claims = ellmer::type_array(ellmer::type_string("A claim"))
    )
  )
  expect_length(required_scalar_fields(array_codebook$schema), 0)

  calls <- new.env()
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) {
    list(get_provider = function() {
      structure(list(), class = c("ellmer::ProviderOpenAICompatible", "S7_object"))
    })
  })
  mockery::stub(tsc, "ellmer::parallel_chat_structured", function(...) {
    calls$structured <- TRUE
    tibble::tibble(claims = list("x"))
  })
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_message(
    result <- f("a", array_codebook, model = "openai_compatible/kimi-k3",
                base_url = "https://example.com/v1"),
    "no required scalar field whose absence would reveal"
  )

  # No request was spent on a call that could not be checked
  expect_null(calls$structured)
  expect_true(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  # ... and the reason is recorded, not just printed
  expect_match(qlm_meta(result, type = "user")$fallback_reason, "cannot be verified")
})


test_that("the undetectable skip applies only where it is warranted", {
  skip_if_not_installed("mockery")

  array_codebook <- qlm_codebook(
    "Test", "Prompt",
    ellmer::type_object(claims = ellmer::type_array(ellmer::type_string("A claim")))
  )
  results <- tibble::tibble(claims = list("x"))

  make_tsc <- function(provider_class) {
    tsc <- try_structured_call
    mockery::stub(tsc, "ellmer::chat", function(...) {
      list(get_provider = function() structure(list(), class = c(provider_class, "S7_object")))
    })
    mockery::stub(tsc, "ellmer::parallel_chat_structured", results)
    tsc
  }

  # An endpoint that enforces by construction needs no local validation
  calls <- new.env()
  f <- qlm_code
  mockery::stub(f, "try_structured_call", make_tsc("ellmer::ProviderOpenAI"))
  mockery::stub(f, "code_handler_json", json_stub(calls))
  expect_silent(f("a", array_codebook, model = "openai/gpt-4o-mini"))
  expect_null(calls$json)

  # And an explicit `structured` is never overridden. The enforcement note may
  # still fire here -- trusting an unverified endpoint is exactly when it
  # should -- so assert on the skip reason rather than on silence.
  calls <- new.env()
  f2 <- qlm_code
  mockery::stub(f2, "try_structured_call", make_tsc("ellmer::ProviderOpenAICompatible"))
  mockery::stub(f2, "code_handler_json", json_stub(calls))
  emitted <- testthat::capture_messages(
    f2("a", array_codebook, model = "openai_compatible/x",
       base_url = "https://example.com/v1", structured = "structured")
  )
  expect_false(any(grepl("no required scalar field", emitted)))
  expect_null(calls$json)
})


test_that("a schema-valid empty array is not mistaken for a failure", {
  skip_if_not_installed("mockery")

  # An empty array is valid JSON Schema output for a required array, and after
  # conversion is indistinguishable from a missing one -- so detection must not
  # guess, and a codebook with a checkable scalar alongside must not fall back
  codebook <- qlm_codebook(
    "Test", "Prompt",
    ellmer::type_object(
      score  = ellmer::type_number("Score"),
      claims = ellmer::type_array(ellmer::type_string("A claim"))
    )
  )
  results <- tibble::tibble(score = c(1, 2), claims = list(character(0), character(0)))

  expect_false(all_required_missing(results, codebook$schema))
  expect_equal(n_incomplete(results, codebook$schema), 0)

  calls <- new.env()
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) {
    list(get_provider = function() {
      structure(list(), class = c("ellmer::ProviderOpenAICompatible", "S7_object"))
    })
  })
  mockery::stub(tsc, "ellmer::parallel_chat_structured", results)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  mockery::stub(f, "code_handler_json", json_stub(calls))

  suppressMessages(result <- f(c("a", "b"), codebook, model = "openai_compatible/x",
                               base_url = "https://example.com/v1"))
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("qlm_code rejects convert = FALSE rather than failing downstream", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  expect_error(
    qlm_code("a", codebook, model = "openai/gpt-4o-mini", convert = FALSE),
    "is not supported"
  )
  # TRUE is the default and must still be accepted
  skip_if_not_installed("mockery")
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "ellmer::parallel_chat_structured", data.frame(score = 0.5))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  expect_s3_class(
    f("a", codebook, model = "openai/gpt-4o-mini", convert = TRUE),
    "qlm_coded"
  )
})


test_that("qlm_code forwards params and api_args to ellmer unchanged", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Which of the two a setting belongs in is ellmer's and the provider's
  # business. quallmer must not inspect or rewrite either object -- notably it
  # must not "helpfully" move top_k out of params, even though ellmer maps that
  # onto the unrelated OpenAI field top_logprobs for OpenAI-compatible
  # providers.
  user_params <- ellmer::params(temperature = 0.6, top_p = 0.95, top_k = 20)
  user_api_args <- list(top_k = 20, min_p = 0, enable_thinking = TRUE)

  seen <- NULL
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) {
    seen <<- list(...)
    structure(list(), class = "Chat")
  })
  mockery::stub(tsc, "ellmer::parallel_chat_structured", data.frame(score = 0.5))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  f("a", codebook, model = "openai_compatible/qwen3-max",
    base_url = "https://example.com/v1",
    params = user_params, api_args = user_api_args)

  expect_identical(seen$params, user_params)
  expect_identical(seen$api_args, user_api_args)
  expect_equal(seen$base_url, "https://example.com/v1")
})


test_that("the JSON path forwards api_args too, adding only the response format", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  user_params <- ellmer::params(temperature = 0.6)

  seen <- NULL
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) {
    seen <<- list(...)
    structure(list(), class = "Chat")
  })
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    list(text = "{\"score\":1}", error = NA_character_, status = NA_integer_,
         usage = matrix(0, 1, 4, dimnames = list(
           NULL, c("input_tokens", "output_tokens", "cached_input_tokens", "cost"))))
  })

  h(x = "a", codebook = codebook, model = "openai_compatible/kimi-k3",
    chat_args = list(params = user_params,
                     api_args = list(reasoning_effort = "max")),
    execution_args = list())

  expect_identical(seen$params, user_params)
  expect_equal(seen$api_args$reasoning_effort, "max")
  # JSON mode is the one thing this path must set
  expect_equal(seen$api_args$response_format, list(type = "json_object"))
})


# Unit identifiers must be unique ---------------------------------------------

test_that("qlm_code rejects duplicated input names before spending a request", {
  skip_if_not_installed("mockery")
  f <- qlm_code
  mockery::stub(f, "try_structured_call", function(...) stop("a request was made"))

  expect_error(
    f(c(a = "x", a = "y"), structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "must be unique"
  )
})

test_that("check_ids refuses missing identifiers before repeated ones", {
  expect_silent(check_ids(c("a", "b", "c")))
  expect_silent(check_ids(1:3))
  expect_error(check_ids(c("a", NA, "c")), "must not be missing")
  expect_error(check_ids(c("a", "", "c")), "must not be missing")
  expect_error(check_ids(c(NA, "a", "a")), "must not be missing")
  expect_error(check_ids(c("a", "b", "a")), "must be unique")
  expect_error(check_ids(c("a", "b", "a")), "\\ba\\b")

  # Factors are checked by their labels, including the empty one
  expect_silent(check_ids(factor(c("a", "b"))))
  expect_error(check_ids(factor(c("", "u1"))), "must not be missing")
  expect_error(check_ids(factor(c("a", "a"))), "must be unique")
  # Only a plain vector can be a key
  expect_error(check_ids(list("a", NULL)), "character, factor or numeric")
  expect_error(check_ids(matrix(1:4, 2)), "character, factor or numeric")
})

test_that("an empty factor label is refused as an identifier end to end", {
  a <- data.frame(.id = factor(c("", "u1")), score = c(0, 1))
  expect_error(as_qlm_coded(a, name = "A"), "must not be missing")
})

test_that("selecting .id away returns a plain tibble, not a broken coded object", {
  x <- as_qlm_coded(data.frame(.id = c("a", "b"), score = c(1, 0)), name = "A")
  projected <- x["score"]
  expect_false(inherits(projected, "qlm_coded"))
  expect_false(inherits(projected, "qlm_humancoded"))
  expect_s3_class(projected, "tbl_df")
  expect_null(attr(projected, "meta"))
  expect_equal(names(projected), "score")
  # ... whereas keeping it keeps the object
  kept <- x[c(".id", "score")]
  expect_s3_class(kept, "qlm_coded")
  expect_false(is.null(attr(kept, "meta")))
  # and a vector comes back as a vector
  expect_equal(x[, "score", drop = TRUE], c(1, 0))
  # Selecting the key twice is refused rather than kept as a classed object
  expect_error(x[c(".id", ".id")], "exactly one")
  expect_error(x[c(".id", "score", ".id")], "exactly one")
})

test_that("new_qlm_coded requires exactly one .id column", {
  codebook <- structured_test_codebook()
  two <- data.frame(id = c("a", "b"), .id = c("x", "y"), score = c(1, 2))
  expect_error(
    new_qlm_coded(
      results = two, codebook = codebook, data = c("a", "b"), input_type = "text",
      chat_args = list(name = "test/model"), execution_args = list(),
      metadata = list(timestamp = Sys.time(), n_units = 2),
      name = "run", call = quote(qlm_code(...)), parent = NULL
    ),
    "exactly one"
  )
})

test_that("new_qlm_coded rejects a table whose .id repeats", {
  codebook <- structured_test_codebook()
  expect_error(
    new_qlm_coded(
      results = data.frame(id = c("d1", "d1", "d2"), score = c(1, 2, 3)),
      codebook = codebook, data = c("a", "b", "c"), input_type = "text",
      chat_args = list(name = "test/model"), execution_args = list(),
      metadata = list(timestamp = Sys.time(), n_units = 3),
      name = "run", call = quote(qlm_code(...)), parent = NULL
    ),
    "must be unique"
  )
})


test_that("check_qlm_coded verifies what every function relies on", {
  x <- as_qlm_coded(data.frame(.id = c("a", "b"), score = c(1, 0)), name = "A")
  expect_identical(check_qlm_coded(x), x)

  expect_error(check_qlm_coded(data.frame(.id = "a")), "must be a")

  no_meta <- x
  attr(no_meta, "meta") <- NULL
  expect_error(check_qlm_coded(no_meta), "no run metadata")

  no_id <- x
  names(no_id)[names(no_id) == ".id"] <- "id"
  expect_error(check_qlm_coded(no_id), "exactly one")

  forged <- x
  forged$.id <- c("a", "a")
  expect_error(check_qlm_coded(forged), "must be unique")
  forged$.id <- c(NA, "b")
  expect_error(check_qlm_coded(forged), "must not be missing")

  # The message names the object the caller passed
  expect_error(check_qlm_coded(forged, what = "{.arg gold}"), "gold")
})

test_that("every entry point refuses an object a row operation has left with a repeated key", {
  x <- as_qlm_coded(data.frame(.id = c("a", "b"), score = c(1, 0)), name = "A")
  # vctrs row slicing, which dplyr's verbs are built on, keeps the class and
  # attributes and does not go through `[`; so does base rbind()
  doubled <- vctrs::vec_slice(x, c(1, 1))
  expect_s3_class(doubled, "qlm_coded")
  expect_false(is.null(attr(doubled, "meta")))
  expect_s3_class(rbind(x, x), "qlm_coded")
  expect_error(qlm_failures(rbind(x, x)), "must be unique")

  expect_error(qlm_failures(doubled), "must be unique")
  expect_error(qlm_compare(doubled, x, by = "score", level = "interval"), "must be unique")
  expect_error(qlm_validate(doubled, gold = x, by = "score", level = "interval"), "must be unique")
  expect_error(qlm_trail(doubled), "must be unique")
  expect_error(qlm_replicate(doubled), "must be unique")
})


# cost that cannot be priced (#135) --------------------------------------------

# qlm_code() with the structured call stubbed to return `results`, and the
# chat built for real, off the environment and sending nothing, so that the
# diagnosis reads the provider the run would use. Callers pin
# structured = "structured": DeepSeek defaults to the JSON path.
coding_run <- function(results) {
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::parallel_chat_structured", results)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  f
}
offline <- function() list(Authorization = "Bearer x")

test_that("qlm_code says once, before the run, why cost will be NA (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8), cost = c(NA_real_, NA_real_))

  f <- coding_run(results)
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, include_cost = TRUE, structured = "structured"),
    "no prices for DeepSeek models"
  )
  expect_equal(qlm_meta(coded)$cost_note, "NA (ellmer has no prices for DeepSeek models)")
  expect_output(print(coded), "# Cost:     NA (ellmer has no prices for DeepSeek models)",
                fixed = TRUE)
})

test_that("qlm_code's cost message says whether token counts are recorded (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # include_cost alone: no token columns come back, and the message says so
  f <- coding_run(data.frame(score = c(0.5, 0.8), cost = c(NA_real_, NA_real_)))
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, include_cost = TRUE, structured = "structured"),
    "Supply the provider's published rates as `prices`"
  )
  expect_false("input_tokens" %in% names(coded))

  # With include_tokens the counts are there, and the message says that instead
  with_tokens <- data.frame(score = c(0.5, 0.8), input_tokens = c(10, 12),
                            output_tokens = c(3, 4), cached_input_tokens = c(0, 0),
                            cost = c(NA_real_, NA_real_))
  f <- coding_run(with_tokens)
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, include_cost = TRUE, include_tokens = TRUE,
               structured = "structured"),
    "Token counts are recorded; supply"
  )
  expect_true("input_tokens" %in% names(coded))
})

test_that("qlm_code is silent about cost when it was not asked for, or is priced (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8))

  # Not asked for: the lookup is not even made
  f <- coding_run(results)
  expect_no_message(coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
                               credentials = offline, structured = "structured"))
  expect_null(qlm_meta(coded)$cost_note)
  expect_output(print(coded), "# Units:")
  expect_no_match(paste(capture.output(print(coded)), collapse = "\n"), "# Cost:")

  # Asked for and priced: nothing to say
  expect_no_message(coded <- f(c("a", "b"), codebook, model = "openai/gpt-4.1-mini",
                               credentials = offline, include_cost = TRUE,
                               structured = "structured"))
  expect_null(qlm_meta(coded)$cost_note)
})


# cost from supplied rates (#135) ----------------------------------------------

# coding_run() from above, with the execution arguments the structured call
# received recorded in `seen`.
priced_run <- function(results, seen) {
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::parallel_chat_structured", function(chat, prompts, type, ...) {
    seen$execution_args <- list(...)
    results
  })
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  f
}

test_that("qlm_code costs an unpriced run from supplied rates (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8), input_tokens = c(1e6, 2e6),
                        output_tokens = c(1e5, 1e5), cached_input_tokens = c(0, 5e5),
                        cost = c(NA_real_, NA_real_))
  seen <- new.env()

  f <- priced_run(results, seen)
  # No "cost will be NA" message: it will not be
  expect_no_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, structured = "structured",
               prices = c(input = 1, output = 10, cached_input = 0.1))
  )

  # Costed by ellmer's sum, per million tokens
  expect_equal(coded$cost, c(2, (2e6 + 5e5 * 0.1 + 1e6) / 1e6))
  # Supplying rates asked ellmer for tokens and cost
  expect_true(isTRUE(seen$execution_args$include_tokens))
  expect_true(isTRUE(seen$execution_args$include_cost))
  # The rates travel with the object, and print says where the cost came from
  expect_equal(qlm_meta(coded)$prices, c(input = 1, output = 10, cached_input = 0.1))
  expect_equal(qlm_meta(coded)$cost_note,
               "from supplied rates: $1 input, $10 output, $0.1 cached input, per million tokens")
  expect_output(print(coded), "# Cost:     from supplied rates: $1 input", fixed = TRUE)
})

test_that("qlm_code leaves ellmer's own prices in charge (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8), input_tokens = c(1e6, 2e6),
                        output_tokens = c(1e5, 1e5), cached_input_tokens = c(0, 0),
                        cost = c(0.3, 0.6))

  f <- priced_run(results, new.env())
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "openai/gpt-4.1-mini",
               credentials = offline, structured = "structured",
               prices = c(input = 1, output = 10)),
    "prices.*is not used"
  )
  expect_equal(coded$cost, c(0.3, 0.6))
  expect_null(qlm_meta(coded)$prices)
  expect_null(qlm_meta(coded)$cost_note)
})

test_that("qlm_code rejects malformed prices before any request (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  f <- priced_run(data.frame(score = 1), new.env())
  expect_error(
    f("a", codebook, model = "openai/gpt-4.1-mini", credentials = offline,
      prices = c(input = 1), structured = "structured"),
    "Missing: output"
  )
})
