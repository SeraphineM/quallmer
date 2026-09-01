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

  mockery::stub(qlm_code, "ellmer::chat", mock_chat)
  mockery::stub(qlm_code, "ellmer::parallel_chat_structured", mock_results)

  # Call with a provider-specific argument (like base_url for openai_compatible)
  qlm_code(c("text1", "text2"), codebook, model = "test/model",
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

  mockery::stub(qlm_code, "ellmer::chat", mock_chat)

  # Mock parallel_chat_structured and verify it's called
  mock_pcs <- mockery::mock(mock_results, cycle = TRUE)
  mockery::stub(qlm_code, "ellmer::parallel_chat_structured", mock_pcs)

  result <- qlm_code(c("text1", "text2"), codebook,
                     model = "test/model", batch = FALSE)

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

  mockery::stub(qlm_code, "ellmer::chat", mock_chat)

  # Mock batch_chat_structured and verify it's called
  mock_bcs <- mockery::mock(mock_results, cycle = TRUE)
  mockery::stub(qlm_code, "ellmer::batch_chat_structured", mock_bcs)

  # Use an execution arg that's valid (convert is in both parallel and batch)
  result <- suppressWarnings(
    qlm_code(c("text1", "text2"), codebook,
             model = "test/model", batch = TRUE,
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

  mockery::stub(qlm_code, "ellmer::chat", mock_chat)
  mockery::stub(qlm_code, "ellmer::parallel_chat_structured", mock_results)

  result <- qlm_code(c("text1", "text2", "text3"), codebook,
                     model = "test/model")

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


test_that("code_handler_for routes only providers that need JSON-mode coding", {
  expect_null(code_handler_for("openai/gpt-4o-mini"))
  expect_null(code_handler_for("anthropic/claude-sonnet-4-5"))
  expect_null(code_handler_for("openai"))

  expect_identical(code_handler_for("deepseek/deepseek-chat"), code_handler_json)
  expect_identical(code_handler_for("deepseek"), code_handler_json)
})


test_that("qlm_code delegates to the handler and records the backend", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  handler_args <- NULL
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           max_retries = 2L) {
    handler_args <<- list(model = model, batch = batch, max_retries = max_retries,
                          chat_args = chat_args, execution_args = execution_args)
    results <- tibble::tibble(score = c(0.5, 0.8))
    attr(results, "qlm_backend_meta") <- list(backend = "json_mode", n_invalid = 0)
    results
  }

  f <- qlm_code
  mockery::stub(f, "code_handler_for", function(...) fake_handler)

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

  mock_chat <- structure(list(), class = "Chat")
  mock_pcs <- mockery::mock(data.frame(score = c(0.5, 0.8)), cycle = TRUE)

  f <- qlm_code
  mockery::stub(f, "ellmer::chat", mock_chat)
  mockery::stub(f, "ellmer::parallel_chat_structured", mock_pcs)

  result <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini")

  mockery::expect_called(mock_pcs, 1)
  expect_null(qlm_meta(result, type = "object")$backend)
})


test_that("qlm_code errors on max_retries only when it is set explicitly", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Explicitly set for a provider that cannot honour it
  expect_error(
    qlm_code(c("a"), codebook, model = "openai/gpt-4o-mini", max_retries = 2),
    "not supported for model"
  )
  # Even when the value happens to equal the default
  expect_error(
    qlm_code(c("a"), codebook, model = "openai/gpt-4o-mini", max_retries = 2L),
    "not supported for model"
  )

  # Left at the default, the same call proceeds
  mock_chat <- structure(list(), class = "Chat")
  f <- qlm_code
  mockery::stub(f, "ellmer::chat", mock_chat)
  mockery::stub(f, "ellmer::parallel_chat_structured", data.frame(score = 0.5))
  expect_s3_class(
    f(c("a"), codebook, model = "openai/gpt-4o-mini"),
    "qlm_coded"
  )
})


test_that("qlm_code passes its max_retries default to the handler", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  seen <- NULL
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           max_retries) {
    seen <<- max_retries
    tibble::tibble(score = 0.5)
  }
  f <- qlm_code
  mockery::stub(f, "code_handler_for", function(...) fake_handler)

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
