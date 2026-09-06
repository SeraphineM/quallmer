test_that("qlm_replicate errors on non-qlm_coded input", {
  skip_if_not_installed("ellmer")

  expect_error(
    qlm_replicate(data.frame(a = 1)),
    "qlm_coded"
  )
})

test_that("qlm_replicate works with no overrides", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create a mock qlm_coded object
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Mock qlm_code to avoid actual API calls
  mockery::stub(qlm_replicate, "qlm_code", coded)

  result <- qlm_replicate(coded)

  expect_s3_class(result, "qlm_coded")
  expect_equal(attr(result, "meta")$object$parent, "original")
  expect_identical(attr(result, "codebook"), attr(coded, "codebook"))
  expect_equal(attr(result, "meta")$object$chat_args$name, attr(coded, "meta")$object$chat_args$name)
})

test_that("qlm_replicate applies model override", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create a mock qlm_coded object
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result with new model
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "gpt-4o-mini",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Mock qlm_code to return expected result
  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, model = "openai/gpt-4o-mini")

  expect_equal(attr(result, "meta")$object$chat_args$name, "openai/gpt-4o-mini")
  expect_equal(attr(result, "meta")$object$parent, "original")
})

test_that("qlm_replicate applies codebook override", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create original mock
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook1 <- qlm_codebook("Test1", "Prompt1", type_obj)
  codebook2 <- qlm_codebook("Test2", "Prompt2", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook1,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result with new codebook
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook2,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "replication_1",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Mock qlm_code
  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, codebook = codebook2)

  expect_equal(attr(result, "codebook"), codebook2)
})

test_that("qlm_replicate applies name override", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create mock
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "my_replication",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, name = "my_replication")

  expect_equal(attr(result, "meta")$user$name, "my_replication")
})

test_that("qlm_replicate auto-generates name from model", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create mock
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "anthropic/claude-sonnet-4-20250514"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "claude-sonnet-4-20250514",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, model = "anthropic/claude-sonnet-4-20250514")

  expect_equal(attr(result, "meta")$user$name, "claude-sonnet-4-20250514")
})

test_that("qlm_replicate passes through additional arguments", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create mock
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result with temperature override
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(temperature = 0.7),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "replication_1",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, temperature = 0.7)

  expect_equal(attr(result, "meta")$object$execution_args$temperature, 0.7)
})

test_that("qlm_replicate stores correct call", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mockery")

  # Create mock
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "gpt-4o-mini",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, model = "openai/gpt-4o-mini")

  expect_true(inherits(attr(result, "meta")$object$call, "call"))
  expect_true(grepl("qlm_replicate", deparse(attr(result, "meta")$object$call)[1]))
})


test_that("qlm_replicate preserves batch flag by default", {
  skip_if_not_installed("ellmer")

  # Create mock with batch=TRUE
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(path = "/tmp/batch"),
    batch = TRUE,
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result that also has batch=TRUE
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(path = "/tmp/batch"),
    batch = TRUE,
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "replication_1",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded)

  # Verify batch flag is preserved
  expect_true(attr(result, "meta")$object$batch)
})


test_that("qlm_replicate allows batch override to TRUE", {
  skip_if_not_installed("ellmer")

  # Create mock with batch=FALSE
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    batch = FALSE,
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result with batch=TRUE
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(path = "/tmp/batch"),
    batch = TRUE,
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "replication_1",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, batch = TRUE, path = "/tmp/batch")

  # Verify batch flag was overridden
  expect_true(attr(result, "meta")$object$batch)
})


test_that("qlm_replicate allows batch override to FALSE", {
  skip_if_not_installed("ellmer")

  # Create mock with batch=TRUE
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:5, category = c("A", "B", "A", "B", "C"))
  coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(path = "/tmp/batch"),
    batch = TRUE,
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Create expected result with batch=FALSE
  expected_result <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = paste0("text", 1:5),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(max_active = 5),
    batch = FALSE,
    metadata = list(timestamp = Sys.time(), n_units = 5),
    name = "replication_1",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  mockery::stub(qlm_replicate, "qlm_code", expected_result)

  result <- qlm_replicate(coded, batch = FALSE, max_active = 5)

  # Verify batch flag was overridden
  expect_false(attr(result, "meta")$object$batch)
})


test_that("qlm_replicate restores chat_args, not just execution_args", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  coded <- new_qlm_coded(
    results = data.frame(id = 1:2, score = c(0.5, 0.8)),
    codebook = codebook,
    data = c("a", "b"),
    input_type = "text",
    chat_args = list(
      name = "openai/gpt-4o-mini",
      params = list(temperature = 0),
      api_args = list(seed = 42),
      base_url = "https://example.com/v1"
    ),
    execution_args = list(max_active = 3),
    batch = FALSE,
    metadata = list(n_units = 2),
    name = "run1",
    call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })
  f(coded, name = "run2")

  # Everything the original passed to ellmer::chat() comes back
  expect_equal(seen$params, list(temperature = 0))
  expect_equal(seen$api_args, list(seed = 42))
  expect_equal(seen$base_url, "https://example.com/v1")
  # ... alongside the execution arguments, which already worked
  expect_equal(seen$max_active, 3)
  # `name` in chat_args is the model; it reaches qlm_code() as `model`, and
  # `name` itself stays the run name rather than being restored over it
  expect_equal(seen$model, "openai/gpt-4o-mini")
  expect_equal(seen$name, "run2")
})


test_that("qlm_replicate lets overrides win over restored chat_args", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini", params = list(temperature = 0)),
    execution_args = list(max_active = 3),
    batch = FALSE, metadata = list(n_units = 1), name = "run1",
    call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })
  f(coded, params = list(temperature = 1), max_active = 8, name = "run2")

  expect_equal(seen$params, list(temperature = 1))
  expect_equal(seen$max_active, 8)
})


test_that("qlm_replicate drops provider-bound arguments when provider changes", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  old_credentials <- function() "old-provider-key"

  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(
      name = "deepseek/deepseek-v4-flash",
      params = list(temperature = 0),
      echo = "none",
      credentials = old_credentials,
      base_url = "https://api.deepseek.example",
      api_args = list(seed = 42),
      api_headers = c(`X-Provider` = "deepseek")
    ),
    execution_args = list(max_active = 3),
    batch = FALSE, metadata = list(n_units = 1), name = "run1",
    call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })
  expect_message(
    f(coded, model = "openai/gpt-4o-mini", name = "run2"),
    paste0(
      "not carrying over endpoint-specific arguments: `credentials`, ",
      "`base_url`, `api_args`, `api_headers`"
    )
  )

  expect_equal(seen$params, list(temperature = 0))
  expect_equal(seen$echo, "none")
  expect_equal(seen$max_active, 3)
  expect_false(any(c("credentials", "base_url", "api_args", "api_headers") %in% names(seen)))

  # Explicit settings for the replacement provider still pass through.
  replacement_credentials <- function() "replacement-key"
  expect_message(
    f(coded, model = "openai/gpt-4o-mini", name = "run3",
      credentials = replacement_credentials,
      base_url = "https://api.openai.example"),
    "not carrying over endpoint-specific arguments: `api_args`, `api_headers`"
  )
  expect_identical(seen$credentials, replacement_credentials)
  expect_equal(seen$base_url, "https://api.openai.example")
})


test_that("qlm_replicate carries json_retries only where it applies", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  make <- function(model) {
    new_qlm_coded(
      results = data.frame(id = 1L, score = 0.5),
      codebook = codebook, data = "a", input_type = "text",
      chat_args = list(name = model),
      execution_args = list(),
      batch = FALSE,
      metadata = list(n_units = 1, backend = "json_mode", json_retries = 5L),
      name = "run1", call = quote(qlm_code())
    )
  }

  seen <- NULL
  capture <- function(...) {
    seen <<- list(...)
    make("deepseek/deepseek-chat")
  }

  # Same provider: the original setting is reproduced
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", capture)
  f(make("deepseek/deepseek-chat"), name = "run2")
  expect_equal(seen$json_retries, 5L)

  # JSON mode is now reachable for any provider, so the setting still applies
  # after a model change
  seen <- NULL
  f(make("deepseek/deepseek-chat"), model = "openai/gpt-4o-mini", name = "run3")
  expect_equal(seen$json_retries, 5L)
})


test_that("qlm_replicate reproduces the path taken, not the mode requested", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  make <- function(..., model = "openai_compatible/kimi-k3") {
    new_qlm_coded(
      results = data.frame(id = 1L, score = 0.5),
      codebook = codebook, data = "a", input_type = "text",
      chat_args = list(name = model),
      execution_args = list(), batch = FALSE,
      metadata = c(list(n_units = 1), list(...)),
      name = "run1", call = quote(qlm_code())
    )
  }

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    make(structured = "json", backend = "json_mode")
  })

  # The case that matters: the run asked for "auto" and the endpoint failed, so
  # it validated locally. Replicating with "auto" would let an intermittently
  # conforming endpoint take the structured path instead and skip that
  # validation, making the two runs incomparable.
  f(make(structured = "auto", backend = "json_mode", json_retries = 4L), name = "run2")
  expect_equal(seen$structured, "json")
  expect_equal(seen$json_retries, 4L)

  # Likewise the other way: a run that did take the structured path replicates
  # as "structured", so a failure surfaces rather than being papered over by a
  # fallback the original never used
  seen <- NULL
  f(make(structured = "auto", backend = "structured"), name = "run3")
  expect_equal(seen$structured, "structured")
  # json_retries has no meaning there, and supplying it is an error
  expect_false("json_retries" %in% names(seen))

  # An explicit override still wins
  seen <- NULL
  f(make(structured = "auto", backend = "json_mode"), structured = "auto", name = "run4")
  expect_equal(seen$structured, "auto")
})


test_that("qlm_replicate leaves the mode alone for objects with no recorded backend", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Coded before `backend` was recorded at all
  legacy <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    legacy
  })

  f(legacy, name = "run2")
  expect_false("structured" %in% names(seen))
})


test_that("qlm_replicate drops credentials when the endpoint changes but the prefix does not", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Kimi through Moonshot. Qwen through Alibaba Model Studio has the same
  # prefix, so only base_url distinguishes the two services.
  moonshot <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(
      name = "openai_compatible/kimi-k3",
      base_url = "https://api.moonshot.ai/v1",
      credentials = function() list(Authorization = "Bearer moonshot-key"),
      api_headers = c(`X-Trace` = "1"),
      api_args = list(reasoning_effort = "max"),
      params = list(temperature = 0)
    ),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    moonshot
  })

  # Repointing at DashScope keeps the prefix identical, so a prefix-only check
  # would send Moonshot's credential to Alibaba
  expect_message(
    f(moonshot, base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1",
      name = "run2"),
    "not carrying over endpoint-specific arguments"
  )

  expect_false("credentials" %in% names(seen))
  expect_false("api_headers" %in% names(seen))
  expect_false("api_args" %in% names(seen))
  # The new endpoint is used, and portable settings still travel
  expect_equal(seen$base_url, "https://dashscope-intl.aliyuncs.com/compatible-mode/v1")
  expect_equal(seen$params, list(temperature = 0))
})


test_that("qlm_replicate keeps credentials when the endpoint is unchanged", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(
      name = "openai_compatible/kimi-k3",
      base_url = "https://api.moonshot.ai/v1",
      credentials = function() list(Authorization = "Bearer moonshot-key")
    ),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })

  # A different model on the same host is the same endpoint: nothing to drop
  expect_silent(f(coded, model = "openai_compatible/kimi-k2.6", name = "run2"))
  expect_true("credentials" %in% names(seen))
  expect_equal(seen$base_url, "https://api.moonshot.ai/v1")

  # And a plain replication of the same run keeps everything
  seen <- NULL
  expect_silent(f(coded, name = "run3"))
  expect_true("credentials" %in% names(seen))
})


test_that("qlm_replicate keeps an explicitly supplied credential across an endpoint change", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(
      name = "openai_compatible/kimi-k3",
      base_url = "https://api.moonshot.ai/v1",
      credentials = function() list(Authorization = "Bearer moonshot-key")
    ),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })

  # Replacing the credential alongside the endpoint is the supported route,
  # and is not reported as omitted
  expect_silent(
    f(coded,
      base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1",
      credentials = function() list(Authorization = "Bearer dashscope-key"),
      name = "run2")
  )
  expect_equal(
    seen$credentials()$Authorization, "Bearer dashscope-key"
  )
})


test_that("qlm_replicate completes the replication as its parent was completed", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1:2, category = c("A", "B")),
    codebook = codebook, data = c("t1", "t2"), input_type = "text",
    chat_args = list(name = "test/model"), execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "original", call = quote(qlm_code(...)), parent = NULL
  )

  seen <- new.env()
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", coded)
  mockery::stub(f, "replay_backfill", function(result, parent, backfill = NULL) {
    seen$parent <- parent
    seen$backfill <- backfill
    result
  })

  f(coded)
  expect_identical(seen$parent, coded)
  expect_null(seen$backfill)

  f(coded, backfill = FALSE)
  expect_false(seen$backfill)
})

test_that("qlm_replicate leaves the coding path to a new provider", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1:2, category = c("A", "B")),
    codebook = codebook, data = c("t1", "t2"), input_type = "text",
    chat_args = list(name = "deepseek/deepseek-chat"), execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 2, backend = "json_mode",
                    json_retries = 3L),
    name = "original", call = quote(qlm_code(...)), parent = NULL
  )

  seen <- new.env()
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen$args <- list(...)
    coded
  })
  mockery::stub(f, "replay_backfill", function(result, ...) result)

  # Same provider: the JSON path and its retries travel
  f(coded)
  expect_equal(seen$args$structured, "json")
  expect_equal(seen$args$json_retries, 3L)

  # Another provider: the path does not, so qlm_code() chooses for it; the
  # retry budget still applies wherever JSON mode is taken
  f(coded, model = "openai/gpt-4o-mini")
  expect_null(seen$args$structured)
  expect_equal(seen$args$json_retries, 3L)

  # The same prefix at another base_url is another endpoint too: Qwen through
  # DashScope and Kimi through Moonshot enforce a schema quite differently
  compatible <- new_qlm_coded(
    results = data.frame(id = 1:2, category = c("A", "B")),
    codebook = codebook, data = c("t1", "t2"), input_type = "text",
    chat_args = list(name = "openai_compatible/qwen3.5",
                     base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 2, backend = "structured"),
    name = "original", call = quote(qlm_code(...)), parent = NULL
  )
  f(compatible)
  expect_equal(seen$args$structured, "structured")
  f(compatible, model = "openai_compatible/kimi-k3", base_url = "https://api.moonshot.ai/v1")
  expect_null(seen$args$structured)
})

test_that("qlm_replicate validates backfill before coding anything", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1:2, category = c("A", "B")),
    codebook = codebook, data = c("t1", "t2"), input_type = "text",
    chat_args = list(name = "test/model"), execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "original", call = quote(qlm_code(...)), parent = NULL
  )
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) stop("a paid call was made"))
  expect_error(f(coded, backfill = "yes"), "single non-negative integer")
  expect_error(f(coded, backfill = -1), "single non-negative integer")
  expect_error(f(coded, backfill = 1.5), "single non-negative integer")
  expect_error(f(coded, backfill = Inf), "single non-negative integer")
  expect_error(f(coded, backfill = .Machine$integer.max + 1), "single non-negative integer")
})


test_that("qlm_replicate treats an explicit base_url = NULL as a change of endpoint", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  proxied <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(
      name = "openai/gpt-4o-mini",
      base_url = "https://proxy.example/v1",
      credentials = function() list(Authorization = "Bearer proxy-secret"),
      api_args = list(reasoning_effort = "max"),
      params = list(temperature = 0)
    ),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1, backend = "structured"),
    name = "run1", call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    proxied
  })

  # Clearing base_url points the run at OpenAI's own host. The proxy's
  # credential and request arguments must not go there, and the path the
  # proxy took says nothing about what the default host accepts.
  expect_message(
    f(proxied, base_url = NULL, name = "run2"),
    "to the provider's default endpoint; not carrying over"
  )
  expect_false("base_url" %in% names(seen))
  expect_false("credentials" %in% names(seen))
  expect_false("api_args" %in% names(seen))
  expect_null(seen$structured)
  expect_equal(seen$params, list(temperature = 0))
})


test_that("qlm_replicate hands an integer backfill on as given", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )
  seen <- new.env()
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", coded)
  mockery::stub(f, "replay_backfill", function(result, parent, backfill = NULL) {
    seen$backfill <- backfill
    result
  })
  f(coded, backfill = 3)
  expect_equal(seen$backfill, 3)
  f(coded, backfill = 0)
  expect_equal(seen$backfill, 0)
})


# supplied prices (#135) -------------------------------------------------------

priced_coded <- function() {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  new_qlm_coded(
    results = data.frame(id = 1:2, score = c(0.5, 0.8)),
    codebook = codebook,
    data = c("a", "b"),
    input_type = "text",
    chat_args = list(name = "deepseek/deepseek-chat"),
    execution_args = list(include_tokens = TRUE, include_cost = TRUE),
    batch = FALSE,
    metadata = list(n_units = 2, prices = c(input = 1, output = 10, cached_input = 0.1)),
    name = "run1",
    call = quote(qlm_code())
  )
}

test_that("qlm_replicate carries supplied prices to the same model, not to another (#135)", {
  coded <- priced_coded()
  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })

  # Same model: the rates come back
  f(coded, name = "again")
  expect_equal(seen$prices, c(input = 1, output = 10, cached_input = 0.1))

  # Different model: dropped, with a note
  expect_message(f(coded, model = "deepseek/deepseek-reasoner", name = "other"),
                 "Not carrying over `prices`: the model differs")
  expect_null(seen$prices)

  # Same model string reached through another endpoint: a different price
  # list, so dropped too
  expect_message(
    f(coded, base_url = "https://other.example/v1", credentials = function() list(),
      name = "moved"),
    "the endpoint differs"
  )
  expect_null(seen$prices)

  # Same model and endpoint, run as a batch: providers price batches
  # differently, so dropped
  expect_message(f(coded, batch = TRUE, name = "batched"), "the batch setting differs")
  expect_null(seen$prices)

  # Several changes are all named
  expect_message(f(coded, model = "deepseek/deepseek-reasoner", batch = TRUE, name = "both"),
                 "the model, batch setting differ")

  # A service tier is priced on its own: from unset (ellmer's "auto") to an
  # explicit tier, dropped; the same tier again, carried
  expect_message(f(coded, service_tier = "priority", name = "fast"),
                 "the service tier differs")
  expect_null(seen$prices)
  expect_message(f(coded, service_tier = "default", name = "std"),
                 "the service tier differs")
  expect_null(seen$prices)
  f(coded, service_tier = "auto", name = "same")
  expect_equal(seen$prices, c(input = 1, output = 10, cached_input = 0.1))

  # An original run on an explicit tier carries to the same tier only
  tiered <- priced_coded()
  attr(tiered, "meta")$object$chat_args$service_tier <- "priority"
  f(tiered, name = "again")
  expect_equal(seen$prices, c(input = 1, output = 10, cached_input = 0.1))
  expect_message(f(tiered, service_tier = "flex", name = "cheap"), "the service tier differs")
  expect_null(seen$prices)

  # An explicit override is left alone
  f(coded, model = "deepseek/deepseek-reasoner", prices = c(input = 2, output = 20))
  expect_equal(seen$prices, c(input = 2, output = 20))
})


test_that("qlm_replicate does not send a credential the trail redacted (#154)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(
      name = "openai/gpt-4o-mini",
      api_key = "<redacted>",
      api_headers = c(Authorization = "<redacted>", `anthropic-beta` = "b"),
      params = list(temperature = 0)
    ),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )
  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) { seen <<- list(...); coded })
  mockery::stub(f, "replay_backfill", function(result, ...) result)

  msgs <- capture_messages(f(coded, name = "run2"))
  expect_true(any(grepl("`api_key`, `api_headers` carry values redacted", msgs)))
  expect_false("api_key" %in% names(seen))
  expect_equal(seen$api_headers, c(`anthropic-beta` = "b"))
  expect_equal(seen$params, list(temperature = 0))

  # An explicit value in `...` supersedes the redacted one and is not
  # reported as dropped
  msgs <- capture_messages(f(coded, api_key = "sk-new", name = "run3"))
  expect_true(any(grepl("`api_headers` carries a value redacted", msgs)))
  expect_equal(seen$api_key, "sk-new")
})

test_that("qlm_replicate carries tools on the same endpoint and drops them with the provider (#122)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  web_search <- ellmer::openai_tool_web_search()
  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini", tools = list(web_search)),
    execution_args = list(), batch = FALSE,
    metadata = list(n_units = 1), name = "run1", call = quote(qlm_code())
  )
  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) { seen <<- list(...); coded })
  mockery::stub(f, "replay_backfill", function(result, ...) result)

  # Same endpoint: the run's instrument, tools included, is reproduced
  f(coded, name = "run2")
  expect_identical(seen$tools, list(web_search))

  # Another provider: a hosted tool belongs to its provider, so it is dropped
  # and named, like any endpoint-specific argument
  expect_message(f(coded, model = "anthropic/claude-sonnet-4", name = "run3"),
                 "not carrying over endpoint-specific argument: `tools`")
  expect_null(seen$tools)

  # An explicit replacement is used as given
  lookup <- ellmer::tool(function() "ok", name = "lookup", description = "d")
  suppressMessages(f(coded, model = "anthropic/claude-sonnet-4", tools = lookup, name = "run4"))
  expect_identical(seen$tools, lookup)

  # Read back from a trail, the tools are descriptions, and are not sent
  saved <- coded
  m <- attr(saved, "meta")
  m$object$chat_args$tools <- tool_records(list(web_search))
  attr(saved, "meta") <- m
  msgs <- capture_messages(f(saved, name = "run5"))
  expect_true(any(grepl("`tools` carries a value redacted", msgs)))
  expect_null(seen$tools)
})


# on_error and path-specific execution arguments (#171) ------------------------

# A parent coded on `batch` with `execution_args` recorded, and a stub for
# qlm_code() that records what a replication passes it.
replicate_parent <- function(execution_args, batch = FALSE) {
  type_obj <- ellmer::type_object(category = ellmer::type_string("Category"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  new_qlm_coded(
    results = data.frame(id = 1:2, category = c("A", "B")),
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"),
    execution_args = execution_args,
    batch = batch,
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )
}

replicate_with <- function(seen) {
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(x, codebook, model, ..., batch = FALSE,
                                        name = NULL, notes = NULL) {
    seen$args <- list(...)
    seen$batch <- batch
    replicate_parent(list(), batch = batch)
  })
  f
}

test_that("qlm_replicate replays a recorded on_error; an old parent takes the default (#171)", {
  skip_if_not_installed("mockery")
  seen <- new.env()
  f <- replicate_with(seen)

  # Coded before on_error was recorded: nothing is passed, so qlm_code()'s
  # own default applies to the replication
  f(replicate_parent(list()))
  expect_false("on_error" %in% names(seen$args))

  # Recorded: replayed as it was
  f(replicate_parent(list(on_error = "return", max_active = 5)))
  expect_equal(seen$args$on_error, "return")
  expect_equal(seen$args$max_active, 5)

  # An override in `...` wins
  f(replicate_parent(list(on_error = "return")), on_error = "stop")
  expect_equal(seen$args$on_error, "stop")
})


test_that("qlm_replicate drops inherited arguments the other path cannot take (#171)", {
  skip_if_not_installed("mockery")
  seen <- new.env()
  f <- replicate_with(seen)

  # Parallel to batch: the parallel call's arguments stay behind
  f(replicate_parent(list(on_error = "continue", max_active = 5, include_tokens = TRUE)),
    batch = TRUE, path = "/tmp/batch")
  expect_true(seen$batch)
  expect_false(any(c("on_error", "max_active") %in% names(seen$args)))
  expect_true(seen$args$include_tokens)
  expect_equal(seen$args$path, "/tmp/batch")

  # Batch to parallel: the batch API's arguments stay behind
  f(replicate_parent(list(path = "/tmp/batch", wait = TRUE, include_cost = TRUE), batch = TRUE),
    batch = FALSE)
  expect_false(seen$batch)
  expect_false(any(c("path", "wait") %in% names(seen$args)))
  expect_true(seen$args$include_cost)
})


test_that("an explicit on_error on a batch replication is refused, not dropped (#171)", {
  # Only the inherited value is filtered out; the caller's own override goes
  # through to qlm_code(), which says why it cannot apply, before any request
  expect_error(
    qlm_replicate(replicate_parent(list()), batch = TRUE, on_error = "return"),
    "not supported with `batch = TRUE`"
  )
})


# File inputs (#124) -----------------------------------------------------------

test_that("qlm_replicate checks a file input's hashes before coding it again (#124)", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run <- audio_run(paths)
  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(x, ...) {
    seen <<- x
    run
  })

  expect_s3_class(f(run, name = "rep"), "qlm_coded")
  expect_equal(seen, paths)

  # The path now holds different bytes: refused before anything is coded
  writeBin(as.raw(99:110), paths[["b"]])
  seen <- NULL
  expect_error(f(run, name = "rep2"), 'differs from the one this run coded: "b"')
  expect_null(seen)
})


test_that("qlm_replicate of a run without recorded hashes proceeds with a notice (#124)", {
  paths <- c(a = audio_file())
  legacy <- audio_run(paths, with_hashes = FALSE)
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(x, ...) legacy)
  expect_message(f(legacy, name = "rep"), "cannot be verified")
})


test_that("qlm_replicate needs the registration a run relied on (#124)", {
  withr::defer(reset_registered_input_models())
  paths <- c(a = audio_file())
  run <- audio_run(paths, registered = "google_gemini/gemini-4-ultra",
                   model = "google_gemini/gemini-4-ultra")
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(x, ...) run)

  expect_error(f(run, name = "rep"), "qlm_register_model")
  suppressMessages(qlm_register_model("google_gemini/gemini-4-ultra", input_type = "audio"))
  expect_s3_class(f(run, name = "rep"), "qlm_coded")
})
