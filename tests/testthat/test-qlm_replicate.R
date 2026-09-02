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


test_that("qlm_replicate does not carry tools across a replication", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  coded <- new_qlm_coded(
    results = data.frame(id = 1L, score = 0.5),
    codebook = codebook, data = "a", input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini", tools = list("a tool"),
                     params = list(temperature = 0)),
    execution_args = list(),
    batch = FALSE, metadata = list(n_units = 1), name = "run1",
    call = quote(qlm_code())
  )

  seen <- NULL
  f <- qlm_replicate
  mockery::stub(f, "qlm_code", function(...) {
    seen <<- list(...)
    coded
  })
  f(coded, name = "run2")

  # Provider-specific, so deliberately not round-tripped
  expect_false("tools" %in% names(seen))
  expect_equal(seen$params, list(temperature = 0))
})


test_that("qlm_replicate carries max_retries only where it applies", {
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
      metadata = list(n_units = 1, backend = "json_mode", max_retries = 5L),
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
  expect_equal(seen$max_retries, 5L)

  # Replicating onto a provider that cannot honour it drops it rather than
  # aborting: changing the model is a legitimate thing to do.
  seen <- NULL
  f(make("deepseek/deepseek-chat"), model = "openai/gpt-4o-mini", name = "run3")
  expect_false("max_retries" %in% names(seen))
})
