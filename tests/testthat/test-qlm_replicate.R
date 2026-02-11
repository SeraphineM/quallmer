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
