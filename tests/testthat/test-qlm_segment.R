test_that("qlm_segment validates codebook argument", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  expect_error(
    qlm_segment("some text", codebook = list(name = "fake"), model = "test"),
    "must be a.*qlm_codebook"
  )

  expect_error(
    qlm_segment("some text", codebook = "not valid", model = "test"),
    "must be a.*qlm_codebook"
  )
})


test_that("qlm_segment rejects image-type codebooks", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Instructions",
    schema = ellmer::type_object(tag = ellmer::type_string("A tag")),
    input_type = "image"
  )

  expect_error(
    qlm_segment("some text", codebook = cb, model = "test"),
    "only supports text input"
  )
})


test_that("qlm_segment rejects schema with reserved 'text' field", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Instructions",
    schema = ellmer::type_object(
      text   = ellmer::type_string("The segment text"),
      aspect = ellmer::type_string("The aspect")
    )
  )

  expect_error(
    qlm_segment("some text", codebook = cb, model = "test"),
    "must not include a field named.*text"
  )
})


test_that("qlm_segment rejects non-character, non-corpus input", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Instructions",
    schema = ellmer::type_object(aspect = ellmer::type_string("Aspect"))
  )

  expect_error(
    qlm_segment(123L, codebook = cb, model = "test"),
    "must be a character vector or quanteda corpus"
  )
})


test_that("qlm_segment returns a quanteda corpus from character input", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "ABSA", "Segment by aspect.",
    schema = ellmer::type_object(
      aspect = ellmer::type_string("Aspect label")
    )
  )

  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- list(
    tibble::tibble(text = c("Clean room.", "Basic furnishings."),
                   aspect = c("cleanliness", "features")),
    tibble::tibble(text = "Great location.", aspect = "location")
  )

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  input  <- c(review1 = "Clean room. Basic furnishings.", review2 = "Great location.")
  result <- qlm_segment(input, cb, model = "test/model")

  expect_true(quanteda::is.corpus(result))
  expect_equal(quanteda::ndoc(result), 3L)
  expect_equal(quanteda::docnames(result), c("review1.1", "review1.2", "review2.1"))
})


test_that("qlm_segment sets docvars correctly from character input", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "ABSA", "Segment by aspect.",
    schema = ellmer::type_object(
      aspect = ellmer::type_string("Aspect label")
    )
  )

  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- list(
    tibble::tibble(text = c("Clean room.", "Basic furnishings."),
                   aspect = c("cleanliness", "features")),
    tibble::tibble(text = "Great location.", aspect = "location")
  )

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  result <- qlm_segment(
    c(review1 = "Clean room. Basic furnishings.", review2 = "Great location."),
    cb, model = "test/model"
  )

  dv <- quanteda::docvars(result)
  expect_equal(dv$docid,  c("review1", "review1", "review2"))
  expect_equal(dv$segid,  c(1L, 2L, 1L))
  expect_equal(dv$aspect, c("cleanliness", "features", "location"))
})


test_that("qlm_segment uses sequential text labels for unnamed character input", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Segment.",
    schema = ellmer::type_object(tag = ellmer::type_string("Tag"))
  )

  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- list(
    tibble::tibble(text = "Segment A.", tag = "a"),
    tibble::tibble(text = "Segment B.", tag = "b")
  )

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  result <- qlm_segment(c("Text one.", "Text two."), cb, model = "test/model")

  expect_equal(quanteda::docnames(result), c("text1.1", "text2.1"))
  expect_equal(quanteda::docvars(result)$docid, c("text1", "text2"))
})


test_that("qlm_segment returns a corpus from corpus input", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "ABSA", "Segment by aspect.",
    schema = ellmer::type_object(
      aspect = ellmer::type_string("Aspect label")
    )
  )

  corp <- quanteda::corpus(
    c(review1 = "Clean room. Basic furnishings.", review2 = "Great location.")
  )

  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- list(
    tibble::tibble(text = c("Clean room.", "Basic furnishings."),
                   aspect = c("cleanliness", "features")),
    tibble::tibble(text = "Great location.", aspect = "location")
  )

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  result <- qlm_segment(corp, cb, model = "test/model")

  expect_true(quanteda::is.corpus(result))
  expect_equal(quanteda::ndoc(result), 3L)
  expect_equal(quanteda::docnames(result), c("review1.1", "review1.2", "review2.1"))
})


test_that("qlm_segment corpus output inherits parent docvars", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Segment.",
    schema = ellmer::type_object(aspect = ellmer::type_string("Aspect"))
  )

  corp <- quanteda::corpus(
    c(doc1 = "Clean room. Great location."),
    docvars = data.frame(hotel = "Grand Hotel", stars = 4L)
  )

  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- list(
    tibble::tibble(text = c("Clean room.", "Great location."),
                   aspect = c("cleanliness", "location"))
  )

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  result <- qlm_segment(corp, cb, model = "test/model")
  dv     <- quanteda::docvars(result)

  expect_equal(quanteda::ndoc(result), 2L)
  expect_equal(dv$hotel,  c("Grand Hotel", "Grand Hotel"))
  expect_equal(dv$stars,  c(4L, 4L))
  expect_equal(dv$aspect, c("cleanliness", "location"))
})


test_that("qlm_segment warns for documents producing no segments", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Segment.",
    schema = ellmer::type_object(tag = ellmer::type_string("Tag"))
  )

  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- list(
    tibble::tibble(text = "Some text.", tag = "a"),
    tibble::tibble(text = character(0), tag = character(0))
  )

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  expect_warning(
    qlm_segment(c(doc1 = "Text one.", doc2 = "Text two."), cb, model = "test/model"),
    "produced no segments"
  )
})


test_that("qlm_segment passes provider-specific arguments to ellmer::chat", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("quanteda")

  cb <- qlm_codebook(
    "Test", "Segment.",
    schema = ellmer::type_object(tag = ellmer::type_string("Tag"))
  )

  # Track what arguments are passed to ellmer::chat
  chat_args_received <- NULL
  mock_chat <- function(...) {
    chat_args_received <<- list(...)
    structure(list(), class = "ellmer_chat")
  }
  mock_results <- list(tibble::tibble(text = "A.", tag = "a"))

  mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)

  # Call with a provider-specific argument (like base_url for openai_compatible)
  qlm_segment("Text.", cb, model = "test/model", base_url = "https://my-api.com/v1")

  # Verify the provider-specific argument was passed through to ellmer::chat
  expect_true("base_url" %in% names(chat_args_received))
  expect_equal(chat_args_received$base_url, "https://my-api.com/v1")
})
