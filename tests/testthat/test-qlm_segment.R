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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  input  <- c(review1 = "Clean room. Basic furnishings.", review2 = "Great location.")
  result <- qlm_segment(input, cb, model = "openai_compatible/test-model")

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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  result <- qlm_segment(
    c(review1 = "Clean room. Basic furnishings.", review2 = "Great location."),
    cb, model = "openai_compatible/test-model"
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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  result <- qlm_segment(c("Text one.", "Text two."), cb, model = "openai_compatible/test-model")

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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  result <- qlm_segment(corp, cb, model = "openai_compatible/test-model")

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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  result <- qlm_segment(corp, cb, model = "openai_compatible/test-model")
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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  expect_warning(
    qlm_segment(c(doc1 = "Text one.", doc2 = "Text two."), cb, model = "openai_compatible/test-model"),
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
  mockery::stub(qlm_segment, "ellmer::parallel_chat_structured",
                tibble::tibble(segments = mock_results))

  # Call with a provider-specific argument (like base_url for openai_compatible)
  qlm_segment("Text.", cb, model = "openai_compatible/test-model", base_url = "https://my-api.com/v1")

  # Verify the provider-specific argument was passed through to ellmer::chat
  expect_true("base_url" %in% names(chat_args_received))
  expect_equal(chat_args_received$base_url, "https://my-api.com/v1")
})


# usage and cost (#119) --------------------------------------------------------

# qlm_segment() with the chat built for real, off the environment and sending
# nothing, and the structured call stubbed to return `results`: one row per
# document, segments in a list-column, usage beside them as ellmer attaches
# it to a data frame. The stub carries ellmer's formals, since qlm_segment()
# reads them to route `...`. `seen` records the execution arguments received.
segment_run <- function(results, seen = NULL) {
  f <- qlm_segment
  stub <- function(chat, prompts, type, convert = TRUE, include_tokens = FALSE,
                   include_cost = FALSE, max_active = 10, rpm = 500,
                   on_error = c("return", "continue", "stop")) {
    if (!is.null(seen)) {
      seen$execution_args <- list(include_tokens = include_tokens,
                                  include_cost = include_cost)
    }
    results
  }
  mockery::stub(f, "ellmer::parallel_chat_structured", stub)
  f
}
offline <- function() list(Authorization = "Bearer x")
absa <- function() {
  qlm_codebook("ABSA", "Segment by aspect.",
               schema = ellmer::type_object(aspect = ellmer::type_string("Aspect label")))
}
# Three documents: two segments, none (answered, charged, nothing extracted),
# one
three_docs <- function() {
  tibble::tibble(
    segments = list(
      tibble::tibble(text = c("Clean room.", "Basic furnishings."),
                     aspect = c("cleanliness", "features")),
      tibble::tibble(text = character(), aspect = character()),
      tibble::tibble(text = "Great location.", aspect = "location")
    ),
    input_tokens = c(100, 120, 90),
    output_tokens = c(40, 5, 20),
    cached_input_tokens = c(0, 0, 0),
    cost = c(NA_real_, NA_real_, NA_real_)
  )
}
three_texts <- c(a = "Clean room. Basic furnishings.", b = "Nothing here.",
                 c = "Great location.")

test_that("qlm_segment keeps every document's usage, segments or not (#119)", {
  skip_if_not_installed("quanteda")
  f <- segment_run(three_docs())
  expect_warning(
    expect_message(
      result <- f(three_texts, absa(), model = "deepseek/deepseek-chat",
                  credentials = offline, include_tokens = TRUE, include_cost = TRUE),
      "no prices for DeepSeek models"
    ),
    "produced no segments"
  )

  # The canonical table has one row per input document, in input order
  usage <- quanteda::meta(result, "usage")
  expect_equal(usage$docid, c("a", "b", "c"))
  expect_equal(usage$input_tokens, c(100, 120, 90))
  expect_equal(usage$output_tokens, c(40, 5, 20))
  expect_true(all(is.na(usage$cost)))
  # Summing it gives the run's total, the zero-segment document included
  expect_equal(sum(usage$input_tokens), 310)

  # The docvars repeat each document's figures on its own segments, aligned
  dv <- quanteda::docvars(result)
  expect_equal(dv$docid, c("a", "a", "c"))
  expect_equal(dv$input_tokens, c(100, 100, 90))
  expect_equal(dv$output_tokens, c(40, 40, 20))
  expect_equal(dv$aspect, c("cleanliness", "features", "location"))
  expect_true(all(is.na(dv$cost)))

  expect_equal(quanteda::meta(result, "cost_note"), "NA (ellmer has no prices for DeepSeek models)")
  expect_null(quanteda::meta(result, "prices"))
})

test_that("qlm_segment records tokens or cost on their own, and nothing unasked (#119)", {
  skip_if_not_installed("quanteda")
  seen <- new.env()

  # Neither asked for: no usage anywhere
  f <- segment_run(three_docs()[, "segments"], seen)
  expect_no_message(
    suppressWarnings(result <- f(three_texts, absa(), model = "deepseek/deepseek-chat",
                                 credentials = offline))
  )
  expect_null(quanteda::meta(result, "usage"))
  expect_false(any(c("input_tokens", "cost") %in% names(quanteda::docvars(result))))
  expect_null(quanteda::meta(result, "cost_note"))

  # Tokens only
  f <- segment_run(three_docs()[, c("segments", "input_tokens", "output_tokens",
                                    "cached_input_tokens")], seen)
  suppressWarnings(result <- f(three_texts, absa(), model = "deepseek/deepseek-chat",
                               credentials = offline, include_tokens = TRUE))
  expect_equal(names(quanteda::meta(result, "usage")),
               c("docid", "input_tokens", "output_tokens", "cached_input_tokens"))
  expect_false("cost" %in% names(quanteda::docvars(result)))
  expect_null(quanteda::meta(result, "cost_note"))

  # Cost only: the diagnosis says the counts are not being recorded
  f <- segment_run(three_docs()[, c("segments", "cost")], seen)
  expect_message(
    suppressWarnings(result <- f(three_texts, absa(), model = "deepseek/deepseek-chat",
                                 credentials = offline, include_cost = TRUE)),
    "Supply the provider's published rates as `prices`"
  )
  expect_equal(names(quanteda::meta(result, "usage")), c("docid", "cost"))
  expect_false("input_tokens" %in% names(quanteda::docvars(result)))
})

test_that("qlm_segment costs an unpriced run from supplied rates, per document (#119)", {
  skip_if_not_installed("quanteda")
  seen <- new.env()
  f <- segment_run(three_docs(), seen)
  expect_no_message(
    suppressWarnings(result <- f(three_texts, absa(), model = "deepseek/deepseek-chat",
                                 credentials = offline,
                                 prices = c(input = 1, output = 10, cached_input = 0.1)))
  )
  # Supplying rates asked ellmer for tokens and cost
  expect_true(isTRUE(seen$execution_args$include_tokens))
  expect_true(isTRUE(seen$execution_args$include_cost))

  usage <- quanteda::meta(result, "usage")
  expect_equal(usage$cost, (c(100, 120, 90) * 1 + c(40, 5, 20) * 10) / 1e6)
  dv <- quanteda::docvars(result)
  expect_equal(dv$cost, usage$cost[c(1, 1, 3)])
  expect_equal(quanteda::meta(result, "prices"), c(input = 1, output = 10, cached_input = 0.1))
  expect_match(quanteda::meta(result, "cost_note"), "^from supplied rates")
})

test_that("qlm_segment reserves the usage names when usage is requested (#119)", {
  skip_if_not_installed("quanteda")
  cb <- qlm_codebook("ABSA", "Segment.",
                     schema = ellmer::type_object(cost = ellmer::type_number("Cost of the item")))
  f <- segment_run(three_docs())
  expect_error(
    f(three_texts, cb, model = "deepseek/deepseek-chat", credentials = offline,
      include_cost = TRUE),
    "reserved for the run's usage"
  )
  # Not requested: the field is the codebook's to use
  plain <- tibble::tibble(segments = list(tibble::tibble(text = "A.", cost = 3)))
  f <- segment_run(plain)
  result <- f(c(a = "A."), cb, model = "deepseek/deepseek-chat", credentials = offline)
  expect_equal(quanteda::docvars(result)$cost, 3)

  # An inherited docvar clashes the same way
  corp <- quanteda::corpus(c(a = "A."), docvars = data.frame(input_tokens = 1))
  f <- segment_run(three_docs()[1, ])
  expect_error(
    f(corp, absa(), model = "deepseek/deepseek-chat", credentials = offline,
      include_tokens = TRUE),
    "reserved for the run's usage"
  )
})

test_that("qlm_segment rejects convert = FALSE (#119)", {
  f <- segment_run(three_docs())
  expect_error(
    f(three_texts, absa(), model = "deepseek/deepseek-chat", credentials = offline,
      convert = FALSE),
    "convert = FALSE"
  )
})
