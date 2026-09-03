# Helpers ---------------------------------------------------------------------

json_test_codebook <- function(...) {
  qlm_codebook(
    "Test",
    "Rate it.",
    ellmer::type_object(
      score = ellmer::type_number("Score"),
      lab = ellmer::type_enum(values = c("pos", "neg"))
    ),
    ...
  )
}

# Fake usage matrix with `n` identical rows, as json_chat_turns() returns.
json_test_usage <- function(n) {
  matrix(
    c(rep(10, n), rep(5, n), rep(0, n), rep(0.001, n)),
    nrow = n,
    dimnames = list(NULL, c("input_tokens", "output_tokens",
                            "cached_input_tokens", "cost"))
  )
}

# A code_handler_json() with ellmer::chat() and json_chat_turns() stubbed out.
# `attempts` is a list of list(text =, error =, status =, finish =), one per
# expected round trip; `error`, `status` and `finish` default to NA.
json_test_handler <- function(attempts, calls = NULL) {
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) structure(list(), class = "fake_chat"))
  i <- 0L
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    i <<- i + 1L
    if (!is.null(calls)) {
      calls$n <- i
      calls$prompts[[i]] <- prompts
    }
    attempt <- attempts[[i]]
    n <- length(attempt$text)
    list(
      text = attempt$text,
      error = attempt$error %||% rep(NA_character_, n),
      status = attempt$status %||% rep(NA_integer_, n),
      finish = attempt$finish %||% rep(NA_character_, n),
      usage = json_test_usage(n)
    )
  })
  h
}

json_test_messages <- function(x) {
  vapply(
    x$.error,
    function(e) if (is.null(e)) NA_character_ else conditionMessage(e),
    character(1)
  )
}


# json_schema_from_type() -----------------------------------------------------

test_that("json_schema_from_type renders basic, enum and object types", {
  schema <- json_schema_from_type(json_test_codebook()$schema)

  expect_equal(schema$type, "object")
  expect_equal(names(schema$properties), c("score", "lab"))
  expect_equal(schema$properties$score, list(type = "number", description = "Score"))
  expect_equal(schema$properties$lab, list(type = "string", enum = list("pos", "neg")))
  expect_equal(schema$required, list("score", "lab"))
  expect_false(schema$additionalProperties)
})

test_that("json_schema_from_type recurses into arrays and nested objects", {
  type <- ellmer::type_object(
    claims = ellmer::type_array(
      ellmer::type_object(
        text = ellmer::type_string("Claim text"),
        salience = ellmer::type_number("Salience")
      ),
      description = "Claims made"
    )
  )
  schema <- json_schema_from_type(type)

  expect_equal(schema$properties$claims$type, "array")
  expect_equal(schema$properties$claims$description, "Claims made")
  expect_equal(schema$properties$claims$items$type, "object")
  expect_equal(names(schema$properties$claims$items$properties), c("text", "salience"))
})

test_that("json_schema_from_type omits optional properties from required", {
  type <- ellmer::type_object(
    a = ellmer::type_string("A"),
    b = ellmer::type_string("B", required = FALSE)
  )
  expect_equal(json_schema_from_type(type)$required, list("a"))
})

test_that("json_schema_from_type rejects unsupported types", {
  expect_error(json_schema_from_type("not a type"), "Unsupported schema type")
})


# validate_against_type() -----------------------------------------------------

test_that("validate_against_type accepts a conforming object", {
  schema <- json_test_codebook()$schema
  value <- list(score = 1, lab = "pos")

  expect_equal(validate_against_type(value, schema, "$"), value)
})

test_that("validate_against_type reports missing, wrong-type and bad-enum values", {
  schema <- json_test_codebook()$schema

  expect_error(
    validate_against_type(list(lab = "pos"), schema, "$"),
    "\\$\\.score is required but missing",
  )
  expect_error(
    validate_against_type(list(score = "high", lab = "pos"), schema, "$"),
    "\\$\\.score must be a number"
  )
  expect_error(
    validate_against_type(list(score = 1, lab = "maybe"), schema, "$"),
    "\\$\\.lab must be one of: pos, neg"
  )
  expect_error(
    validate_against_type(list(score = 1, lab = "pos", extra = 2), schema, "$"),
    "\\$ has unexpected property: extra"
  )
  expect_error(
    validate_against_type(list(score = NULL, lab = "pos"), schema, "$"),
    "\\$\\.score is required and cannot be null"
  )
})

test_that("validate_against_type reports the JSON path of a nested failure", {
  type <- ellmer::type_object(
    claims = ellmer::type_array(
      ellmer::type_object(salience = ellmer::type_number("Salience"))
    )
  )
  value <- list(claims = list(
    list(salience = 1),
    list(salience = 2),
    list(salience = "high")
  ))

  expect_error(
    validate_against_type(value, type, "$"),
    "\\$\\.claims\\[3\\]\\.salience must be a number"
  )
})

test_that("validate_against_type distinguishes arrays from objects", {
  type <- ellmer::type_object(xs = ellmer::type_array(ellmer::type_string()))

  expect_error(
    validate_against_type(list(xs = list(a = "one")), type, "$"),
    "\\$\\.xs must be a JSON array"
  )
  expect_equal(
    validate_against_type(list(xs = list("one", "two")), type, "$"),
    list(xs = list("one", "two"))
  )
  # An empty array is legal
  expect_equal(validate_against_type(list(xs = list()), type, "$"), list(xs = list()))
})

test_that("validate_against_type accepts an empty object when nothing is required", {
  type <- ellmer::type_object(a = ellmer::type_string("A", required = FALSE))
  # jsonlite parses "{}" to a *named* empty list, not an unnamed one
  value <- jsonlite::fromJSON("{}", simplifyVector = FALSE)

  expect_named(value)
  expect_equal(validate_against_type(value, type, "$"), list(a = NULL))
})

test_that("validate_against_type keeps extras when additional_properties is TRUE", {
  withr::local_options(lifecycle_verbosity = "quiet")
  type <- ellmer::type_object(
    a = ellmer::type_string("A"),
    .additional_properties = TRUE
  )

  expect_equal(
    validate_against_type(list(a = "x", b = 2), type, "$"),
    list(a = "x", b = 2)
  )
})

test_that("validate_against_type enforces integer bounds", {
  type <- ellmer::type_object(n = ellmer::type_integer("N"))

  expect_equal(validate_against_type(list(n = 3), type, "$"), list(n = 3))
  expect_error(validate_against_type(list(n = 3.5), type, "$"), "must be a integer")
})


# parse_and_validate_json() ---------------------------------------------------

test_that("parse_and_validate_json handles empty, unparsable and non-object text", {
  schema <- json_test_codebook()$schema

  expect_equal(
    parse_and_validate_json(NA_character_, schema)$error,
    "The API returned an empty response."
  )
  expect_equal(
    parse_and_validate_json("   ", schema)$error,
    "The API returned an empty response."
  )
  expect_match(parse_and_validate_json("{not json", schema)$error, "^Invalid JSON: ")
  expect_equal(
    parse_and_validate_json("[1, 2]", schema)$error,
    "JSON output must be an object."
  )
})

test_that("parse_and_validate_json strips code fences before parsing", {
  schema <- json_test_codebook()$schema
  fenced <- "```json\n{\"score\": 1, \"lab\": \"pos\"}\n```"

  result <- parse_and_validate_json(fenced, schema)
  expect_true(result$ok)
  expect_equal(result$value, list(score = 1, lab = "pos"))
})


# Small helpers ---------------------------------------------------------------

test_that("strip_code_fence removes fences and leaves plain JSON alone", {
  expect_equal(strip_code_fence("```json\n{\"a\":1}\n```"), "{\"a\":1}")
  expect_equal(strip_code_fence("```\n{\"a\":1}\n```"), "{\"a\":1}")
  expect_equal(strip_code_fence("  {\"a\":1}  "), "{\"a\":1}")
})

test_that("is_length_rejection recognises over-long input only", {
  expect_true(is_length_rejection("This model's maximum context length is 128000 tokens"))
  expect_true(is_length_rejection("Range of input length should be [1, 129024]"))
  expect_true(is_length_rejection("the prompt is too long"))

  # Generic latency language is transient, not evidence of a context overflow.
  expect_false(is_length_rejection("upstream server took too long to respond"))
  expect_false(is_length_rejection("request waited too long in queue"))

  # Content refusals must NOT be treated as unrecoverable: they are
  # non-deterministic, and the same document is coded on a later attempt.
  expect_false(is_length_rejection(
    "<400> InternalError.Algo.DataInspectionFailed: Input text data may contain inappropriate content."
  ))
  expect_false(is_length_rejection("the request was rejected because it was considered high risk"))
  expect_false(is_length_rejection("blocked by the content policy"))

  expect_false(is_length_rejection("429 rate limit exceeded"))
  expect_false(is_length_rejection(NA_character_))
  expect_false(is_length_rejection(character(0)))
})

test_that("strip_ansi removes escape sequences", {
  expect_equal(strip_ansi("\033[31mfailed\033[39m"), "failed")
  expect_equal(strip_ansi(character(0)), character(0))
})


# json_chat_turns() -----------------------------------------------------------

test_that("json_chat_turns extracts text, tokens and cost from turns", {
  turn <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText("{\"score\":1}")),
    tokens = c(10L, 5L, 2L),
    cost = 0.004
  )
  fake_chat <- list(last_turn = function() turn)

  f <- json_chat_turns
  mockery::stub(f, "ellmer::parallel_chat", list(fake_chat))
  result <- f(structure(list(), class = "fake_chat"), list("a"), list())

  expect_equal(result$text, "{\"score\":1}")
  expect_true(is.na(result$error))
  expect_true(is.na(result$status))
  expect_true(is.na(result$finish))
  expect_equal(unname(result$usage[1, ]), c(10, 5, 2, 0.004))
})

test_that("json_chat_turns captures the reason a request failed", {
  failed <- simpleError("\033[31mcontext length exceeded\033[39m")

  f <- json_chat_turns
  mockery::stub(f, "ellmer::parallel_chat", list(failed, NULL))
  result <- f(structure(list(), class = "fake_chat"), list("a", "b"), list())

  expect_equal(result$error[[1]], "context length exceeded")
  expect_equal(result$error[[2]], "the request failed")
  expect_true(all(is.na(result$text)))
  expect_true(all(is.na(result$finish)))
  expect_equal(sum(result$usage), 0)
})

test_that("json_chat_turns keeps the finish reason of each turn", {
  cut <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText("{\"score\":")),
    tokens = c(10L, 60L, 0L), cost = 0.01, finish_reason = "max_tokens"
  )
  done <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText("{\"score\":1}")),
    tokens = c(10L, 5L, 0L), cost = 0.01, finish_reason = "success"
  )

  f <- json_chat_turns
  mockery::stub(f, "ellmer::parallel_chat", list(
    list(last_turn = function() cut),
    list(last_turn = function() done),
    simpleError("boom")
  ))
  result <- f(structure(list(), class = "fake_chat"), list("a", "b", "c"), list())

  expect_equal(result$finish, c("max_tokens", "success", NA))
})

test_that("turn_finish_reason reads what is there and shrugs at what is not", {
  turn <- function(...) {
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentText("x")), tokens = c(1L, 1L, 0L), cost = 0, ...
    )
  }
  expect_true(is.na(turn_finish_reason(turn())))
  expect_identical(turn_finish_reason(turn(finish_reason = "max_tokens")), "max_tokens")
  # ellmer wraps a reason it did not recognise in I(); the marker is dropped
  expect_identical(turn_finish_reason(turn(finish_reason = I("odd"))), "odd")
  # A turn class without the property, as older ellmer versions produce
  expect_true(is.na(turn_finish_reason(structure(list(), class = "not_a_turn"))))
})


# Incomplete responses --------------------------------------------------------

test_that("incomplete_response_reason names the provider's reason, and only when there is one", {
  expect_null(incomplete_response_reason(NA_character_))
  expect_null(incomplete_response_reason(character()))
  expect_null(incomplete_response_reason("success"))
  expect_null(incomplete_response_reason("tool_use"))
  expect_null(incomplete_response_reason("stop_sequence"))

  expect_match(
    incomplete_response_reason("max_tokens", 4096),
    "cut off at the max_tokens limit after 4,096 output tokens; raise"
  )
  # No token count to report: the sentence still reads
  expect_match(
    incomplete_response_reason("max_tokens"),
    "cut off at the max_tokens limit; raise"
  )
  expect_match(incomplete_response_reason("context_window"), "context window")
  expect_match(incomplete_response_reason("content_filter"), "content filter")
  # ... and that wording is what is_content_refusal() recognises
  expect_true(is_content_refusal(incomplete_response_reason("content_filter")))
  expect_match(incomplete_response_reason("odd"), "finish reason \"odd\"")
})

test_that("is_truncation covers the limits a retry cannot lift", {
  expect_true(is_truncation("max_tokens"))
  expect_true(is_truncation("context_window"))
  expect_false(is_truncation("content_filter"))
  expect_false(is_truncation("success"))
  expect_false(is_truncation(NA_character_))
  expect_false(is_truncation(character()))
})

test_that("code_handler_json records a cut-off response and does not retry it", {
  calls <- new.env()
  h <- json_test_handler(list(
    list(
      text = c("{\"score\":1,\"lab\":", "{\"score\":1,\"lab\":\"pos\"}"),
      finish = c("max_tokens", "success")
    )
  ), calls)

  expect_warning(
    result <- h(
      x = c("a", "b"), codebook = json_test_codebook(),
      model = "deepseek/deepseek-chat", chat_args = list(), execution_args = list()
    ),
    "1 response could not be coded"
  )

  # One round trip: no repair attempt was spent on a response the same
  # limit would cut again
  expect_equal(calls$n, 1)
  msgs <- json_test_messages(result)
  expect_match(msgs[[1]], "cut off at the max_tokens limit after 5 output tokens")
  expect_match(msgs[[1]], "params\\(max_tokens = \\)")
  expect_true(is.na(msgs[[2]]))
  expect_equal(result$score, c(NA, 1))
  expect_equal(attr(result, "qlm_backend_meta")$n_invalid, 1)
})

test_that("code_handler_json rejects a response reported as cut off even when it parses", {
  # The provider's word wins over a clean parse, as in ellmer's own
  # check_finish_reason(): an object that closed just before the limit may
  # still be short of what the model would have written
  calls <- new.env()
  h <- json_test_handler(list(
    list(text = "{\"score\":1,\"lab\":\"pos\"}", finish = "max_tokens")
  ), calls)

  expect_warning(
    result <- h(
      x = "a", codebook = json_test_codebook(),
      model = "deepseek/deepseek-chat", chat_args = list(), execution_args = list()
    ),
    "could not be coded"
  )

  expect_equal(calls$n, 1)
  expect_true(is.na(result$score))
  expect_match(json_test_messages(result)[[1]], "cut off at the max_tokens limit")
})

test_that("code_handler_json retries a content-filtered response and names the filter", {
  calls <- new.env()
  h <- json_test_handler(list(
    list(text = "", finish = "content_filter"),
    list(text = "{\"score\":1,\"lab\":\"pos\"}", finish = "success")
  ), calls)

  result <- h(
    x = "a", codebook = json_test_codebook(),
    model = "deepseek/deepseek-chat", chat_args = list(), execution_args = list()
  )
  expect_equal(calls$n, 2)
  expect_equal(result$score, 1)

  # One that never clears records the provider's reason, not "empty response"
  h2 <- json_test_handler(rep(list(list(text = "", finish = "content_filter")), 3))
  expect_warning(
    result2 <- h2(
      x = "a", codebook = json_test_codebook(),
      model = "deepseek/deepseek-chat", chat_args = list(), execution_args = list(),
      max_retries = 2
    ),
    "content filter"
  )
  expect_match(json_test_messages(result2)[[1]], "withheld by the provider's content filter")
})


# code_handler_json() ---------------------------------------------------------

test_that("code_handler_json codes conforming responses", {
  h <- json_test_handler(list(
    list(text = c("{\"score\":1,\"lab\":\"pos\"}", "{\"score\":-1,\"lab\":\"neg\"}"))
  ))

  result <- h(
    x = c("a", "b"), codebook = json_test_codebook(),
    model = "deepseek/deepseek-chat", chat_args = list(), execution_args = list()
  )

  expect_equal(result$score, c(1, -1))
  expect_equal(as.character(result$lab), c("pos", "neg"))
  expect_false(".error" %in% names(result))
  expect_equal(attr(result, "qlm_backend_meta")$backend, "json_mode")
  expect_equal(attr(result, "qlm_backend_meta")$n_invalid, 0)
})

test_that("code_handler_json repairs an invalid response and sums usage across attempts", {
  calls <- new.env()
  h <- json_test_handler(
    list(
      list(text = c("{\"score\":1,\"lab\":\"pos\"}", "{\"score\":2,\"lab\":\"maybe\"}")),
      list(text = "{\"score\":2,\"lab\":\"neg\"}")
    ),
    calls = calls
  )

  result <- h(
    x = c("a", "b"), codebook = json_test_codebook(),
    model = "deepseek/deepseek-chat", chat_args = list(),
    execution_args = list(include_tokens = TRUE, include_cost = TRUE)
  )

  expect_equal(calls$n, 2)
  # Only the failing unit is re-sent, with the validation error in the prompt
  expect_length(calls$prompts[[2]], 1)
  expect_match(calls$prompts[[2]][[1]], "\\$\\.lab must be one of: pos, neg")

  expect_equal(as.character(result$lab), c("pos", "neg"))
  expect_false(".error" %in% names(result))
  # A repair attempt is a real billed request, so unit 2 is charged twice
  expect_equal(result$input_tokens, c(10, 20))
  expect_equal(result$output_tokens, c(5, 10))
  expect_equal(result$cost, c(0.001, 0.002))
})

test_that("code_handler_json gives up after max_retries and records the reason", {
  invalid <- list(text = "{\"score\":1,\"lab\":\"maybe\"}")
  h <- json_test_handler(list(invalid, invalid, invalid))

  expect_warning(
    result <- h(
      x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
      chat_args = list(), execution_args = list()
    ),
    "1 response could not be coded"
  )

  expect_true(is.na(result$score))
  expect_true(is.na(result$lab))
  expect_match(json_test_messages(result), "\\$\\.lab must be one of")
  expect_equal(attr(result, "qlm_backend_meta")$n_invalid, 1)
})

test_that("code_handler_json retries a content refusal, which is not deterministic", {
  refusal <- list(
    text = NA_character_,
    error = "InternalError.Algo.DataInspectionFailed: inappropriate content",
    status = 400L
  )
  calls <- new.env()
  # Refused twice, then coded: the behaviour observed at two providers, where
  # the same document is refused on one pass and accepted on the next.
  h <- json_test_handler(
    list(refusal, refusal, list(text = "{\"score\":1,\"lab\":\"pos\"}")),
    calls = calls
  )

  result <- h(
    x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
    chat_args = list(), execution_args = list()
  )

  expect_equal(calls$n, 3)
  expect_equal(result$score, 1)
  expect_false(".error" %in% names(result))
  expect_equal(attr(result, "qlm_backend_meta")$n_invalid, 0)
})

test_that("code_handler_json records a refusal that never clears", {
  refusal <- list(
    text = NA_character_,
    error = "InternalError.Algo.DataInspectionFailed: inappropriate content",
    status = 400L
  )
  h <- json_test_handler(list(refusal, refusal, refusal))

  expect_warning(
    result <- h(
      x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
      chat_args = list(), execution_args = list()
    ),
    "1 response could not be coded"
  )

  expect_match(json_test_messages(result), "DataInspectionFailed")
  # Counted, rather than looking identical to a success
  expect_equal(attr(result, "qlm_backend_meta")$n_invalid, 1)
})

test_that("code_handler_json attributes each error to the right unit", {
  refusal <- "blocked by the content policy"
  h <- json_test_handler(list(
    # a valid, b refused, c invalid, d valid
    list(
      text = c("{\"score\":1,\"lab\":\"pos\"}", NA_character_,
               "{\"score\":3,\"lab\":\"maybe\"}", "{\"score\":4,\"lab\":\"neg\"}"),
      error = c(NA_character_, refusal, NA_character_, NA_character_),
      status = c(NA_integer_, 400L, NA_integer_, NA_integer_)
    ),
    # b and c are both retried -- a refusal is not treated as unrecoverable
    list(text = c(NA_character_, "{\"score\":3,\"lab\":\"nope\"}"),
         error = c(refusal, NA_character_), status = c(400L, NA_integer_)),
    list(text = c(NA_character_, "{\"score\":3,\"lab\":\"nope\"}"),
         error = c(refusal, NA_character_), status = c(400L, NA_integer_))
  ))

  expect_warning(
    result <- h(
      x = c("a", "b", "c", "d"), codebook = json_test_codebook(),
      model = "deepseek/deepseek-chat", chat_args = list(), execution_args = list()
    ),
    "2 responses could not be coded"
  )

  expect_equal(result$score, c(1, NA, NA, 4))
  messages <- json_test_messages(result)
  expect_true(is.na(messages[[1]]))
  expect_match(messages[[2]], "content policy")
  expect_match(messages[[3]], "\\$\\.lab must be one of")
  expect_true(is.na(messages[[4]]))
})

test_that("code_handler_json honours max_retries", {
  invalid <- list(text = "{\"score\":1,\"lab\":\"maybe\"}")
  calls <- new.env()
  h <- json_test_handler(list(invalid, invalid, invalid, invalid, invalid), calls = calls)

  suppressWarnings(h(
    x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
    chat_args = list(), execution_args = list(), max_retries = 4
  ))

  expect_equal(calls$n, 5)
})

test_that("code_handler_json forces JSON mode while keeping user api_args", {
  captured <- NULL
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) {
    captured <<- list(...)
    structure(list(), class = "fake_chat")
  })
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    list(text = "{\"score\":1,\"lab\":\"pos\"}", error = NA_character_,
         usage = json_test_usage(1))
  })

  h(
    x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
    chat_args = list(api_args = list(seed = 1L)), execution_args = list()
  )

  expect_equal(captured$name, "deepseek/deepseek-chat")
  expect_equal(captured$api_args$seed, 1L)
  expect_equal(captured$api_args$response_format, list(type = "json_object"))
  expect_match(captured$system_prompt, "Return exactly one valid JSON object")
})

test_that("code_handler_json rejects unsupported requests", {
  codebook <- json_test_codebook()

  expect_error(
    code_handler_json("a", codebook, "deepseek", list(), list(), batch = TRUE),
    "must be .*FALSE.* for model"
  )
  expect_error(
    code_handler_json(
      "a", json_test_codebook(input_type = "image"), "deepseek", list(), list()
    ),
    "supports text codebooks only"
  )
  expect_error(
    code_handler_json(
      "a",
      qlm_codebook("T", "P", ellmer::type_array(ellmer::type_string())),
      "deepseek", list(), list()
    ),
    "must have a schema created by"
  )
  expect_error(
    code_handler_json("a", codebook, "deepseek", list(), list(), max_retries = -1),
    "single non-negative integer"
  )
  expect_error(
    code_handler_json("a", codebook, "deepseek", list(), list(), max_retries = 1.5),
    "single non-negative integer"
  )
  expect_error(
    code_handler_json("a", codebook, "deepseek", list(api_args = "seed"), list()),
    "must be a named list"
  )
})

test_that("code_handler_json produces the same shape as the structured path", {
  parsed <- list(list(score = 1, lab = "pos"), list(score = -1, lab = "neg"))
  codebook <- json_test_codebook()

  h <- json_test_handler(list(
    list(text = c("{\"score\":1,\"lab\":\"pos\"}", "{\"score\":-1,\"lab\":\"neg\"}"))
  ))
  ours <- h(
    x = c("a", "b"), codebook = codebook, model = "deepseek/deepseek-chat",
    chat_args = list(), execution_args = list()
  )
  attr(ours, "qlm_backend_meta") <- NULL

  theirs <- ellmer:::convert_from_type(parsed, ellmer::type_array(codebook$schema))

  expect_equal(names(ours), names(theirs))
  expect_equal(ours, theirs)
})


# Reporting API failures ------------------------------------------------------

test_that("api_error_detail reads the provider message out of the response body", {
  # DeepSeek serves this as application/octet-stream, so httr2 will not parse it
  # and the condition message is only "HTTP 400 Bad Request."
  body <- paste0(
    '{"error":{"message":"The supported API model names are deepseek-v4-pro, ',
    'deepseek-v4-flash, and deepseek-v4-flash-vision-exp, but you passed ',
    'deepseek-v3-flash.","type":"invalid_request_error"}}'
  )
  cnd <- structure(
    class = c("httr2_http_400", "httr2_error", "error", "condition"),
    list(
      message = "HTTP 400 Bad Request.",
      status = 400L,
      resp = list(body = charToRaw(body))
    )
  )

  expect_match(api_error_detail(cnd), "but you passed deepseek-v3-flash")
  expect_equal(api_error_status(cnd), 400L)
  expect_match(api_error_message(cnd), "^HTTP 400 Bad Request\\. The supported API")
})

test_that("api_error_detail copes with bodies it cannot use", {
  make_cnd <- function(body) {
    structure(
      class = c("httr2_error", "error", "condition"),
      list(message = "HTTP 500 Internal Server Error.", status = 500L,
           resp = list(body = body))
    )
  }

  expect_true(is.na(api_error_detail(make_cnd(raw(0)))))
  expect_true(is.na(api_error_detail(make_cnd(charToRaw("<html>nope</html>")))))
  expect_true(is.na(api_error_detail(make_cnd(charToRaw('{"ok":true}')))))
  expect_equal(
    api_error_detail(make_cnd(charToRaw('{"error":"bad request"}'))),
    "bad request"
  )
  expect_equal(
    api_error_detail(make_cnd(charToRaw('{"message":"top-level failure"}'))),
    "top-level failure"
  )
  expect_equal(
    api_error_detail(make_cnd(charToRaw(
      '{"error":{"type":"invalid_request"},"message":"fallback failure"}'
    ))),
    "fallback failure"
  )
  expect_true(is.na(api_error_detail(make_cnd(charToRaw('"not an object"')))))
  expect_true(is.na(api_error_detail(simpleError("boom"))))
  expect_equal(api_error_message(simpleError("boom")), "boom")
  expect_equal(api_error_message(NULL), "the request failed")
  expect_true(is.na(api_error_status(simpleError("boom"))))
})

test_that("is_fatal_status separates configuration errors from transient ones", {
  expect_true(is_fatal_status(400L))
  expect_true(is_fatal_status(401L))
  expect_true(is_fatal_status(404L))

  expect_false(is_fatal_status(429L))   # rate limited: worth retrying
  expect_false(is_fatal_status(503L))   # server error: worth retrying
  expect_false(is_fatal_status(NA_integer_))
})

test_that("set_bullets truncates and does not treat messages as cli templates", {
  expect_equal(unname(set_bullets("plain")), "plain")
  expect_named(set_bullets("plain"), "x")
  expect_equal(unname(set_bullets("a {braced} message")), "a {{braced}} message")
  expect_length(set_bullets(as.character(1:10)), 4)
  expect_match(set_bullets(as.character(1:10))[[4]], "7 other reason")
  expect_length(set_bullets(character()), 0)
})

test_that("code_handler_json aborts when the provider rejects every request", {
  calls <- new.env()
  h <- json_test_handler(
    list(list(
      text = c(NA_character_, NA_character_),
      error = rep(paste0(
        "HTTP 400 Bad Request. The supported API model names are ",
        "deepseek-v4-pro, deepseek-v4-flash, and deepseek-v4-flash-vision-exp, ",
        "but you passed deepseek-v3-flash."
      ), 2),
      status = c(400L, 400L)
    )),
    calls = calls
  )

  expect_error(
    h(
      x = c("a", "b"), codebook = json_test_codebook(),
      model = "deepseek/deepseek-v3-flash", chat_args = list(),
      execution_args = list()
    ),
    "but you passed deepseek-v3-flash"
  )

  # No point re-sending a request the provider rejected outright
  expect_equal(calls$n, 1)
})

test_that("code_handler_json does not retry a fatal failure in a mixed batch", {
  calls <- new.env()
  h <- json_test_handler(
    list(list(
      text = c(NA_character_, "{\"score\":1,\"lab\":\"pos\"}"),
      error = c("HTTP 400 Bad Request. Invalid request body.", NA_character_),
      status = c(400L, NA_integer_)
    )),
    calls = calls
  )

  expect_warning(
    result <- h(
      x = c("a", "b"), codebook = json_test_codebook(),
      model = "deepseek/deepseek-v4-pro", chat_args = list(),
      execution_args = list()
    ),
    "1 response could not be coded, out of 2"
  )

  expect_equal(calls$n, 1)
  expect_true(is.na(result$score[[1]]))
  expect_equal(result$score[[2]], 1)
  expect_match(json_test_messages(result)[[1]], "Invalid request body")
})

test_that("code_handler_json retries transient failures but not fatal ones", {
  calls <- new.env()
  h <- json_test_handler(
    list(
      list(
        text = c(NA_character_, NA_character_, "{\"score\":3,\"lab\":\"pos\"}"),
        error = c("HTTP 400 Bad Request. Context length exceeded.",
                  "HTTP 429 Too Many Requests.", NA_character_),
        status = c(400L, 429L, NA_integer_)
      ),
      # only the rate-limited unit comes back
      list(text = "{\"score\":2,\"lab\":\"neg\"}")
    ),
    calls = calls
  )

  expect_warning(
    result <- h(
      x = c("a", "b", "c"), codebook = json_test_codebook(),
      model = "deepseek/deepseek-v4-pro", chat_args = list(), execution_args = list()
    ),
    "1 response could not be coded, out of 3"
  )

  expect_equal(calls$n, 2)
  expect_length(calls$prompts[[2]], 1)
  expect_equal(result$score, c(NA, 2, 3))
  expect_match(json_test_messages(result)[[1]], "Context length exceeded")
})

test_that("code_handler_json names the reasons in its warning", {
  h <- json_test_handler(list(
    list(
      text = c("{\"score\":1,\"lab\":\"nope\"}", NA_character_),
      error = c(NA_character_, "HTTP 400 Bad Request. Context length exceeded."),
      status = c(NA_integer_, 400L)
    ),
    list(text = "{\"score\":1,\"lab\":\"nope\"}"),
    list(text = "{\"score\":1,\"lab\":\"nope\"}")
  ))

  expect_warning(
    h(
      x = c("a", "b"), codebook = json_test_codebook(),
      model = "deepseek/deepseek-v4-pro", chat_args = list(), execution_args = list()
    ),
    "Context length exceeded"
  )
})


# Structured-failure detection ------------------------------------------------

test_that("required_scalar_fields covers only checkable properties", {
  schema <- ellmer::type_object(
    code     = ellmer::type_enum("A code", values = c("a", "b")),
    n        = ellmer::type_integer("N"),
    optional = ellmer::type_string("Optional", required = FALSE),
    claims   = ellmer::type_array(ellmer::type_string("A claim"))
  )
  # Arrays become list-columns, where "nothing useful came back" has no simple
  # NA representation, so they are not checkable this way
  expect_setequal(required_scalar_fields(schema), c("code", "n"))
  expect_equal(required_scalar_fields("not a schema"), character())
})

test_that("all_required_missing detects a call that returned nothing usable", {
  schema <- json_test_codebook()$schema

  expect_true(all_required_missing(
    data.frame(score = c(NA_real_, NA_real_), lab = c(NA_character_, NA_character_)),
    schema
  ))
  # One good row is enough to say the endpoint did something
  expect_false(all_required_missing(
    data.frame(score = c(1, NA_real_), lab = c("pos", NA_character_)),
    schema
  ))
  expect_false(all_required_missing(
    data.frame(score = c(1, 2), lab = c("pos", "neg")),
    schema
  ))
})

test_that("all_required_missing does not misread a non-table result", {
  schema <- json_test_codebook()$schema
  # convert = FALSE returns a list; there is no table to inspect and so no
  # basis for calling the attempt a failure
  expect_false(all_required_missing(list(list(score = 1, lab = "pos")), schema))
  expect_false(all_required_missing(data.frame(), schema))
  expect_false(all_required_missing(NULL, schema))
})

test_that("n_incomplete counts partially failed rows", {
  schema <- json_test_codebook()$schema
  results <- data.frame(score = c(1, NA_real_, 3), lab = c("pos", "neg", NA_character_))

  expect_equal(n_incomplete(results, schema), 2)
  expect_equal(n_incomplete(data.frame(score = 1, lab = "pos"), schema), 0)
  expect_equal(n_incomplete(list(), schema), 0L)
})

test_that("rows with a recorded error are not evidence about schema enforcement", {
  schema <- json_test_codebook()$schema

  # Every row failed for a recorded reason: nothing to judge enforcement from,
  # and nothing to count as incomplete that is not already counted as failed
  failed <- data.frame(score = c(NA_real_, NA_real_), lab = c(NA_character_, NA_character_))
  failed$.error <- list(simpleError("HTTP 401"), simpleError("cut off"))
  expect_false(all_required_missing(failed, schema))
  expect_equal(n_incomplete(failed, schema), 0L)

  # The one intact row is all NA: that is the non-enforcement signal
  mixed <- data.frame(score = c(NA_real_, NA_real_), lab = c(NA_character_, NA_character_))
  mixed$.error <- list(simpleError("HTTP 401"), NULL)
  expect_true(all_required_missing(mixed, schema))
  expect_equal(n_incomplete(mixed, schema), 1L)

  # A response ellmer could extract nothing from is the signal itself: the
  # endpoint answered, in prose, so these rows are judged and "auto" falls back
  prose <- data.frame(score = c(NA_real_, NA_real_), lab = c(NA_character_, NA_character_))
  prose$.error <- list(
    extraction_error("Data extraction failed: no JSON responses found."),
    extraction_error("Data extraction failed: no JSON responses found.")
  )
  expect_true(all_required_missing(prose, schema))
  expect_equal(n_incomplete(prose, schema), 0L)
})


# Provider capability ---------------------------------------------------------

test_that("provider_enforces_schema follows ellmer's mechanism, not a vendor list", {
  withr::local_envvar(OPENAI_API_KEY = "x", ANTHROPIC_API_KEY = "x",
                      GROQ_API_KEY = "x", MISTRAL_API_KEY = "x")
  # ellmer::chat() derives the model from the name string, so it must not also
  # be passed through `...`
  provider_of <- function(name, ...) {
    suppressWarnings(ellmer::chat(name, echo = "none", ...)$get_provider())
  }

  # Own chat_body() using a mechanism the provider enforces
  expect_true(provider_enforces_schema(provider_of("openai/gpt-4o-mini")))
  expect_true(provider_enforces_schema(provider_of("anthropic/claude-sonnet-4-5")))

  # Falls through to ProviderOpenAICompatible, which sends strict = TRUE and
  # takes the answer on trust
  expect_false(provider_enforces_schema(provider_of("groq/llama-3.1-8b-instant")))
  expect_false(provider_enforces_schema(provider_of("mistral/mistral-large-latest")))
  expect_false(provider_enforces_schema(
    provider_of("openai_compatible/some-model", base_url = "https://example.com/v1",
                credentials = function() "k")
  ))
})

test_that("is_json_word_error recognises the DashScope rejection", {
  expect_true(is_json_word_error(paste(
    "<400> InternalError.Algo.InvalidParameter: 'messages' must contain the word",
    "'json' in some form, to use 'response_format' of type 'json_object'"
  )))
  expect_false(is_json_word_error("HTTP 429 Too Many Requests."))
  expect_false(is_json_word_error(NA_character_))
  expect_false(is_json_word_error(character(0)))
})
