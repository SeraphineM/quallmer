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
# expected round trip; `error`, `status` and `finish` default to NA. The
# model-name lookup a wholly rejected run makes is stubbed out too, since it
# would otherwise ask the provider; `hint` is what it answers.
json_test_handler <- function(attempts, calls = NULL, hint = character()) {
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) structure(list(), class = "fake_chat"))
  mockery::stub(h, "model_name_hint", function(...) hint)
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
  # An unexplained failure may have been billed; a request never sent was not
  expect_true(all(is.na(result$usage[1, ])))
  expect_equal(unname(result$usage[2, ]), c(0, 0, 0, 0))
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
  # Classed, so a backfill can tell a cut-off from a validation error that
  # merely mentions max_tokens
  expect_s3_class(result$.error[[1]], "quallmer_truncation_error")
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
      json_retries = 2
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

test_that("code_handler_json gives up after json_retries and records the reason", {
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

test_that("code_handler_json honours json_retries", {
  invalid <- list(text = "{\"score\":1,\"lab\":\"maybe\"}")
  calls <- new.env()
  h <- json_test_handler(list(invalid, invalid, invalid, invalid, invalid), calls = calls)

  suppressWarnings(h(
    x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
    chat_args = list(), execution_args = list(), json_retries = 4
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
    code_handler_json("a", codebook, "deepseek", list(), list(), json_retries = -1),
    "single non-negative integer"
  )
  expect_error(
    code_handler_json("a", codebook, "deepseek", list(), list(), json_retries = 1.5),
    "single non-negative integer"
  )
  expect_error(
    code_handler_json("a", codebook, "deepseek", list(), list(), json_retries = Inf),
    "single non-negative integer"
  )
  expect_error(
    code_handler_json("a", codebook, "deepseek", list(), list(), json_retries = .Machine$integer.max + 1),
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

test_that("code_handler_json names the model when the provider does not list it (#133)", {
  rejected <- list(
    text = c(NA_character_, NA_character_),
    error = rep("HTTP 400 Bad Request.", 2),
    status = c(400L, 400L)
  )
  hint <- c("i" = "\"gpt-4o-mimi\" is not a model that \"openai\" lists.",
            "i" = "Did you mean \"gpt-4o-mini\"?")
  h <- json_test_handler(list(rejected), hint = hint)

  err <- tryCatch(
    h(x = c("a", "b"), codebook = json_test_codebook(), model = "openai/gpt-4o-mimi",
      chat_args = list(), execution_args = list()),
    error = function(e) e
  )
  msg <- strip_ansi(conditionMessage(err))
  expect_match(msg, "Every request to model \"openai/gpt-4o-mimi\" was rejected")
  expect_match(msg, "Did you mean \"gpt-4o-mini\"")
  # The provider has answered the question, so the generic advice is dropped
  expect_no_match(msg, "Check the model name")

  # With nothing to add, the provider's error and the generic advice stand
  h <- json_test_handler(list(rejected))
  expect_error(
    h(x = c("a", "b"), codebook = json_test_codebook(), model = "openai/gpt-4o-mimi",
      chat_args = list(), execution_args = list()),
    "Check the model name"
  )

  # An answer qlm_code() already has is used rather than asked for again
  h <- json_test_handler(list(rejected), hint = hint)
  expect_error(
    h(x = c("a", "b"), codebook = json_test_codebook(), model = "openai/gpt-4o-mimi",
      chat_args = list(), execution_args = list(), model_hint = character()),
    "Check the model name"
  )
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


# Legacy failure detection ----------------------------------------------------

# Objects coded before every response was validated (#140) carry no .error
# for a value the endpoint silently left out; failed_units() still reads
# their required scalars.
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

test_that("is_json_word_error recognises the DashScope rejection", {
  expect_true(is_json_word_error(paste(
    "<400> InternalError.Algo.InvalidParameter: 'messages' must contain the word",
    "'json' in some form, to use 'response_format' of type 'json_object'"
  )))
  expect_false(is_json_word_error("HTTP 429 Too Many Requests."))
  expect_false(is_json_word_error(NA_character_))
  expect_false(is_json_word_error(character(0)))
})


# cost that cannot be priced (#135) --------------------------------------------

# The handler with only the round trip stubbed out: the chat is built for
# real, off the environment, so the diagnosis reads the provider the run
# would use.
json_offline_handler <- function(attempts) {
  h <- code_handler_json
  mockery::stub(h, "model_name_hint", function(...) character())
  i <- 0L
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    i <<- i + 1L
    attempt <- attempts[[i]]
    n <- length(attempt$text)
    list(
      text = attempt$text,
      error = rep(NA_character_, n),
      status = rep(NA_integer_, n),
      finish = rep(NA_character_, n),
      usage = json_test_usage(n)
    )
  })
  h
}
offline_args <- list(credentials = function() list(Authorization = "Bearer x"))

test_that("the JSON path diagnoses cost from its own chat, and stays quiet when told (#135)", {
  attempts <- list(list(text = "{\"score\":1,\"lab\":\"pos\"}"))

  h <- json_offline_handler(attempts)
  expect_message(
    result <- h(x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
                chat_args = offline_args, execution_args = list(include_cost = TRUE)),
    "no prices for DeepSeek models"
  )
  expect_equal(attr(result, "qlm_backend_meta")$unpriced$kind, "provider")
  expect_equal(attr(result, "qlm_backend_meta")$unpriced$provider, "DeepSeek")

  # As the fallback after a structured attempt that already said it
  h <- json_offline_handler(attempts)
  expect_no_message(
    result <- h(x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
                chat_args = offline_args, execution_args = list(include_cost = TRUE),
                cost_message = FALSE)
  )
  expect_equal(attr(result, "qlm_backend_meta")$unpriced$kind, "provider")

  # No cost asked for: nothing said, nothing kept
  h <- json_offline_handler(attempts)
  expect_no_message(
    result <- h(x = "a", codebook = json_test_codebook(), model = "deepseek/deepseek-chat",
                chat_args = offline_args, execution_args = list())
  )
  expect_null(attr(result, "qlm_backend_meta")$unpriced)
})

test_that("code_handler_json registers tools on the chat it builds (#122)", {
  skip_if_not_installed("mockery")
  registered <- list()
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) {
    structure(list(register_tool = function(tl) {
      registered[[length(registered) + 1L]] <<- tl
      invisible(NULL)
    }), class = "fake_chat")
  })
  mockery::stub(h, "model_name_hint", function(...) character())
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    list(text = '{"score": 1}', error = NA_character_, status = NA_integer_,
         finish = NA_character_, usage = json_test_usage(1))
  })
  codebook <- qlm_codebook("Test", "Prompt", ellmer::type_object(score = ellmer::type_number("Score")))
  web_search <- ellmer::openai_tool_web_search()

  h("a", codebook, "openai/gpt-4o-mini", list(), list(), tools = list(web_search))
  expect_length(registered, 1)
  expect_identical(registered[[1]], web_search)

  registered <- list()
  h("a", codebook, "openai/gpt-4o-mini", list(), list())
  expect_length(registered, 0)
})


# on_error (#171) --------------------------------------------------------------

test_that("code_handler_json forwards on_error as given, with no default of its own (#171)", {
  skip_if_not_installed("mockery")
  seen <- new.env()

  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) structure(list(), class = "fake_chat"))
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    seen$pc_args <- pc_args
    list(
      text = '{"score": 1}',
      error = NA_character_, status = NA_integer_, finish = NA_character_,
      usage = json_test_usage(1)
    )
  })
  codebook <- qlm_codebook(
    "Test", "Test prompt",
    ellmer::type_object(score = ellmer::type_integer("Score"))
  )

  # The default is qlm_code()'s to supply; the handler adds nothing
  h("a", codebook, "openai/gpt-4o-mini", chat_args = list(),
    execution_args = list(), cost_message = FALSE)
  expect_false("on_error" %in% names(seen$pc_args))

  # What it is given is what ellmer gets
  h("a", codebook, "openai/gpt-4o-mini", chat_args = list(),
    execution_args = list(on_error = "return", max_active = 3), cost_message = FALSE)
  expect_equal(seen$pc_args$on_error, "return")
  expect_equal(seen$pc_args$max_active, 3)
})
