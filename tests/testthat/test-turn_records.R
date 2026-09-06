# One record per input, from either path's turns, and one order of
# precedence for what a response is. These are the pieces both coding paths
# share once the responses are in hand (#140).

score_schema <- function() {
  ellmer::type_object(score = ellmer::type_number("Score"))
}

# turn_records() ---------------------------------------------------------------

test_that("turn_records reads turns, chats, errors and absent results alike", {
  turn <- json_turn(list(score = 1), tokens = c(10, 5, 2), cost = 0.004)
  chat <- list(last_turn = function() turn)
  failed <- request_error("HTTP 429 Too Many Requests.", 429L)

  records <- turn_records(list(turn, chat, failed, NULL))

  expect_identical(records$turn[[1]], turn)
  expect_identical(records$turn[[2]], turn)
  expect_null(records$turn[[3]])
  expect_null(records$turn[[4]])
  expect_length(records$turn, 4L)

  expect_equal(records$error, c(NA, NA, "HTTP 429 Too Many Requests.", "the request failed"))
  expect_equal(records$status, c(NA, NA, 429L, NA))
  expect_equal(records$finish, c("success", "success", NA, NA))
  expect_equal(unname(records$usage[1, ]), c(10, 5, 2, 0.004))
  expect_equal(unname(records$usage[3, ]), c(0, 0, 0, 0))
  expect_equal(unname(records$usage[4, ]), c(0, 0, 0, 0))
})

test_that("turn_records keeps unknown usage as NA and a refused request at zero", {
  unknown <- ellmer::AssistantTurn(list(ellmer::ContentText("x")))
  records <- turn_records(list(unknown, request_error()))

  expect_true(all(is.na(records$usage[1, ])))
  expect_equal(unname(records$usage[2, ]), c(0, 0, 0, 0))
})

test_that("turn_records has nothing to say about an empty run", {
  records <- turn_records(list())
  expect_length(records$turn, 0L)
  expect_equal(nrow(records$usage), 0L)
})

# add_structured_values() ------------------------------------------------------

test_that("add_structured_values takes the parsed JSON as sent, and nothing from a failure", {
  turns <- list(
    json_turn(string = '{"score": "high", "extra": true}'),
    text_turn("no JSON here"),
    request_error(),
    NULL
  )
  records <- add_structured_values(turn_records(turns))

  # Unconverted: the string is still a string, the extra is still there
  expect_identical(records$value[[1]], list(score = "high", extra = TRUE))
  expect_true(is.na(records$problem[[1]]))
  expect_null(records$value[[2]])
  expect_equal(records$problem[[2]], "Data extraction failed: no JSON responses found.")
  expect_null(records$value[[3]])
  expect_true(is.na(records$problem[[3]]))
  expect_length(records$value, 4L)
})

test_that("add_structured_values reports malformed JSON with its cause", {
  records <- add_structured_values(turn_records(list(json_turn(string = '{"score": 0.'))))
  expect_null(records$value[[1]])
  expect_match(records$problem[[1]], "^Invalid JSON: ")
})

test_that("add_structured_values unwraps what ellmer wrapped, and only that", {
  wrapped <- json_turn(list(wrapper = list("a", "b")))
  expect_identical(
    add_structured_values(turn_records(list(wrapped)), needs_wrapper = TRUE)$value[[1]],
    list("a", "b")
  )
  expect_identical(
    add_structured_values(turn_records(list(wrapped)), needs_wrapper = FALSE)$value[[1]],
    list(wrapper = list("a", "b"))
  )
  bare <- json_turn(list(score = 1))
  records <- add_structured_values(turn_records(list(bare)), needs_wrapper = TRUE)
  expect_null(records$value[[1]])
  expect_match(records$problem[[1]], "no \"wrapper\" property")
})

test_that("extract_structured_value takes the first of several JSON contents, and says so", {
  ContentJson <- utils::getFromNamespace("ContentJson", "ellmer")
  turn <- ellmer::AssistantTurn(list(
    ContentJson(list(score = 1)), ContentJson(list(score = 2))
  ))
  expect_warning(got <- extract_structured_value(turn), "Found 2 JSON responses")
  expect_identical(got$value, list(score = 1))
})

# settle_response() ------------------------------------------------------------

test_that("settle_response lets a transport failure outrank everything", {
  settled <- settle_response(
    checked = list(ok = TRUE, value = list(score = 1)),
    problem = NA_character_, error = "HTTP 500", finish = "max_tokens"
  )
  expect_false(settled$ok)
  expect_equal(settled$stage, "transport")
  expect_equal(settled$error, "API request failed: HTTP 500")
  expect_false(settled$truncated)
})

test_that("settle_response lets the provider's finish reason outrank a parseable payload", {
  settled <- settle_response(
    checked = list(ok = TRUE, value = list(score = 1)),
    problem = NA_character_, error = NA_character_, finish = "max_tokens",
    output_tokens = 100
  )
  expect_false(settled$ok)
  expect_equal(settled$stage, "incomplete")
  expect_true(settled$truncated)
  expect_match(settled$error, "cut off at the max_tokens limit after 100 output tokens")

  filtered <- settle_response(NULL, "Data extraction failed: no JSON responses found.",
                              NA_character_, "content_filter")
  expect_equal(filtered$stage, "incomplete")
  expect_false(filtered$truncated)
  expect_match(filtered$error, "content filter")

  window <- settle_response(NULL, NA_character_, NA_character_, "context_window")
  expect_true(window$truncated)
})

test_that("settle_response reads a completed response through extraction to validation", {
  extraction <- settle_response(NULL, "Invalid JSON: premature EOF", NA_character_, "success")
  expect_equal(extraction$stage, "extraction")
  expect_equal(extraction$error, "Invalid JSON: premature EOF")

  nothing <- settle_response(NULL, NA_character_, NA_character_, "success")
  expect_equal(nothing$stage, "extraction")
  expect_equal(nothing$error, "The API returned an empty response.")

  schema <- settle_response(list(ok = FALSE, error = "$.score must be a number"),
                            NA_character_, NA_character_, "success")
  expect_equal(schema$stage, "schema")
  expect_equal(schema$error, "$.score must be a number")

  ok <- settle_response(list(ok = TRUE, value = list(score = 1)),
                        NA_character_, NA_character_, "success")
  expect_true(ok$ok)
  expect_equal(ok$stage, "ok")
  expect_identical(ok$value, list(score = 1))
})

test_that("settle_response treats an unknown finish reason and a forced tool call as completed", {
  for (finish in list(NA_character_, "tool_use", "stop_sequence")) {
    settled <- settle_response(list(ok = TRUE, value = list(score = 1)),
                               NA_character_, NA_character_, finish)
    expect_true(settled$ok)
  }
  odd <- settle_response(list(ok = TRUE, value = list(score = 1)),
                         NA_character_, NA_character_, "odd")
  expect_false(odd$ok)
  expect_match(odd$error, "finish reason \"odd\"")
})

# unit_error() -----------------------------------------------------------------

test_that("unit_error classes a failure by its stage", {
  expect_s3_class(unit_error("x", "schema"), "quallmer_schema_error")
  expect_s3_class(unit_error("x", "schema"), "quallmer_extraction_error")
  expect_s3_class(unit_error("x", "extraction"), "quallmer_extraction_error")
  expect_false(inherits(unit_error("x", "extraction"), "quallmer_schema_error"))
  expect_s3_class(unit_error("x", "incomplete", truncated = TRUE), "quallmer_truncation_error")
  expect_false(inherits(unit_error("x", "incomplete"), "quallmer_truncation_error"))
  expect_identical(class(unit_error("x", "transport")), c("simpleError", "error", "condition"))
  expect_equal(conditionMessage(unit_error(NULL, "transport")), "failed for an unrecorded reason")
  expect_true(is_schema_error(schema_error("x")))
  expect_true(is_extraction_error(schema_error("x")))
})

test_that("a classed unit error survives a round trip through RDS", {
  path <- withr::local_tempfile(fileext = ".rds")
  errors <- list(schema_error("$.score must be a number"),
                 truncation_error("cut off"),
                 NULL)
  saveRDS(errors, path)
  back <- readRDS(path)
  expect_identical(back, errors)
  expect_s3_class(back[[1]], "quallmer_schema_error")
  expect_s3_class(back[[2]], "quallmer_truncation_error")
})

# tabulate_results() -----------------------------------------------------------

test_that("tabulate_results converts once, in ellmer's column order", {
  schema <- ellmer::type_object(
    score = ellmer::type_number("Score"),
    lab = ellmer::type_enum(values = c("pos", "neg")),
    tags = ellmer::type_array(ellmer::type_string()),
    detail = ellmer::type_object(k = ellmer::type_string("K"))
  )
  values <- list(
    list(score = 1, lab = "pos", tags = list("a", "b"), detail = list(k = "x")),
    NULL,
    list(score = 3, lab = "neg", tags = list(), detail = list(k = "z"))
  )
  errors <- list(NULL, schema_error("$.score must be a number"), NULL)
  usage <- matrix(c(10, 5, 0, 0.1, 20, 30, 0, NA, 30, 7, 1, 0.3), nrow = 3, byrow = TRUE,
                  dimnames = list(NULL, c("input_tokens", "output_tokens",
                                          "cached_input_tokens", "cost")))

  out <- tabulate_results(values, errors, usage, schema,
                          include_tokens = TRUE, include_cost = TRUE)

  expect_equal(names(out), c("score", "lab", "tags", "detail", ".error",
                             "input_tokens", "output_tokens", "cached_input_tokens", "cost"))
  expect_equal(nrow(out), 3L)
  expect_equal(out$score, c(1, NA, 3))
  expect_s3_class(out$lab, "factor")
  expect_equal(as.character(out$lab), c("pos", NA, "neg"))
  expect_equal(out$tags, list(c("a", "b"), character(0), character(0)))
  expect_equal(out$detail$k, c("x", NA, "z"))
  expect_null(out$.error[[1]])
  expect_s3_class(out$.error[[2]], "quallmer_schema_error")
  # Usage for a failed unit is what it reported, NA included
  expect_equal(out$output_tokens, c(5, 30, 7))
  expect_equal(out$cost, c(0.1, NA, 0.3))
})

test_that("tabulate_results adds nothing that was not asked for", {
  schema <- score_schema()
  usage <- matrix(0, nrow = 1, ncol = 4,
                  dimnames = list(NULL, c("input_tokens", "output_tokens",
                                          "cached_input_tokens", "cost")))
  out <- tabulate_results(list(list(score = 1)), list(NULL), usage, schema)
  expect_equal(names(out), "score")

  out <- tabulate_results(list(list(score = 1)), list(NULL), usage, schema,
                          include_cost = TRUE)
  expect_equal(names(out), c("score", "cost"))
})

# structured_attempt() ---------------------------------------------------------

test_that("structured_attempt keeps valid rows beside failed ones, with their usage", {
  schema <- score_schema()
  turns <- list(
    json_turn(list(score = 0.5), tokens = c(10, 5, 0), cost = 0.1),
    json_turn(string = '{"score": "high"}', tokens = c(10, 6, 0), cost = 0.2),
    request_error("HTTP 500 Internal Server Error.", 500L),
    text_turn("prose", tokens = c(10, 7, 0), cost = 0.3),
    json_turn(string = '{"sco', tokens = c(10, 100, 0), cost = 0.4, finish_reason = "max_tokens"),
    NULL
  )
  expect_warning(
    attempt <- structured_attempt(turns, schema, provider = NULL,
                                  execution_args = list(include_tokens = TRUE,
                                                        include_cost = TRUE)),
    "5 responses from the structured call could not be coded, out of 6"
  )
  expect_true(attempt$ok)
  expect_equal(attempt$n_invalid, 5L)
  out <- attempt$value
  expect_equal(nrow(out), 6L)
  expect_equal(out$score, c(0.5, NA, NA, NA, NA, NA))
  classes <- vapply(out$.error, function(e) if (is.null(e)) "ok" else class(e)[[1]], "")
  expect_equal(classes, c("ok", "quallmer_schema_error", "simpleError",
                          "quallmer_extraction_error", "quallmer_truncation_error",
                          "simpleError"))
  expect_equal(out$output_tokens, c(5, 6, 0, 7, 100, 0))
  expect_equal(out$cost, c(0.1, 0.2, 0, 0.3, 0.4, 0))
  expect_identical(attempt$usage[, "cost"], out$cost)
})

test_that("structured_attempt judges the endpoint from completed responses only", {
  schema <- score_schema()

  # Every completed response is invalid: the endpoint ignores the schema
  ignored <- list(json_turn(string = "{}"), text_turn("prose"),
                  request_error("HTTP 500", 500L))
  attempt <- structured_attempt(ignored, schema, NULL, list())
  expect_false(attempt$ok)
  expect_true(attempt$invalid)
  expect_match(attempt$error, "no usable values")
  expect_match(attempt$error, "\\$\\.score is required but missing")
  expect_equal(dim(attempt$usage), c(3L, 4L))

  # Nothing completed: no evidence either way, so the failures stand
  unassessed <- list(
    request_error("HTTP 500", 500L),
    json_turn(string = '{"sco', finish_reason = "max_tokens"),
    text_turn("", finish_reason = "content_filter")
  )
  expect_warning(attempt <- structured_attempt(unassessed, schema, NULL, list()),
                 "3 responses")
  expect_true(attempt$ok)
  expect_null(attempt$invalid)

  # One valid response among invalid ones keeps the run
  mixed <- list(json_turn(string = "{}"), json_turn(list(score = 1)))
  expect_warning(attempt <- structured_attempt(mixed, schema, NULL, list()), "1 response")
  expect_true(attempt$ok)
})

test_that("structured_attempt reports a provider that refused every request", {
  schema <- score_schema()
  refused <- list(
    request_error("HTTP 404 Not Found. No such model.", 404L),
    request_error("HTTP 404 Not Found. No such model.", 404L)
  )
  attempt <- structured_attempt(refused, schema, NULL, list())
  expect_false(attempt$ok)
  expect_true(attempt$rejected)
  expect_equal(attempt$error, "API request failed: HTTP 404 Not Found. No such model.")

  # A transient refusal is not that
  transient <- list(request_error("HTTP 429", 429L), request_error("HTTP 404", 404L))
  expect_warning(attempt <- structured_attempt(transient, schema, NULL, list()))
  expect_true(attempt$ok)
  expect_null(attempt$rejected)
})

test_that("structured_attempt handles an empty run", {
  attempt <- structured_attempt(list(), score_schema(), NULL, list())
  expect_true(attempt$ok)
  expect_equal(nrow(attempt$value), 0L)
})

# The JSON handler on the same processor ----------------------------------------

test_that("the JSON path classes its failures by stage too", {
  codebook <- qlm_codebook("Test", "Rate it.", score_schema())
  turns <- list(
    list(last_turn = function() text_turn('{"score": 1}')),
    list(last_turn = function() text_turn('{"score": "high"}')),
    list(last_turn = function() text_turn("not json")),
    list(last_turn = function() text_turn('{"score":', finish_reason = "max_tokens")),
    request_error("HTTP 500", 500L)
  )
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) structure(list(), class = "Chat"))
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    turn_records(turns[seq_along(prompts)])
  })

  expect_warning(
    out <- h(letters[1:5], codebook, model = "openai_compatible/x",
             chat_args = list(), execution_args = list(include_tokens = TRUE),
             json_retries = 0L),
    "4 responses could not be coded"
  )
  expect_equal(out$score, c(1, NA, NA, NA, NA))
  classes <- vapply(out$.error, function(e) if (is.null(e)) "ok" else class(e)[[1]], "")
  expect_equal(classes, c("ok", "quallmer_schema_error", "quallmer_extraction_error",
                          "quallmer_truncation_error", "simpleError"))
  expect_equal(names(out), c("score", ".error", "input_tokens", "output_tokens",
                             "cached_input_tokens"))
})

test_that("the JSON path starts its usage from a prior structured attempt", {
  codebook <- qlm_codebook("Test", "Rate it.", score_schema())
  turns <- list(
    list(last_turn = function() text_turn('{"score": 1}', tokens = c(10, 5, 0), cost = 0.1)),
    list(last_turn = function() text_turn('{"score": 2}', tokens = c(10, 5, 0), cost = 0.1))
  )
  prior <- matrix(c(7, 3, 0, 0.05, NA, NA, NA, NA), nrow = 2, byrow = TRUE,
                  dimnames = list(NULL, c("input_tokens", "output_tokens",
                                          "cached_input_tokens", "cost")))
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) structure(list(), class = "Chat"))
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) turn_records(turns))

  out <- h(c("a", "b"), codebook, model = "openai_compatible/x",
           chat_args = list(), execution_args = list(include_tokens = TRUE, include_cost = TRUE),
           json_retries = 0L, prior_usage = prior)
  expect_equal(out$input_tokens, c(17, NA))
  expect_equal(out$cost, c(0.15, NA))
})
