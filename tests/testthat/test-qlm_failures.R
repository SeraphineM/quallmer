# Build a coded object directly, as test-qlm_code.R does, so no API is needed.
make_coded <- function(results, schema, n_units = nrow(results)) {
  codebook <- qlm_codebook("Test", "Test prompt", schema)
  new_qlm_coded(
    results = results,
    codebook = codebook,
    data = rep("text", nrow(results)),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = n_units),
    name = "run",
    call = quote(qlm_code(...)),
    parent = NULL
  )
}

flat_schema <- ellmer::type_object(
  score = ellmer::type_integer("Score."),
  note = ellmer::type_string("Optional note.", required = FALSE)
)


test_that("qlm_failures reports units carrying an .error, with the message", {
  http_error <- structure(
    class = c("httr2_http_401", "httr2_http", "httr2_error", "rlang_error",
              "error", "condition"),
    list(message = "HTTP 401 Unauthorized.", call = NULL)
  )
  results <- tibble::tibble(
    id = c("a", "b", "c"),
    score = c(2L, NA, NA),
    note = c("fine", NA, NA),
    .error = list(NULL, http_error, simpleError("Response was not valid JSON"))
  )
  coded <- make_coded(results, flat_schema)

  out <- qlm_failures(coded)
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c(".id", "reason", ".error"))
  expect_equal(out$.id, c("b", "c"))
  expect_equal(out$reason, c("HTTP 401 Unauthorized.", "Response was not valid JSON"))
  expect_s3_class(out$.error[[1]], "httr2_http_401")
  expect_s3_class(out$.error[[2]], "simpleError")
})


test_that("qlm_failures counts a unit whose required scalars are all NA, with no .error", {
  # HTTP 200, schema accepted and ignored: NA everywhere and no error recorded.
  # Observed with qwen3.5 through Model Studio (#136).
  results <- tibble::tibble(
    id = c("a", "b"),
    score = c(1L, NA),
    note = c(NA, NA)
  )
  coded <- make_coded(results, flat_schema)

  out <- qlm_failures(coded)
  expect_equal(out$.id, "b")
  expect_equal(out$reason, "every required property is NA")
  expect_null(out$.error[[1]])

  # `note` is not required, so its NA in unit "a" does not count against it
  expect_false("a" %in% out$.id)
})


test_that("qlm_failures returns zero rows when every unit was coded", {
  results <- tibble::tibble(id = 1:3, score = c(1L, 2L, 3L), note = c("x", NA, "z"))
  out <- qlm_failures(make_coded(results, flat_schema))

  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c(".id", "reason", ".error"))
})


test_that("qlm_failures on an array schema keys off .error, not the empty cell", {
  # The #132 case: a failed request leaves a zero-row tibble in the
  # list-column, which !is.na() reports as a value.
  array_schema <- ellmer::type_object(
    assessments = ellmer::type_array(
      items = ellmer::type_object(
        item = ellmer::type_string("Item."),
        score = ellmer::type_integer("0-3.")
      )
    )
  )
  empty <- tibble::tibble(item = character(), score = integer())
  http_error <- simpleError("HTTP 401 Unauthorized.")

  results <- tibble::tibble(
    id = c("a", "b", "c"),
    assessments = list(
      tibble::tibble(item = "apples", score = 2L),
      empty,
      empty
    ),
    .error = list(NULL, http_error, NULL)
  )
  coded <- make_coded(results, array_schema)

  # The obvious check gets this wrong
  expect_equal(sum(!is.na(coded$assessments)), 3L)

  out <- qlm_failures(coded)
  # "b" failed and says so. "c" returned a valid empty array with no error:
  # empty is not missing, so it is not reported.
  expect_equal(out$.id, "b")
  expect_equal(out$reason, "HTTP 401 Unauthorized.")
})


test_that("qlm_failures strips ANSI colour from error messages", {
  results <- tibble::tibble(
    id = "a", score = NA_integer_, note = NA_character_,
    .error = list(simpleError("HTTP 400 \033[31mBad Request\033[39m"))
  )
  out <- qlm_failures(make_coded(results, flat_schema))
  expect_equal(out$reason, "HTTP 400 Bad Request")
})


test_that("qlm_failures works for human-coded objects and rejects other input", {
  human <- as_qlm_coded(
    data.frame(.id = 1:3, sentiment = c("pos", NA, "neg")),
    name = "coder"
  )
  # No schema and no .error: nothing can be called a failure
  expect_equal(nrow(qlm_failures(human)), 0L)

  expect_error(qlm_failures(data.frame(.id = 1)), "must be a .*qlm_coded")
})


test_that("print.qlm_coded reports scored and failed counts only when some failed", {
  results <- tibble::tibble(
    id = c("a", "b", "c"),
    score = c(2L, NA, NA),
    note = c(NA, NA, NA),
    .error = list(NULL, simpleError("HTTP 401 Unauthorized."), NULL)
  )
  # "b" by .error, "c" by every required scalar being NA
  output <- capture.output(print(make_coded(results, flat_schema)))
  expect_true(any(grepl("^# Units:    3 \\(1 scored, 2 failed\\)$", output)))

  ok <- tibble::tibble(id = 1:3, score = 1:3, note = c("x", "y", "z"))
  output <- capture.output(print(make_coded(ok, flat_schema)))
  expect_true(any(grepl("^# Units:    3$", output)))
  expect_false(any(grepl("failed", output)))
})


test_that("print.qlm_coded distinguishes rows present from units attempted", {
  results <- tibble::tibble(
    id = c("a", "b", "c"),
    score = c(2L, NA, 3L),
    note = c(NA, NA, NA),
    .error = list(NULL, simpleError("HTTP 401 Unauthorized."), NULL)
  )
  coded <- make_coded(results, flat_schema)

  # Subsetting keeps the class and the original n_units
  subset <- coded[1:2, ]
  expect_s3_class(subset, "qlm_coded")
  output <- capture.output(print(subset))
  expect_true(any(grepl(
    "^# Units:    3 attempted, 2 present \\(1 scored, 1 failed\\)$", output
  )))

  # With no failures among the rows present, the breakdown is omitted
  output <- capture.output(print(coded[c(1, 3), ]))
  expect_true(any(grepl("^# Units:    3 attempted, 2 present$", output)))
})


test_that("parse_extraction_warning reads ellmer's per-row lines and ignores other warnings", {
  msg <- paste(
    "Failed to extract data from 2/3 turns",
    "* 2: Data extraction failed: no JSON responses found.",
    "* 3: Data extraction failed: \033[31mno JSON responses found.\033[39m",
    sep = "\n"
  )
  out <- parse_extraction_warning(msg)
  expect_equal(out$index, c(2L, 3L))
  expect_equal(out$message, rep("Data extraction failed: no JSON responses found.", 2))

  expect_null(parse_extraction_warning("Failed to extract data from 1/1 turns"))
  expect_null(parse_extraction_warning("Some other warning\n* 1: not ours"))
})


test_that("attach_extraction_errors records only rows without an .error, before usage columns", {
  results <- tibble::tibble(
    score = c(1L, NA, NA),
    input_tokens = c(10L, 0L, 12L),
    cost = c(0.1, 0, 0.1)
  )
  failures <- data.frame(index = c(2L, 3L, 9L), message = c("m2", "m3", "m9"))

  out <- attach_extraction_errors(results, failures)
  expect_equal(names(out), c("score", ".error", "input_tokens", "cost"))
  expect_null(out$.error[[1]])
  expect_equal(conditionMessage(out$.error[[2]]), "m2")
  # Tagged, so the schema-enforcement check can tell it from a request failure
  expect_s3_class(out$.error[[2]], "quallmer_extraction_error")
  expect_s3_class(out$.error[[2]], "simpleError")
  expect_equal(conditionMessage(out$.error[[3]]), "m3")

  # An .error ellmer already recorded takes precedence
  results$.error <- list(NULL, simpleError("request failed"), NULL)
  out <- attach_extraction_errors(results, failures)
  expect_equal(conditionMessage(out$.error[[2]]), "request failed")
  expect_equal(conditionMessage(out$.error[[3]]), "m3")

  # Nothing to attach, or nothing to attach to
  expect_identical(attach_extraction_errors(results, NULL), results)
  expect_identical(attach_extraction_errors(list(1), failures), list(1))
})


test_that("an extraction failure on an array-only schema is reported as failed", {
  # Pinned against ellmer's real multi_convert(): a turn that came back with
  # prose and no JSON. ellmer warns, naming the row, and drops the error; the
  # array cell is then a zero-row tibble with no .error, which is exactly the
  # blind spot #132 is about. If ellmer changes the warning's format this test
  # fails, which is the point: the blind spot must not come back silently.
  multi_convert <- utils::getFromNamespace("multi_convert", "ellmer")
  ContentJson <- utils::getFromNamespace("ContentJson", "ellmer")
  provider <- suppressMessages(
    ellmer::chat_openai(credentials = function() list(Authorization = "Bearer x"))
  )$get_provider()

  array_schema <- ellmer::type_object(
    assessments = ellmer::type_array(
      items = ellmer::type_object(
        item = ellmer::type_string("Item."),
        score = ellmer::type_integer("0-3.")
      )
    )
  )
  good <- ellmer::AssistantTurn(contents = list(
    ContentJson(list(assessments = list(list(item = "apples", score = 2L))))
  ))
  prose <- ellmer::AssistantTurn(contents = list(
    ellmer::ContentText("I am sorry, I cannot score this text.")
  ))

  # Without the capture, ellmer's own result carries no .error at all
  bare <- suppressWarnings(multi_convert(provider, list(good, prose), array_schema))
  expect_false(".error" %in% names(bare))
  expect_equal(nrow(bare$assessments[[2]]), 0L)

  expect_warning(
    results <- with_extraction_errors(
      multi_convert(provider, list(good, prose), array_schema)
    ),
    "Failed to extract data"
  )
  expect_true(".error" %in% names(results))
  expect_null(results$.error[[1]])
  expect_s3_class(results$.error[[2]], "error")

  results$id <- c("a", "b")
  out <- qlm_failures(make_coded(results, array_schema))
  expect_equal(out$.id, "b")
  expect_match(out$reason, "no JSON")
})
