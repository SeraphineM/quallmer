# Helpers ---------------------------------------------------------------------

backfill_schema <- ellmer::type_object(
  score = ellmer::type_integer("Score."),
  note = ellmer::type_string("Optional note.", required = FALSE)
)

# A coded object built directly, so no API is needed. `results` carries an
# `id` column; the inputs are named by it so the backfill can subset them.
make_run <- function(results, schema = backfill_schema, chat_args = list(),
                     execution_args = list(), backend = "structured",
                     name = "run1", n_units = nrow(results), levels = NULL) {
  codebook <- qlm_codebook("Test", "Test prompt", schema, levels = levels)
  data <- stats::setNames(paste0("text ", results$id), results$id)
  new_qlm_coded(
    results = results,
    codebook = codebook,
    data = data,
    input_type = "text",
    chat_args = c(list(name = "openai/gpt-4o-mini"), chat_args),
    execution_args = execution_args,
    batch = FALSE,
    metadata = list(timestamp = Sys.time(), n_units = n_units, backend = backend),
    name = name,
    call = quote(qlm_code(...)),
    parent = NULL
  )
}

# A stand-in for qlm_code(): pass k answers with the k-th table in `passes`,
# each a data frame with an `id` column, from which the rows for the units
# asked for are taken. Records every call in `calls`.
fake_qlm_code <- function(passes, calls = new.env()) {
  function(x, codebook, model, ..., batch = FALSE, name = NULL) {
    k <- (calls$n %||% 0L) + 1L
    calls$n <- k
    calls$args[[k]] <- c(list(x = x, model = model, batch = batch, name = name), list(...))
    table <- passes[[k]]
    rows <- table[match(names(x), table$id), , drop = FALSE]
    rows$id <- names(x)
    rownames(rows) <- NULL
    new_qlm_coded(
      results = rows,
      codebook = codebook,
      data = x,
      input_type = "text",
      chat_args = list(name = model),
      execution_args = list(),
      metadata = list(timestamp = Sys.time(), n_units = length(x)),
      name = name,
      call = quote(qlm_code(...)),
      parent = NULL
    )
  }
}

backfill_with <- function(passes, calls = new.env()) {
  f <- qlm_backfill
  mockery::stub(f, "qlm_code", fake_qlm_code(passes, calls))
  f
}

error_col <- function(...) {
  lapply(list(...), function(m) if (is.null(m)) NULL else simpleError(m))
}


# Guards ----------------------------------------------------------------------

test_that("qlm_backfill rejects what it cannot backfill", {
  expect_error(qlm_backfill(data.frame(a = 1)), "must be a")

  run <- make_run(data.frame(id = c("a", "b"), score = c(1L, NA), note = c(NA, NA)))
  expect_error(qlm_backfill(run, passes = 0), "single positive integer")
  expect_error(qlm_backfill(run, passes = c(1, 2)), "single positive integer")
  expect_error(qlm_backfill(run, passes = Inf), "single positive integer")
  expect_error(qlm_backfill(run, passes = NaN), "single positive integer")
  expect_error(qlm_backfill(run, passes = .Machine$integer.max + 1), "single positive integer")
  expect_error(qlm_backfill(run, model = c("a", "b")), "single string")
  expect_error(qlm_backfill(run, codebook = codebook(run)), "codebook cannot be changed")
  expect_error(qlm_backfill(run, batch = TRUE), "cannot be set")
  # `passes` must be the only bound on paid calls, and the run keeps its name
  expect_error(qlm_backfill(run, backfill = 3), "Use `passes`")
  expect_error(qlm_backfill(run, name = "retry"), "cannot be set")

  human <- qlm_humancoded(data.frame(.id = c("a", "b"), score = c(1L, 2L)), name = "coder")
  expect_error(qlm_backfill(human), "human-coded")
})


# Filling ---------------------------------------------------------------------

test_that("qlm_backfill re-codes only the failed units and merges them in place", {
  skip_if_not_installed("mockery")
  results <- data.frame(
    id = c("a", "b", "c", "d"),
    score = c(2L, NA, 4L, NA),
    note = c("fine", NA, NA, NA),
    input_tokens = c(10, 10, 10, 0), output_tokens = c(5, 5, 5, 0),
    cached_input_tokens = c(0, 0, 0, 0), cost = c(0.01, 0.01, 0.01, 0)
  )
  results$.error <- error_col(NULL, "Invalid JSON", NULL, "HTTP 500 Internal Server Error.")
  run <- make_run(
    results,
    chat_args = list(params = list(temperature = 0)),
    execution_args = list(include_tokens = TRUE, include_cost = TRUE, on_error = "continue")
  )
  expect_equal(qlm_failures(run)$.id, c("b", "d"))

  calls <- new.env()
  pass <- data.frame(
    id = c("b", "d"), score = c(3L, 5L), note = c("late", NA),
    input_tokens = c(11, 12), output_tokens = c(6, 7),
    cached_input_tokens = c(0, 0), cost = c(0.02, 0.03)
  )
  f <- backfill_with(list(pass), calls)

  expect_message(filled <- f(run), "Recovered 2 units; 0 still failed")

  # Only the failed units were sent, named by .id, with the run's own settings
  expect_equal(calls$n, 1)
  sent <- calls$args[[1]]
  expect_equal(names(sent$x), c("b", "d"))
  expect_equal(unname(sent$x), c("text b", "text d"))
  expect_equal(sent$model, "openai/gpt-4o-mini")
  expect_false(sent$batch)
  # Each pass is a single coding call, never a nested backfill
  expect_false(sent$backfill)
  expect_equal(sent$params, list(temperature = 0))
  expect_equal(sent$on_error, "continue")
  expect_true(sent$include_tokens)
  expect_equal(sent$structured, "structured")
  expect_equal(sent$name, "run1_backfill_1")

  # Same rows, same order; the good ones untouched, the failed ones filled
  expect_s3_class(filled, "qlm_coded")
  expect_equal(filled$.id, c("a", "b", "c", "d"))
  expect_equal(filled$score, c(2L, 3L, 4L, 5L))
  expect_equal(filled$note, c("fine", "late", NA, NA))
  expect_equal(nrow(qlm_failures(filled)), 0)
  expect_true(all(vapply(filled$.error, is.null, logical(1))))

  # Usage is summed across attempts, since the failed ones were billed too
  expect_equal(filled$input_tokens, c(10, 21, 10, 12))
  expect_equal(filled$output_tokens, c(5, 11, 5, 7))
  expect_equal(filled$cost, c(0.01, 0.03, 0.01, 0.03))

  # Identity and provenance
  expect_identical(attr(filled, "codebook"), attr(run, "codebook"))
  expect_identical(inputs(filled), inputs(run))
  expect_equal(qlm_meta(filled, "name"), "run1")
  expect_equal(qlm_meta(filled, "n_units", type = "object"), 4)
  passes <- qlm_meta(filled, "backfill", type = "object")
  expect_length(passes, 1)
  expect_equal(passes[[1]]$attempted, c("b", "d"))
  expect_equal(passes[[1]]$recovered, c("b", "d"))
  expect_s3_class(passes[[1]]$timestamp, "POSIXct")
})

test_that("qlm_backfill never overwrites a coding with a failed retry", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b"), score = c(1L, NA), note = c(NA, NA))
  results$.error <- error_col(NULL, "Invalid JSON")
  run <- make_run(results)

  # The retry fails again, with a different reason
  pass <- data.frame(id = "b", score = NA_integer_, note = NA_character_)
  pass$.error <- error_col("HTTP 429 Too Many Requests.")
  f <- backfill_with(list(pass, pass))

  expect_message(filled <- f(run), "No progress")
  expect_equal(filled$score, c(1L, NA))
  # The latest reason is recorded
  expect_equal(qlm_failures(filled)$reason, "HTTP 429 Too Many Requests.")
  # ... and the pass is on record as having recovered nothing
  passes <- qlm_meta(filled, "backfill", type = "object")
  expect_length(passes, 1)
  expect_equal(passes[[1]]$attempted, "b")
  expect_equal(passes[[1]]$recovered, character())
})

test_that("qlm_backfill makes a second pass for what the first did not recover", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b", "c"), score = c(NA, NA, 3L), note = NA_character_)
  results$.error <- error_col("Invalid JSON", "Invalid JSON", NULL)
  run <- make_run(results, backend = "json_mode")

  calls <- new.env()
  first <- data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_)
  first$.error <- error_col(NULL, "Invalid JSON")
  second <- data.frame(id = "b", score = 2L, note = NA_character_)
  f <- backfill_with(list(first, second), calls)

  filled <- suppressMessages(f(run, passes = 3))

  expect_equal(calls$n, 2)
  expect_equal(names(calls$args[[1]]$x), c("a", "b"))
  expect_equal(names(calls$args[[2]]$x), "b")
  # A run that took the JSON path is backfilled on it
  expect_equal(calls$args[[1]]$structured, "json")
  expect_equal(filled$score, c(1L, 2L, 3L))
  passes <- qlm_meta(filled, "backfill", type = "object")
  expect_length(passes, 2)
  expect_equal(passes[[1]]$recovered, "a")
  expect_equal(passes[[2]]$recovered, "b")
})

test_that("qlm_backfill stops after a pass that recovers nothing", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b"), score = c(NA, NA), note = NA_character_)
  run <- make_run(results)

  calls <- new.env()
  nothing <- data.frame(id = c("a", "b"), score = c(NA_integer_, NA_integer_), note = NA_character_)
  f <- backfill_with(list(nothing, nothing, nothing), calls)

  expect_message(filled <- f(run, passes = 3), "No progress")
  expect_equal(calls$n, 1)
  expect_equal(nrow(qlm_failures(filled)), 2)
})

test_that("qlm_backfill does nothing when nothing failed", {
  skip_if_not_installed("mockery")
  run <- make_run(data.frame(id = c("a", "b"), score = c(1L, 2L), note = NA_character_))
  calls <- new.env()
  f <- backfill_with(list(), calls)

  expect_message(out <- f(run), "Nothing to backfill")
  expect_null(calls$n)
  expect_identical(out, run)
  expect_null(qlm_meta(out, type = "object")$backfill)
})


# What is left alone ----------------------------------------------------------

test_that("qlm_backfill skips length rejections, and truncations unless params change", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b", "c", "d"), score = c(NA, NA, NA, NA), note = NA_character_)
  results$.error <- error_col(
    "HTTP 400 Bad Request. This model's maximum context length is 128000 tokens.",
    "The response was cut off at the max_tokens limit after 4,096 output tokens; raise the limit with params(max_tokens = ) to let the model finish.",
    "Invalid JSON",
    NULL
  )
  run <- make_run(results)

  calls <- new.env()
  pass <- data.frame(id = c("b", "c", "d"), score = c(1L, 2L, 3L), note = NA_character_)
  f <- backfill_with(list(pass), calls)

  expect_message(filled <- f(run), "Leaving 2 units alone")
  expect_equal(names(calls$args[[1]]$x), c("c", "d"))
  expect_equal(filled$score, c(NA, NA, 2L, 3L))

  # A higher params(max_tokens = ) is how the output limit is raised, so a
  # cut-off unit is retried then; the length rejection still is not
  calls <- new.env()
  f <- backfill_with(list(pass), calls)
  filled <- suppressMessages(f(run, params = ellmer::params(max_tokens = 32000)))
  expect_equal(names(calls$args[[1]]$x), c("b", "c", "d"))
  expect_equal(calls$args[[1]]$params$max_tokens, 32000)
  expect_equal(filled$score, c(NA, 1L, 2L, 3L))
  # ... and the overrides are on record
  expect_equal(qlm_meta(filled, "backfill", type = "object")[[1]]$overrides$params$max_tokens, 32000)

  # Other params leave the limit where it was, and so leave the unit alone;
  # so does a limit no higher than the run's own
  capped <- make_run(results, chat_args = list(params = list(max_tokens = 4096)))
  for (params in list(ellmer::params(temperature = 0), ellmer::params(max_tokens = 4096),
                      ellmer::params(max_tokens = 100))) {
    calls <- new.env()
    f <- backfill_with(list(pass), calls)
    suppressMessages(f(capped, params = params))
    expect_equal(names(calls$args[[1]]$x), c("c", "d"))
  }
  calls <- new.env()
  f <- backfill_with(list(pass), calls)
  suppressMessages(f(capped, params = ellmer::params(max_tokens = 8192)))
  expect_equal(names(calls$args[[1]]$x), c("b", "c", "d"))
})

test_that("raises_output_limit compares the override with the run's own limit", {
  expect_false(raises_output_limit(list(), list()))
  expect_false(raises_output_limit(list(params = list(temperature = 0)), list(params = list(max_tokens = 60))))
  expect_false(raises_output_limit(list(params = list(max_tokens = 60)), list(params = list(max_tokens = 60))))
  expect_false(raises_output_limit(list(params = list(max_tokens = 30)), list(params = list(max_tokens = 60))))
  expect_true(raises_output_limit(list(params = list(max_tokens = 61)), list(params = list(max_tokens = 60))))
  # No declared limit: the provider default applied, so any explicit one counts
  expect_true(raises_output_limit(list(params = list(max_tokens = 10)), list()))
})

test_that("qlm_backfill with another model retries everything, and records the model", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b", "c"), score = c(NA, NA, 3L), note = NA_character_)
  results$.error <- error_col(
    "HTTP 400 Bad Request. This model's maximum context length is 128000 tokens.",
    "The response was cut off at the max_tokens limit after 4,096 output tokens.",
    NULL
  )
  run <- make_run(results, backend = "structured")

  calls <- new.env()
  pass <- data.frame(id = c("a", "b"), score = c(1L, 2L), note = NA_character_)
  f <- backfill_with(list(pass), calls)

  expect_message(filled <- f(run, model = "deepseek/deepseek-chat"), "deepseek/deepseek-chat")

  sent <- calls$args[[1]]
  expect_equal(names(sent$x), c("a", "b"))
  expect_equal(sent$model, "deepseek/deepseek-chat")
  # The parent's coding path is not imposed on a different provider
  expect_null(sent$structured)
  expect_equal(filled$score, c(1L, 2L, 3L))

  passes <- qlm_meta(filled, "backfill", type = "object")
  expect_equal(passes[[1]]$model, "deepseek/deepseek-chat")
  expect_equal(passes[[1]]$recovered, c("a", "b"))
  # A pass with the run's own model records no model
  same <- backfill_with(list(data.frame(id = "a", score = 1L, note = NA_character_)))
  again <- suppressMessages(same(make_run(data.frame(id = "a", score = NA, note = NA_character_))))
  expect_null(qlm_meta(again, "backfill", type = "object")[[1]]$model)

  out <- capture.output(print(filled))
  expect_true(any(grepl("^# Backfill: 1 pass, recovered 2 of 2 \\(2 with deepseek/deepseek-chat\\)", out)))
})

test_that("qlm_backfill re-derives the skip list from what a pass recorded", {
  skip_if_not_installed("mockery")
  # Three units failed with no recorded reason (abandoned requests). The
  # first pass recovers "b", learns that "a" is too long, and fails "c"
  # transiently; the second pass must resend "c" only.
  results <- data.frame(id = c("a", "b", "c"), score = c(NA, NA, NA), note = NA_character_)
  run <- make_run(results)

  calls <- new.env()
  first <- data.frame(id = c("a", "b", "c"), score = c(NA_integer_, 2L, NA_integer_), note = NA_character_)
  first$.error <- error_col("Request too large: maximum context length exceeded", NULL, "Invalid JSON")
  second <- data.frame(id = "c", score = 3L, note = NA_character_)
  f <- backfill_with(list(first, second), calls)

  filled <- suppressMessages(f(run, passes = 3))
  expect_equal(calls$n, 2)
  expect_equal(names(calls$args[[1]]$x), c("a", "b", "c"))
  expect_equal(names(calls$args[[2]]$x), "c")
  expect_equal(filled$score, c(NA, 2L, 3L))
  expect_match(qlm_failures(filled)$reason, "context length")
})

test_that("is_terminal_failure classifies recorded errors", {
  errors <- list(
    NULL,
    simpleError("HTTP 400 Bad Request. This model's maximum context length is 128000 tokens."),
    truncation_error("The response was cut off at the max_tokens limit after 60 output tokens."),
    simpleError("Invalid JSON"),
    # A validation error that happens to name a schema property: not a cut-off
    simpleError("$ has unexpected property: max_tokens.")
  )
  expect_equal(is_terminal_failure(errors), c(FALSE, TRUE, TRUE, FALSE, FALSE))
  # A raised limit frees the cut-off unit, not the over-long one
  expect_equal(is_terminal_failure(errors, limit_raised = TRUE), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # Nothing is terminal under a different model
  expect_equal(is_terminal_failure(errors, model_changed = TRUE), rep(FALSE, 5))
})

test_that("is_output_truncation goes by class, with the known phrasings for older objects", {
  expect_true(is_output_truncation(truncation_error("anything")))
  expect_true(is_output_truncation(simpleError("The response was cut off at the max_tokens limit after 60 output tokens; raise it.")))
  expect_true(is_output_truncation(simpleError("The response used the whole max_tokens limit of 4,096 and returned nothing.")))
  expect_true(is_output_truncation("Response was truncated because it hit the `max_tokens` limit."))
  expect_false(is_output_truncation(simpleError("$ has unexpected property: max_tokens.")))
  expect_false(is_output_truncation(simpleError("Invalid JSON")))
  expect_false(is_output_truncation(NULL))
  expect_false(is_output_truncation(NA_character_))
})


# Failures of the backfill itself ---------------------------------------------

test_that("qlm_backfill errors when the first pass fails outright, warns later", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b"), score = c(NA, NA), note = NA_character_)
  run <- make_run(results)

  f <- qlm_backfill
  mockery::stub(f, "qlm_code", function(...) stop("Every request was rejected by the provider."))
  expect_error(suppressMessages(f(run)), "before recovering anything")

  # Pass one recovers "a", pass two blows up: keep "a"
  calls <- new.env()
  first <- data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_)
  fake <- fake_qlm_code(list(first), calls)
  g <- qlm_backfill
  mockery::stub(g, "qlm_code", function(...) {
    if ((calls$n %||% 0L) >= 1L) stop("HTTP 503 Service Unavailable.")
    fake(...)
  })
  expect_warning(filled <- suppressMessages(g(run)), "keeping what earlier passes recovered")
  expect_equal(filled$score, c(1L, NA))

  # The failed pass is on record too: it was attempted, and may have been
  # billed, even though nothing from it was merged
  passes <- qlm_meta(filled, "backfill", type = "object")
  expect_length(passes, 2)
  expect_equal(passes[[1]]$recovered, "a")
  expect_null(passes[[1]]$error)
  expect_equal(passes[[2]]$attempted, "b")
  expect_equal(passes[[2]]$recovered, character(0))
  expect_match(passes[[2]]$error, "503")
  expect_equal(backfill_summary(passes), "2 passes (1 failed), recovered 1 of 2")
  expect_true(any(grepl("^# Backfill: 2 passes \\(1 failed\\)", capture.output(print(filled)))))
})


# Identifying units -----------------------------------------------------------

test_that("qlm_backfill finds the inputs of a reordered or subset object by .id", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b", "c"), score = c(1L, NA, NA), note = NA_character_)
  run <- make_run(results)

  # Rows reversed, then the first dropped: positions no longer index inputs()
  shuffled <- run[c(3, 2, 1), ]
  shuffled <- shuffled[-1, ]
  expect_equal(shuffled$.id, c("b", "a"))

  calls <- new.env()
  pass <- data.frame(id = "b", score = 2L, note = NA_character_)
  f <- backfill_with(list(pass), calls)
  filled <- suppressMessages(f(shuffled))

  sent <- calls$args[[1]]$x
  expect_equal(names(sent), "b")
  expect_equal(unname(sent), "text b")
  expect_equal(filled$.id, c("b", "a"))
  expect_equal(filled$score, c(2L, 1L))
})

test_that("inputs_by_id maps names, or positions for unnamed input, and refuses the rest", {
  named <- make_run(data.frame(id = c("a", "b"), score = c(1L, 2L), note = NA_character_))
  expect_equal(inputs_by_id(named, "b"), c(b = "text b"))
  expect_error(inputs_by_id(named, c("b", "zz")), "Cannot find the input for 1 unit")

  # Unnamed input: qlm_code() numbers the units, so .id is the position
  codebook <- qlm_codebook("Test", "Test prompt", backfill_schema)
  unnamed <- new_qlm_coded(
    results = data.frame(id = 1:3, score = c(1L, NA, 3L), note = NA_character_),
    codebook = codebook, data = c("first", "second", "third"), input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"), execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "run1", call = quote(qlm_code(...)), parent = NULL
  )
  expect_equal(inputs_by_id(unnamed, c("3", "2")), c("3" = "third", "2" = "second"))
  expect_error(inputs_by_id(unnamed, "4"), "Cannot find the input")
  expect_error(inputs_by_id(unnamed, "b"), "Cannot find the input")
})

test_that("qlm_backfill and the merge refuse duplicated .id values", {
  run <- make_run(data.frame(id = c("a", "b"), score = c(NA, NA), note = NA_character_))
  # The constructor no longer lets this happen; forge it after the fact
  forged <- run
  forged$.id <- c("dup", "dup")
  expect_error(qlm_backfill(forged), "must be unique")
  retry <- make_run(data.frame(id = "dup", score = 1L, note = NA_character_))
  expect_error(merge_backfill_rows(forged, retry), "must be unique")
})


# Merging ---------------------------------------------------------------------

test_that("merge_backfill_rows handles arrays, nested objects and factors", {
  schema <- ellmer::type_object(
    tags = ellmer::type_array(ellmer::type_string("A tag.")),
    meta = ellmer::type_object(a = ellmer::type_string("A."), b = ellmer::type_integer("B.")),
    score = ellmer::type_integer("Score.")
  )
  convert <- function(rows) {
    out <- ellmer:::convert_from_type(rows, ellmer::type_array(schema))
    out
  }
  original <- convert(list(
    list(tags = list("x"), meta = list(a = "p", b = 1L), score = 1L),
    NULL,
    NULL
  ))
  original$id <- c("a", "b", "c")
  original$.error <- error_col(NULL, "Invalid JSON", "Invalid JSON")
  run <- make_run(original, schema = schema)
  expect_equal(qlm_failures(run)$.id, c("b", "c"))

  retry <- convert(list(
    list(tags = list("y", "z"), meta = list(a = "q", b = 2L), score = 2L),
    NULL
  ))
  retry$id <- c("b", "c")
  retry$.error <- error_col(NULL, "still not JSON")
  new <- make_run(retry, schema = schema)

  merged <- merge_backfill_rows(run, new)

  expect_s3_class(merged, "qlm_coded")
  expect_equal(merged$.id, c("a", "b", "c"))
  expect_equal(merged$tags[[1]], "x")
  expect_equal(merged$tags[[2]], c("y", "z"))
  expect_equal(merged$meta$a, c("p", "q", NA))
  expect_equal(merged$meta$b, c(1L, 2L, NA))
  expect_equal(merged$score, c(1L, 2L, NA))
  expect_null(merged$.error[[2]])
  expect_equal(conditionMessage(merged$.error[[3]]), "still not JSON")
  expect_identical(attr(merged, "meta"), attr(run, "meta"))
})

test_that("merge_backfill_rows reconciles column types and adds .error where needed", {
  # An original whose failed column is all NA can carry a bare logical NA
  original <- data.frame(id = c("a", "b"), score = c(NA, NA), note = NA)
  run <- make_run(original)
  retry <- data.frame(id = "b", score = 2L, note = "ok")
  new <- make_run(retry)

  merged <- merge_backfill_rows(run, new)
  expect_equal(merged$score, c(NA, 2L))
  expect_equal(merged$note, c(NA, "ok"))
  expect_false(".error" %in% names(merged))

  # .error appears, before any usage columns, when a retry records one
  original$input_tokens <- c(1, 1)
  run <- make_run(original)
  retry <- data.frame(id = "b", score = NA_integer_, note = NA_character_, input_tokens = 2)
  retry$.error <- error_col("HTTP 500")
  merged <- merge_backfill_rows(run, make_run(retry))
  expect_equal(names(merged), c(".id", "score", "note", ".error", "input_tokens"))
  expect_equal(merged$input_tokens, c(1, 3))
  expect_equal(conditionMessage(merged$.error[[2]]), "HTTP 500")
})

test_that("merge_backfill_rows refuses units that are not in the run", {
  run <- make_run(data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_))
  stray <- make_run(data.frame(id = "z", score = 1L, note = NA_character_))
  expect_error(merge_backfill_rows(run, stray), "not in the original run")
})


# Reporting -------------------------------------------------------------------

test_that("print reports a backfill", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b", "c"), score = c(1L, NA, NA), note = NA_character_)
  run <- make_run(results)
  pass <- data.frame(id = c("b", "c"), score = c(2L, NA_integer_), note = NA_character_)
  f <- backfill_with(list(pass, pass))
  filled <- suppressMessages(f(run))

  out <- capture.output(print(filled))
  expect_true(any(grepl("^# Units:    3 \\(2 scored, 1 failed\\)", out)))
  expect_true(any(grepl("^# Backfill: 2 passes, recovered 1 of 2", out)))
})


# Replaying on a replication --------------------------------------------------

test_that("replay_backfill repeats the parent's passes, in order, until nothing is left", {
  skip_if_not_installed("mockery")
  parent <- make_run(data.frame(id = c("a", "b"), score = c(1L, 2L), note = NA_character_))
  meta_attr <- attr(parent, "meta")
  meta_attr$object$backfill <- list(
    list(model = NULL, overrides = list(), attempted = "a", recovered = "a"),
    list(model = "deepseek/deepseek-chat", overrides = list(params = list(max_tokens = 100)),
         attempted = "b", recovered = "b")
  )
  attr(parent, "meta") <- meta_attr

  # A replication with two failures, and a stand-in backfill that fixes one
  # unit per call and records how it was called
  calls <- list()
  fake_backfill <- function(x, ..., model = NULL, passes = 2L) {
    calls[[length(calls) + 1L]] <<- list(model = model, passes = passes, dots = list(...))
    i <- which(is.na(x$score))[1]
    x$score[i] <- 9L
    x
  }
  f <- replay_backfill
  mockery::stub(f, "qlm_backfill", fake_backfill)
  replication <- make_run(data.frame(id = c("a", "b"), score = c(NA, NA), note = NA_character_))

  expect_message(out <- f(replication, parent), "Replaying the 2 backfill passes")
  expect_length(calls, 2)
  expect_null(calls[[1]]$model)
  expect_equal(calls[[1]]$passes, 1L)
  expect_equal(calls[[2]]$model, "deepseek/deepseek-chat")
  expect_equal(calls[[2]]$dots$params$max_tokens, 100)
  expect_equal(out$score, c(9L, 9L))

  # Stops as soon as the replication is complete
  calls <- list()
  one_gap <- make_run(data.frame(id = c("a", "b"), score = c(NA, 2L), note = NA_character_))
  suppressMessages(f(one_gap, parent))
  expect_length(calls, 1)

  # FALSE and 0 do nothing; TRUE runs a default backfill even with no passes
  # on record, and an integer that many passes, fresh rather than replayed
  calls <- list()
  expect_identical(f(replication, parent, backfill = FALSE), replication)
  expect_identical(f(replication, parent, backfill = 0), replication)
  expect_length(calls, 0)
  plain <- make_run(data.frame(id = "a", score = 1L, note = NA_character_))
  expect_identical(f(replication, plain), replication)
  f(replication, plain, backfill = TRUE)
  expect_length(calls, 1)
  expect_equal(calls[[1]]$passes, 2L)
  calls <- list()
  f(replication, parent, backfill = 3)
  expect_length(calls, 1)
  expect_equal(calls[[1]]$passes, 3L)
  expect_null(calls[[1]]$model)

  expect_error(f(replication, parent, backfill = "yes"), "single non-negative integer")
  expect_error(f(replication, parent, backfill = Inf), "single non-negative integer")
})


test_that("qlm_trail discloses a backfill, and the other model, in print and report", {
  skip_if_not_installed("mockery")
  results <- data.frame(id = c("a", "b", "c"), score = c(1L, NA, NA), note = NA_character_)
  run <- make_run(results)
  pass <- data.frame(id = c("b", "c"), score = c(2L, 3L), note = NA_character_)
  f <- backfill_with(list(pass))
  filled <- suppressMessages(f(run, model = "deepseek/deepseek-chat"))

  trail <- qlm_trail(filled)
  out <- capture.output(print(trail))
  expect_true(any(grepl("^Backfill: 1 pass, recovered 2 of 2 \\(2 with deepseek/deepseek-chat\\)", out)))

  # The same line in the multi-run listing and in the report
  parent <- make_run(data.frame(id = "a", score = 1L, note = NA_character_), name = "run0")
  meta_attr <- attr(filled, "meta"); meta_attr$object$parent <- "run0"; attr(filled, "meta") <- meta_attr
  out <- capture.output(print(qlm_trail(parent, filled)))
  expect_true(any(grepl("Backfill: 1 pass, recovered 2 of 2", out)))

  # qlm_trail() adds the extensions itself
  stem <- tempfile()
  qlm_trail(parent, filled, path = stem)
  report <- readLines(paste0(stem, ".qmd"))
  expect_true(any(grepl("\\*\\*Backfill:\\*\\* 1 pass, recovered 2 of 2", report)))
})


test_that("replay_backfill keeps the replication when a replayed pass fails", {
  skip_if_not_installed("mockery")
  parent <- make_run(data.frame(id = c("a", "b"), score = c(1L, 2L), note = NA_character_))
  meta_attr <- attr(parent, "meta")
  meta_attr$object$backfill <- list(
    list(model = NULL, overrides = list(), attempted = c("a", "b"), recovered = "a"),
    list(model = "deepseek/deepseek-chat", overrides = list(), attempted = "b", recovered = "b")
  )
  attr(parent, "meta") <- meta_attr

  # The real qlm_backfill(), whose coding call succeeds once and then fails
  calls <- new.env()
  first <- data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_)
  fake <- fake_qlm_code(list(first), calls)
  backfill <- qlm_backfill
  mockery::stub(backfill, "qlm_code", function(...) {
    if ((calls$n %||% 0L) >= 1L) stop("HTTP 503 again.")
    fake(...)
  })
  f <- replay_backfill
  mockery::stub(f, "qlm_backfill", backfill)
  replication <- make_run(data.frame(id = c("a", "b"), score = c(NA, NA), note = NA_character_))

  # Each replayed pass is a first attempt to qlm_backfill(); its abort must
  # not discard the paid replication and pass 1's recovery
  expect_warning(
    out <- suppressMessages(f(replication, parent)),
    "Replayed backfill pass 2 failed; keeping the replication"
  )
  expect_equal(out$score, c(1L, NA))
  expect_equal(calls$n, 1)

  passes <- qlm_meta(out, "backfill", type = "object")
  expect_length(passes, 2)
  expect_equal(passes[[1]]$recovered, "a")
  expect_null(passes[[1]]$error)
  expect_equal(passes[[2]]$model, "deepseek/deepseek-chat")
  expect_equal(passes[[2]]$attempted, "b")
  expect_equal(passes[[2]]$recovered, character(0))
  expect_match(passes[[2]]$error, "503 again")
  expect_equal(backfill_summary(passes), "2 passes (1 failed), recovered 1 of 2")

  # A failure before any recovery still aborts a direct call
  expect_error(suppressMessages(backfill(replication)), class = "quallmer_backfill_error")
})


# Usage and credentials across the merge and the trail ------------------------

test_that("merge_backfill_rows keeps a usage total unknown when any attempt's is", {
  usage <- function(id, score, input, output, cost) {
    data.frame(id = id, score = score, note = NA_character_, input_tokens = input,
               output_tokens = output, cached_input_tokens = 0L, cost = cost)
  }
  run <- make_run(usage(c("a", "b", "c", "d"), c(1L, NA, NA, NA),
                        c(10L, 100L, NA, 30L), c(5L, 20L, NA, 8L), c(0.1, NA, NA, 0.3)))
  pass <- make_run(usage(c("b", "c", "d"), c(2L, 3L, 4L),
                         c(50L, 60L, 70L), c(9L, 9L, 9L), c(0.25, 0.25, NA)),
                   name = "run1_backfill_1")
  merged <- merge_backfill_rows(run, pass)
  expect_equal(merged$score, c(1L, 2L, 3L, 4L))
  # b: known tokens on both attempts add up; an unpriced first attempt keeps
  #    the cost unknown even though the retry was priced
  # c: a first attempt that reported nothing leaves everything unknown
  # d: a priced first attempt and an unpriced retry is unknown too
  expect_equal(merged$input_tokens, c(10L, 150L, NA, 100L))
  expect_equal(merged$output_tokens, c(5L, 29L, NA, 17L))
  expect_equal(merged$cost, c(0.1, NA, NA, NA))

  # A pass that did not record usage leaves the rows it touched unknown, and
  # the rest as they were
  bare <- make_run(data.frame(id = "b", score = 2L, note = NA_character_), name = "run1_backfill_1")
  merged <- merge_backfill_rows(run, bare)
  expect_equal(merged$input_tokens, c(10L, NA, NA, 30L))
  expect_equal(merged$cost, c(0.1, NA, NA, 0.3))

  # A pass that recorded usage the run did not adds nothing: the earlier
  # attempts' usage is unknown, so there is no truthful total
  plain <- make_run(data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_))
  merged <- merge_backfill_rows(plain, make_run(usage("b", 2L, 50L, 9L, 0.25), name = "run1_backfill_1"))
  expect_equal(names(merged), c(".id", "score", "note"))
  expect_equal(merged$score, c(1L, 2L))
})

test_that("qlm_backfill records usage as the run did", {
  skip_if_not_installed("mockery")
  run <- make_run(data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_))
  expect_error(qlm_backfill(run, include_tokens = TRUE), "cannot be set in a backfill")
  expect_error(qlm_backfill(run, include_cost = FALSE), "cannot be set in a backfill")
  # Rates need token counts to work on, which this run did not record
  expect_error(qlm_backfill(run, prices = c(input = 1, output = 2)), "recorded no usage")

  # With usage recorded, rates are accepted and reach the pass
  priced <- make_run(data.frame(
    id = c("a", "b"), score = c(1L, NA), note = NA_character_,
    input_tokens = 1L, output_tokens = 1L, cached_input_tokens = 0L, cost = NA_real_
  ))
  calls <- new.env()
  f <- backfill_with(list(data.frame(id = "b", score = 2L, note = NA_character_)), calls)
  suppressMessages(f(priced, prices = c(input = 1, output = 2)))
  expect_equal(calls$args[[1]]$prices, c(input = 1, output = 2))
})

test_that("qlm_trail redacts the credentials a backfill pass was given (#154)", {
  skip_if_not_installed("mockery")
  run <- make_run(data.frame(id = c("a", "b"), score = c(1L, NA), note = NA_character_))
  f <- backfill_with(list(data.frame(id = "b", score = 2L, note = NA_character_)))
  hidden <- "env3cret"
  filled <- suppressMessages(f(
    run,
    api_key = "sk-b4ckfill",
    base_url = "https://u:url3cret@proxy.example/v1",
    api_headers = c(Authorization = "Bearer hdr3cret"),
    credentials = local({ captured <- hidden; function() "cb3cret" })
  ))
  secrets <- c("sk-b4ckfill", "url3cret", "hdr3cret", "cb3cret", "env3cret")

  # On the object itself the pass is recorded as given
  expect_equal(qlm_meta(filled, "backfill", type = "object")[[1]]$overrides$api_key, "sk-b4ckfill")

  stem <- tempfile()
  trail <- suppressMessages(qlm_trail(filled, path = stem))
  overrides <- attr(trail$runs[[1]]$coded, "meta")$object$backfill[[1]]$overrides
  expect_equal(overrides$api_key, "<redacted>")
  expect_equal(overrides$base_url, "https://proxy.example/v1")
  expect_equal(overrides$api_headers, c(Authorization = "<redacted>"))
  expect_equal(overrides$credentials, "<redacted>")

  rds_file <- paste0(stem, ".rds")
  bytes <- memDecompress(readBin(rds_file, "raw", file.size(rds_file)), "gzip")
  report <- readLines(paste0(stem, ".qmd"))
  for (secret in secrets) {
    expect_length(grepRaw(secret, bytes, fixed = TRUE), 0)
    expect_false(any(grepl(secret, report, fixed = TRUE)), label = secret)
  }
})

test_that("replay_backfill does not send a credential the trail redacted (#154)", {
  skip_if_not_installed("mockery")
  parent <- make_run(data.frame(id = c("a", "b"), score = c(1L, 2L), note = NA_character_))
  meta_attr <- attr(parent, "meta")
  meta_attr$object$backfill <- list(list(
    model = NULL,
    overrides = list(
      api_key = "<redacted>",
      api_headers = c(Authorization = "<redacted>", `anthropic-beta` = "b"),
      base_url = "https://h/v1?api_key=<redacted>",
      params = list(temperature = 0)
    ),
    attempted = c("a", "b"), recovered = c("a", "b")
  ))
  attr(parent, "meta") <- meta_attr

  seen <- NULL
  f <- replay_backfill
  mockery::stub(f, "qlm_backfill", function(x, ...) { seen <<- list(...); x })
  replication <- make_run(data.frame(id = c("a", "b"), score = c(NA, NA), note = NA_character_))

  msgs <- capture_messages(f(replication, parent))
  expect_true(any(grepl("Pass 1: `api_key`, `api_headers`, `base_url` carry values redacted", msgs)))
  expect_false("api_key" %in% names(seen))
  expect_equal(seen$api_headers, c(`anthropic-beta` = "b"))
  expect_equal(seen$base_url, "https://h/v1")
  expect_equal(seen$params, list(temperature = 0))
  expect_equal(seen$passes, 1L)
})

usage_rows <- function(id, score, cost, tokens = 10L) {
  data.frame(id = id, score = score, note = NA_character_, input_tokens = tokens,
             output_tokens = 5L, cached_input_tokens = 0L, cost = cost)
}

test_that("a pass that throws leaves the usage of the units it attempted unknown", {
  skip_if_not_installed("mockery")
  run <- make_run(usage_rows(c("a", "b", "c"), c(1L, NA, NA), 0.2))
  calls <- new.env()
  fake <- fake_qlm_code(list(usage_rows(c("b", "c"), c(2L, NA), 0.06)), calls)
  f <- qlm_backfill
  mockery::stub(f, "qlm_code", function(...) {
    if ((calls$n %||% 0L) >= 1L) stop("HTTP 503.")
    fake(...)
  })
  expect_warning(out <- suppressMessages(f(run, passes = 3L)), "pass 2 failed")

  # a: untouched. b: recovered by pass 1, both attempts' usage added up.
  # c: attempted again by the pass that threw, which may have been billed,
  #    so no total is known
  expect_equal(out$score, c(1L, 2L, NA))
  expect_equal(out$cost, c(0.2, 0.26, NA))
  expect_equal(out$input_tokens, c(10L, 20L, NA))
  passes <- qlm_meta(out, "backfill", type = "object")
  expect_equal(passes[[2]]$attempted, "c")
  expect_match(passes[[2]]$error, "503")
})

test_that("replay_backfill leaves the usage of a failed replayed pass unknown", {
  skip_if_not_installed("mockery")
  parent <- make_run(usage_rows(c("a", "b"), c(1L, 2L), 0.1))
  meta_attr <- attr(parent, "meta")
  meta_attr$object$backfill <- list(
    list(model = NULL, overrides = list(), attempted = c("a", "b"), recovered = "a"),
    list(model = NULL, overrides = list(), attempted = "b", recovered = "b")
  )
  attr(parent, "meta") <- meta_attr

  calls <- new.env()
  fake <- fake_qlm_code(list(usage_rows(c("a", "b"), c(1L, NA), 0.05)), calls)
  backfill <- qlm_backfill
  mockery::stub(backfill, "qlm_code", function(...) {
    if ((calls$n %||% 0L) >= 1L) stop("HTTP 503 again.")
    fake(...)
  })
  f <- replay_backfill
  mockery::stub(f, "qlm_backfill", backfill)
  replication <- make_run(usage_rows(c("a", "b"), c(NA, NA), 0.1))

  expect_warning(out <- suppressMessages(f(replication, parent)), "Replayed backfill pass 2 failed")
  expect_equal(out$score, c(1L, NA))
  expect_equal(out$cost, c(0.15, NA))
  expect_equal(out$input_tokens, c(20L, NA))
})

test_that("a pass records the rates it was costed on, and print and the trail disclose them", {
  skip_if_not_installed("mockery")
  run_rates <- c(input = 1, output = 2, cached_input = 1)
  run <- make_run(usage_rows(c("a", "b"), c(1L, NA), c(0.1, NA)))
  meta_attr <- attr(run, "meta")
  meta_attr$user$prices <- run_rates
  meta_attr$user$cost_note <- prices_note(run_rates)
  attr(run, "meta") <- meta_attr

  # The pass is costed on other rates; the stub records what qlm_code() would
  pass_rates <- c(input = 2, output = 4, cached_input = 2)
  f <- qlm_backfill
  mockery::stub(f, "qlm_code", function(x, codebook, model, ..., name = NULL) {
    out <- make_run(usage_rows("b", 2L, 0.04), name = name)
    m <- attr(out, "meta")
    m$user$prices <- pass_rates
    m$user$cost_note <- prices_note(pass_rates)
    attr(out, "meta") <- m
    out
  })
  filled <- suppressMessages(f(run, prices = pass_rates))

  pass <- qlm_meta(filled, "backfill", type = "object")[[1]]
  expect_equal(pass$prices, pass_rates)
  expect_equal(pass$cost_note, prices_note(pass_rates))

  out <- capture.output(print(filled))
  expect_true(any(grepl("^# Cost:     from supplied rates: \\$1 input, \\$2 output", out)))
  expect_true(any(grepl("^# Cost \\(backfill pass 1\\): from supplied rates: \\$2 input, \\$4 output", out)))

  stem <- tempfile()
  qlm_trail(filled, path = stem)
  report <- readLines(paste0(stem, ".qmd"))
  expect_true(any(grepl("^\\*\\*Cost:\\*\\* from supplied rates: \\$1 input", report)))
  expect_true(any(grepl("^\\*\\*Cost \\(backfill pass 1\\):\\*\\* from supplied rates: \\$2 input", report)))
})

test_that("backfill_cost_notes names only the passes costed differently from the run", {
  expect_length(backfill_cost_notes(list(list(cost_note = "x")), "x"), 0)
  expect_length(backfill_cost_notes(list(list(), list(attempted = "a")), NULL), 0)
  expect_equal(
    backfill_cost_notes(list(list(cost_note = "x"), list(cost_note = "x"), list(cost_note = "y")), "x"),
    c(`backfill pass 3` = "y")
  )
  # A run priced by ellmer has no note; a pass that was not is disclosed
  expect_equal(backfill_cost_notes(list(list(cost_note = "NA (unpriced)")), NULL),
               c(`backfill pass 1` = "NA (unpriced)"))
  # The reverse: a run on supplied rates, and a pass ellmer priced itself,
  # which records no note. Silence would read as the run's rates
  expect_equal(backfill_cost_notes(list(list(cost_note = "x"), list()), "x"),
               c(`backfill pass 2` = "from ellmer's price table"))
  # A pass that failed outright merged nothing and is not a source of cost
  expect_length(backfill_cost_notes(list(list(error = "HTTP 503")), "x"), 0)
})

test_that("an ellmer-priced pass on a run costed from supplied rates is disclosed", {
  skip_if_not_installed("mockery")
  run_rates <- c(input = 1, output = 2, cached_input = 1)
  run <- make_run(usage_rows(c("a", "b"), c(1L, NA), c(0.1, NA)))
  meta_attr <- attr(run, "meta")
  meta_attr$user$prices <- run_rates
  meta_attr$user$cost_note <- prices_note(run_rates)
  attr(run, "meta") <- meta_attr

  # The pass came back priced by ellmer: cost filled, no note recorded
  f <- backfill_with(list(usage_rows("b", 2L, 0.04)))
  filled <- suppressMessages(f(run))
  pass <- qlm_meta(filled, "backfill", type = "object")[[1]]
  expect_null(pass$cost_note)
  expect_null(pass$prices)

  out <- capture.output(print(filled))
  expect_true(any(grepl("^# Cost:     from supplied rates: \\$1 input", out)))
  expect_true(any(grepl("^# Cost \\(backfill pass 1\\): from ellmer's price table$", out)))

  stem <- tempfile()
  qlm_trail(filled, path = stem)
  report <- readLines(paste0(stem, ".qmd"))
  expect_true(any(grepl("^\\*\\*Cost \\(backfill pass 1\\):\\*\\* from ellmer's price table$", report)))
})


# Shipped example objects (#173) ---------------------------------------------

# The workflow guide and the examples of qlm_failures() and qlm_backfill()
# read these from inst/extdata; the guide's prose depends on the shape
# checked here, not on the particular units.

shipped_examples <- function() {
  readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
}

cut_off <- function(errors) {
  vapply(errors, inherits, logical(1), "quallmer_truncation_error")
}

test_that("the shipped incomplete run carries a transient failure and a cut-off", {
  incomplete <- shipped_examples()$example_coded_incomplete

  expect_s3_class(incomplete, "qlm_coded")
  # Saved in the current metadata layout, not upgraded on read
  expect_false(is.null(attr(incomplete, "meta")))
  expect_equal(names(incomplete)[1:4], c(".id", "sentiment", "rating", "evidence"))

  # The mix the workflow guide quotes, in its prose and in the backfill
  # transcript: one request timed out, three responses cut off. The
  # generating script accepts only a run with these counts; a regeneration
  # that changes them has to change the guide too.
  failures <- qlm_failures(incomplete)
  expect_equal(nrow(failures), 4L)
  timed_out <- !cut_off(failures$.error)
  expect_equal(sum(cut_off(failures$.error)), 3L)
  expect_equal(sum(timed_out), 1L)
  expect_s3_class(failures$.error[[which(timed_out)]], "httr2_failure")
  expect_match(failures$reason[timed_out], "Timeout was reached", fixed = TRUE)

  # A timed-out request records ellmer's condition, which carries the
  # request; the credential header must not have travelled with it
  serialised <- rawToChar(serialize(incomplete, NULL, ascii = TRUE))
  expect_false(grepl("sk-ant-", serialised, fixed = TRUE))
})

test_that("the shipped backfilled run recovered exactly the transient failures", {
  examples <- shipped_examples()
  incomplete <- examples$example_coded_incomplete
  filled <- examples$example_coded_backfilled

  before <- qlm_failures(incomplete)
  after <- qlm_failures(filled)
  transient <- before$.id[!cut_off(before$.error)]
  terminal <- before$.id[cut_off(before$.error)]

  passes <- qlm_meta(filled, "backfill", type = "object")
  expect_length(passes, 1L)
  expect_setequal(passes[[1]]$attempted, transient)
  expect_setequal(passes[[1]]$recovered, transient)
  expect_null(passes[[1]]$model)

  # What is left is what a backfill cannot fix, and only that
  expect_setequal(after$.id, terminal)
  expect_true(all(cut_off(after$.error)))

  # Everything coded the first time is untouched, in the same order
  expect_equal(filled$.id, incomplete$.id)
  untouched <- !incomplete$.id %in% before$.id
  fields <- c("sentiment", "rating", "evidence")
  expect_equal(
    as.data.frame(filled[untouched, fields]),
    as.data.frame(incomplete[untouched, fields])
  )
  expect_false(anyNA(filled$sentiment[filled$.id %in% transient]))

  expect_output(print(filled), "# Backfill: 1 pass, recovered", fixed = TRUE)
  expect_output(print(qlm_trail(filled)), "Backfill:", fixed = TRUE)
})


# File inputs (#124) -----------------------------------------------------------

test_that("qlm_backfill checks the failed units' files before uploading them again (#124)", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run <- audio_run(paths, failed = "b")
  reached <- 0L
  f <- qlm_backfill
  mockery::stub(f, "qlm_code", function(...) {
    reached <<- reached + 1L
    stop("pass reached the model", call. = FALSE)
  })

  # A change to the failed unit's file is refused before the pass
  writeBin(as.raw(99:110), paths[["b"]])
  expect_error(f(run), 'differs from the one this run coded: "b"')
  expect_equal(reached, 0L)

  # A change to a unit that is not being re-coded does not block the pass
  paths2 <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run2 <- audio_run(paths2, failed = "b")
  writeBin(as.raw(99:110), paths2[["a"]])
  expect_error(f(run2), "pass reached the model")
  expect_equal(reached, 1L)
})


test_that("a backfill keeps the pass's file hashes (#124)", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))

  # A legacy run gains hashes for exactly the units the pass re-coded
  legacy <- audio_run(paths, with_hashes = FALSE, failed = "b")
  f <- qlm_backfill
  mockery::stub(f, "qlm_code", function(x, ...) audio_run(x))
  filled <- suppressMessages(f(legacy, passes = 1L))
  table <- qlm_meta(filled, "input_files")
  expect_equal(table$.id, c("a", "b"))
  expect_equal(table$sha256, c(NA_character_, hash_file(paths[["b"]])))
  # ... which a later check reports as unverifiable for "a" and passes for "b"
  expect_message(verify_input_files(filled), 'no recorded file hash.*"a"')
})


test_that("qlm_backfill keeps an ordinal enum's ordered levels (#165)", {
  skip_if_not_installed("mockery")
  lv <- c("low", "medium", "high")
  schema <- ellmer::type_object(sev = ellmer::type_enum(lv))
  results <- data.frame(
    id = c("a", "b", "c"),
    sev = factor(c("high", NA, "low"), levels = lv, ordered = TRUE)
  )
  results$.error <- error_col(NULL, "Invalid JSON", NULL)
  run <- make_run(results, schema = schema, levels = list(sev = "ordinal"))
  expect_true(is.ordered(run$sev))

  pass <- data.frame(id = "b", sev = factor("medium", levels = lv, ordered = TRUE))
  f <- backfill_with(list(pass))
  expect_message(filled <- f(run), "Recovered 1 unit")

  expect_identical(filled$sev, factor(c("high", "medium", "low"), levels = lv, ordered = TRUE))
  expect_equal(nrow(qlm_failures(filled)), 0)
})


test_that("a backfill pass checks a video URL against the download it uploads (#179)", {
  withr::local_envvar(c(GEMINI_API_KEY = "test"))
  clip <- video_file(as.raw(1:10))
  downloaded <- video_file(as.raw(11:30))
  x <- c(clip = clip, ad = "https://example.org/ad.mp4")
  run <- media_run(x, input_type = "video", local = c(clip, downloaded), failed = "ad")
  log <- character()
  testthat::local_mocked_bindings(
    try_structured_call = function(x, ...) {
      log <<- c(log, "inference")
      list(ok = TRUE, value = data.frame(language = rep("en", length(x))))
    },
    download_input_url = function(url, dest) {
      log <<- c(log, paste0("download:", url))
      writeBin(as.raw(99:110), dest)
      invisible(dest)
    }
  )
  err <- tryCatch(
    suppressMessages(qlm_backfill(run, passes = 1L)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "failed before recovering anything")
  expect_match(err, 'differs from the one this run coded: "ad"')
  expect_equal(log, "download:https://example.org/ad.mp4")

  # The same bytes: the pass re-codes the unit and keeps the hash
  log <- character()
  testthat::local_mocked_bindings(download_input_url = function(url, dest) {
    log <<- c(log, "download")
    writeBin(as.raw(11:30), dest)
    invisible(dest)
  })
  filled <- suppressMessages(qlm_backfill(run, passes = 1L))
  expect_false(any(failed_units(filled)))
  expect_equal(log, c("download", "inference"))
  expect_equal(qlm_meta(filled, "input_files")$sha256, c(hash_file(clip), hash_file(downloaded)))
})
