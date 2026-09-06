# The structured adapter reaches into ellmer for the turns its exported
# functions convert and discard. These tests pin two things: that the
# installed ellmer still has what the adapter needs, and that the adapter
# hands ellmer's transport exactly what ellmer's own functions would, so a
# request built here is the request parallel_chat_structured() would have
# sent.

# ---- dependency contract ---------------------------------------------------

test_that("ellmer_structured_internals() finds what it needs in the installed ellmer", {
  internals <- ellmer_structured_internals()

  expect_true(is.function(internals$parallel_turns))
  expect_true(is.function(internals$type_needs_wrapper))
  expect_true(is.function(internals$wrap_type_if_needed))
  expect_true(is.function(internals$convert_from_type))
  expect_true(is.function(internals$BatchJob$new))
  # Optional, but present in every supported version
  expect_true(is.function(internals$log_turns))
})

test_that("a missing or changed ellmer internal is a compatibility error", {
  real <- ellmer_symbol
  local_mocked_bindings(ellmer_symbol = function(name) {
    if (name == "parallel_turns") NULL else real(name)
  })
  expect_error(
    ellmer_structured_internals(),
    "Missing or changed: `parallel_turns`"
  )

  local_mocked_bindings(ellmer_symbol = function(name) {
    if (name == "wrap_type_if_needed") function(type) type else real(name)
  })
  expect_error(
    ellmer_structured_internals(),
    "Missing or changed: `wrap_type_if_needed`"
  )

  local_mocked_bindings(ellmer_symbol = function(name) {
    if (name == "BatchJob") list(new = NULL) else real(name)
  })
  expect_error(ellmer_structured_internals(), "Missing or changed: `BatchJob`")
})

test_that("a missing log_turns() is tolerated", {
  real <- ellmer_symbol
  local_mocked_bindings(ellmer_symbol = function(name) {
    if (name == "log_turns") NULL else real(name)
  })
  expect_null(ellmer_structured_internals()$log_turns)
})

# ---- user turns ------------------------------------------------------------

test_that("structured_user_turn() builds the turn ellmer's as_user_turn() builds", {
  as_user_turn <- utils::getFromNamespace("as_user_turn", "ellmer")
  image <- ellmer::ContentImageInline("image/png", "aGVsbG8=")

  prompts <- list(
    "Code this text.",
    image,
    list(ellmer::ContentText("Code this image."), image)
  )
  for (prompt in prompts) {
    expect_identical(structured_user_turn(prompt), as_user_turn(prompt))
  }
  # A turn already built passes through
  turn <- ellmer::UserTurn(list(ellmer::ContentText("done")))
  expect_identical(structured_user_turn(turn), turn)
})

test_that("structured_user_turn() refuses a shape as_input_content() never produces", {
  expect_error(structured_user_turn(1:3), "must be a string")
  expect_error(structured_user_turn(list("a", 1)), "must be a string")
})

# ---- parallel: equivalence with parallel_chat_structured() -----------------

# Capture what reaches ellmer's transport from either route. The mocked
# parallel_turns() returns the staged turns, which parallel_chat_structured()
# then converts as it would a real response, so the comparison covers the
# whole of ellmer's orchestration up to the network.
capture_parallel_turns <- function(turns) {
  captured <- new.env()
  local_mocked_bindings(
    parallel_turns = function(provider, model, conversations, tools,
                              type = NULL, max_active = 10, rpm = 60,
                              on_error = "return") {
      captured$args <- list(
        provider = provider, model = model, conversations = conversations,
        tools = tools, type = type, max_active = max_active, rpm = rpm,
        on_error = on_error
      )
      turns
    },
    .package = "ellmer",
    .env = parent.frame()
  )
  captured
}

test_that("structured_chat_turns() hands parallel_turns() what parallel_chat_structured() does", {
  type <- ellmer::type_object(
    score = ellmer::type_number("Score"),
    labels = ellmer::type_array(ellmer::type_string(), "Labels")
  )
  prompts <- list("first", "second")
  turns <- list(
    json_turn(list(score = 0.5, labels = list("a"))),
    json_turn(list(score = 0.8, labels = list()))
  )
  chat <- offline_chat(
    "openai/gpt-4.1-mini",
    system_prompt = "Code each text.",
    params = ellmer::params(temperature = 0, max_tokens = 200)
  )

  captured <- capture_parallel_turns(turns)
  ellmer::parallel_chat_structured(chat, prompts, type)
  from_ellmer <- captured$args

  got <- structured_chat_turns(chat, prompts, type)
  from_adapter <- captured$args

  expect_identical(from_adapter, from_ellmer)
  expect_identical(got, turns)

  # And what was captured is a complete request description
  expect_length(from_adapter$conversations, 2L)
  expect_s3_class(from_adapter$conversations[[1]][[1]], "ellmer::SystemTurn")
  expect_s3_class(from_adapter$conversations[[1]][[2]], "ellmer::UserTurn")
  expect_identical(from_adapter$type, type)
  expect_identical(from_adapter$model, chat$get_model_object())
  expect_identical(from_adapter$provider, chat$get_provider())
})

test_that("structured_chat_turns() wraps a non-object type as ellmer does for an OpenAI-compatible endpoint", {
  type <- ellmer::type_array(ellmer::type_string(), "Labels")
  chat <- offline_chat(
    "openai_compatible/some-model",
    base_url = "https://example.invalid/v1",
    system_prompt = "Label each text."
  )
  turns <- list(json_turn(list(wrapper = list("a", "b"))))

  captured <- capture_parallel_turns(turns)
  ellmer::parallel_chat_structured(chat, list("one"), type)
  from_ellmer <- captured$args

  structured_chat_turns(chat, list("one"), type)
  from_adapter <- captured$args

  expect_identical(from_adapter, from_ellmer)
  expect_s3_class(from_adapter$type, "ellmer::TypeObject")
  expect_named(from_adapter$type@properties, "wrapper")
  expect_true(structured_needs_wrapper(type, chat$get_provider()))
  # An object root is never wrapped, on any provider
  expect_false(structured_needs_wrapper(
    ellmer::type_object(x = ellmer::type_string()), chat$get_provider()
  ))
})

test_that("structured_chat_turns() matches parallel_chat_structured() on a forced-tool provider with content prompts", {
  type <- ellmer::type_object(score = ellmer::type_number("Score"))
  chat <- offline_chat("anthropic/claude-sonnet-5", system_prompt = "Score it.")
  image <- ellmer::ContentImageInline("image/png", "aGVsbG8=")
  prompts <- list(
    list(ellmer::ContentText("Score this image."), image),
    image
  )
  turns <- list(json_turn(list(score = 1)), json_turn(list(score = 0)))

  captured <- capture_parallel_turns(turns)
  ellmer::parallel_chat_structured(chat, prompts, type)
  from_ellmer <- captured$args

  structured_chat_turns(chat, prompts, type)
  expect_identical(captured$args, from_ellmer)
})

test_that("structured_chat_turns() uses parallel_chat_structured()'s defaults and passes explicit settings through", {
  type <- ellmer::type_object(score = ellmer::type_number("Score"))
  chat <- offline_chat()
  captured <- capture_parallel_turns(list(json_turn(list(score = 1))))

  structured_chat_turns(chat, list("one"), type)
  expect_identical(captured$args$max_active, 10)
  expect_identical(captured$args$rpm, 500)
  expect_identical(captured$args$on_error, "return")

  structured_chat_turns(
    chat, list("one"), type,
    execution_args = list(max_active = 2, rpm = 30, on_error = "continue",
                          include_tokens = TRUE, include_cost = TRUE,
                          convert = TRUE)
  )
  expect_identical(captured$args$max_active, 2)
  expect_identical(captured$args$rpm, 30)
  expect_identical(captured$args$on_error, "continue")
  expect_false(any(c("include_tokens", "include_cost", "convert") %in%
                     names(captured$args)))
})

test_that("structured_chat_turns() keeps turns, errors and absent results at their positions", {
  type <- ellmer::type_object(score = ellmer::type_number("Score"))
  chat <- offline_chat()
  staged <- list(
    json_turn(list(score = 0.5)),
    request_error("HTTP 429 Too Many Requests.", 429L),
    NULL,
    text_turn("I cannot score this.", finish_reason = "content_filter"),
    json_turn(list(score = 0.9), tokens = c(20, 200, 0), finish_reason = "max_tokens")
  )
  capture_parallel_turns(staged)

  got <- structured_chat_turns(chat, as.list(letters[1:5]), type)
  expect_length(got, 5L)
  expect_identical(got, staged)
  expect_identical(got[[5]]@finish_reason, "max_tokens")
  expect_identical(got[[5]]@tokens, c(20, 200, 0))
})

test_that("structured_chat_turns() logs the parallel turns once, and not at all without log_turns()", {
  type <- ellmer::type_object(score = ellmer::type_number("Score"))
  chat <- offline_chat()
  turns <- list(json_turn(list(score = 0.5)))
  capture_parallel_turns(turns)

  logged <- 0L
  local_mocked_bindings(
    log_turns = function(provider, model, turns) logged <<- logged + 1L,
    .package = "ellmer"
  )
  structured_chat_turns(chat, list("one"), type)
  expect_identical(logged, 1L)

  real <- ellmer_symbol
  local_mocked_bindings(ellmer_symbol = function(name) {
    if (name == "log_turns") NULL else real(name)
  })
  expect_identical(structured_chat_turns(chat, list("one"), type), turns)
  expect_identical(logged, 1L)
})

# ---- batch: equivalence with batch_chat_structured() -----------------------

# A stand-in for ellmer's BatchJob that records how it was constructed and
# answers with staged turns, or reports the job as still pending.
fake_batch_job <- function(turns, pending = FALSE) {
  captured <- new.env()
  generator <- list(new = function(chat, prompts, path, type = NULL,
                                   wait = TRUE, ignore_hash = FALSE, ...) {
    captured$args <- list(prompts = prompts, path = path, type = type,
                          wait = wait, ignore_hash = ignore_hash)
    job <- list(
      step_until_done = function() if (pending) NULL else job,
      result_turns = function() turns
    )
    job
  })
  local_mocked_bindings(BatchJob = generator, .package = "ellmer",
                        .env = parent.frame())
  captured
}

test_that("structured_chat_turns(batch = TRUE) builds the job batch_chat_structured() builds", {
  type <- ellmer::type_object(score = ellmer::type_number("Score"))
  chat <- offline_chat("openai/gpt-4.1-mini", system_prompt = "Score it.")
  prompts <- list("one", "two")
  turns <- list(json_turn(list(score = 0.1)), json_turn(list(score = 0.2)))
  path <- withr::local_tempfile(fileext = ".json")

  captured <- fake_batch_job(turns)
  ellmer::batch_chat_structured(chat, prompts, path = path, type = type)
  from_ellmer <- captured$args

  got <- structured_chat_turns(
    chat, prompts, type, batch = TRUE,
    execution_args = list(path = path)
  )
  expect_identical(captured$args, from_ellmer)
  expect_identical(got, turns)
  expect_identical(captured$args$wait, TRUE)
  expect_identical(captured$args$ignore_hash, FALSE)

  structured_chat_turns(
    chat, prompts, type, batch = TRUE,
    execution_args = list(path = path, wait = FALSE, ignore_hash = TRUE)
  )
  expect_identical(captured$args$wait, FALSE)
  expect_identical(captured$args$ignore_hash, TRUE)
})

test_that("structured_chat_turns(batch = TRUE) wraps the wire type but reports the original", {
  type <- ellmer::type_array(ellmer::type_string(), "Labels")
  chat <- offline_chat(
    "openai_compatible/some-model",
    base_url = "https://example.invalid/v1"
  )
  path <- withr::local_tempfile(fileext = ".json")
  captured <- fake_batch_job(list(json_turn(list(wrapper = list("a")))))

  ellmer::batch_chat_structured(chat, list("one"), path = path, type = type)
  from_ellmer <- captured$args
  structured_chat_turns(chat, list("one"), type, batch = TRUE,
                        execution_args = list(path = path))
  expect_identical(captured$args, from_ellmer)
  expect_named(captured$args$type@properties, "wrapper")
})

test_that("structured_chat_turns(batch = TRUE) returns NULL for a job still pending", {
  type <- ellmer::type_object(score = ellmer::type_number("Score"))
  chat <- offline_chat()
  path <- withr::local_tempfile(fileext = ".json")
  fake_batch_job(list(json_turn(list(score = 0.1))), pending = TRUE)

  expect_null(structured_chat_turns(
    chat, list("one"), type, batch = TRUE,
    execution_args = list(path = path, wait = FALSE)
  ))
})

# ---- fixtures ----------------------------------------------------------------

test_that("json_turn() keeps an empty object and an empty array distinct", {
  from_string <- json_turn(string = '{"labels": [], "extra": {}}')
  parsed <- from_string@contents[[1]]@parsed
  expect_identical(parsed$labels, list())
  expect_null(names(parsed$labels))
  expect_identical(names(parsed$extra), character())

  from_data <- json_turn(list(labels = list(), extra = structure(list(), names = character())))
  expect_identical(from_data@contents[[1]]@parsed, parsed)
})
