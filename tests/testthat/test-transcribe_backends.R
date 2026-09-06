test_that("openai_transcription_request builds the multipart request", {
  wav <- audio_file()
  req <- openai_transcription_request(
    wav, model_id = "whisper-1", language = NULL, prompt = "names",
    api_key = "k", base_url = "https://api.openai.com/v1", rpm = 45
  )
  expect_equal(req$url, "https://api.openai.com/v1/audio/transcriptions")
  expect_equal(bearer_of(req), "Bearer k")
  fields <- req$body$data
  expect_equal(names(fields), c("file", "model", "response_format", "prompt"))
  expect_s3_class(fields$file, "form_file")
  expect_equal(fields$response_format, "json")
  expect_equal(req$policies$retry_max_tries, 3)
  expect_true("throttle_realm" %in% names(req$policies))
})


test_that("openai_error_body reads the provider's sentence, and nothing else", {
  resp <- httr2::response_json(status_code = 404, body = transcription_fixture("error"))
  expect_equal(openai_error_body(resp), "The model `no-such-model` does not exist or you do not have access to it.")
  expect_null(openai_error_body(httr2::response(status_code = 500, body = charToRaw("oops"))))
  expect_null(openai_error_body(httr2::response_json(status_code = 500, body = list(error = list(code = 1)))))
})


test_that("parse_openai_transcription keeps both usage shapes as they came", {
  tokens <- parse_openai_transcription(httr2::response_json(body = transcription_fixture("gpt_4o_mini_transcribe_cn")))
  expect_match(tokens$text, "地铁站")
  expect_true(is.na(tokens$error))
  expect_equal(tokens$usage$type, "tokens")
  expect_equal(tokens$usage$input_token_details, list(text_tokens = 0L, audio_tokens = 199L))

  seconds <- parse_openai_transcription(httr2::response_json(body = transcription_fixture("whisper_1_cn")))
  expect_equal(seconds$usage, list(type = "duration", seconds = 20L))

  none <- parse_openai_transcription(httr2::response_json(body = list(usage = list(type = "duration", seconds = 1))))
  expect_true(is.na(none$text))
  expect_match(none$error, "no transcript text")
  expect_equal(none$usage$seconds, 1)

  bare <- parse_openai_transcription(httr2::response_json(body = list(text = "words")))
  expect_null(bare$usage)
})


test_that("the Gemini instruction is fixed, with the caller's hints appended", {
  base <- transcription_instruction()
  expect_match(base, "^You are a transcription engine")
  expect_match(base, "no translation")
  expect_false(grepl("ISO 639-1", base))
  hinted <- transcription_instruction(language = "de", prompt = "Bundestag debate")
  expect_match(hinted, "ISO 639-1 code \"de\"")
  expect_match(hinted, "Context from the caller: Bundestag debate$")
})


test_that("gemini_transcription_outcome reads the last turn and ellmer's usage", {
  turn <- text_turn("  the words  ", tokens = c(300, 20, 0))
  chat <- list(last_turn = function() turn, get_cost = function() 0.01)
  out <- gemini_transcription_outcome(chat)
  expect_equal(out$text, "the words")
  expect_true(is.na(out$error))
  expect_equal(out$usage$cost, 0.01)
  expect_equal(out$usage$note, audio_cost_note())

  chat$get_cost <- function() stop("no prices")
  expect_true(is.na(gemini_transcription_outcome(chat)$usage$cost))
})


test_that("the Gemini setup builds the chat offline and runs the gate", {
  withr::local_envvar(c(GEMINI_API_KEY = "test"))
  setup <- gemini_transcription_setup("google_gemini/gemini-2.5-flash", language = "en",
                                      prompt = NULL, api_key = NULL, base_url = NULL)
  expect_s3_class(setup$chat, "Chat")
  expect_match(setup$chat$get_system_prompt(), "ISO 639-1 code \"en\"")
  expect_null(setup$registered)
  expect_null(setup$recorded_base_url)
  expect_equal(setup$ellmer_version, as.character(utils::packageVersion("ellmer")))

  expect_error(
    gemini_transcription_setup("google_gemini/gemini-2.5-flash-tts", NULL, NULL, NULL, NULL),
    "not recognised as accepting audio"
  )
  # An endpoint is recorded redacted
  with_url <- gemini_transcription_setup("google_gemini/gemini-2.5-flash", NULL, NULL,
                                         api_key = "k", base_url = "https://u:p@g.example.org/v1")
  expect_equal(with_url$recorded_base_url, "https://g.example.org/v1")
})


test_that("the OpenAI setup takes the key from the argument or the environment", {
  withr::local_envvar(c(OPENAI_API_KEY = "from-env"))
  expect_equal(openai_transcription_setup(NULL, NULL)$api_key, "from-env")
  expect_equal(openai_transcription_setup("given", NULL)$api_key, "given")
  expect_equal(openai_transcription_setup(NULL, NULL)$base_url, "https://api.openai.com/v1")
  setup <- openai_transcription_setup(NULL, "https://k:s@h.example.org/v1")
  expect_equal(setup$base_url, "https://k:s@h.example.org/v1")
  expect_equal(setup$recorded_base_url, "https://h.example.org/v1")
  withr::local_envvar(c(OPENAI_API_KEY = ""))
  expect_error(openai_transcription_setup(NULL, NULL), "OPENAI_API_KEY")
})
