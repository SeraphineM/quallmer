# Fixtures for qlm_transcribe() (#178), shared by the transcribe, backend,
# code, replicate, backfill and trail tests

# A captured response body from the OpenAI transcription endpoint
transcription_fixture <- function(name) {
  jsonlite::fromJSON(
    testthat::test_path("fixtures", paste0("transcription_", name, ".json")),
    simplifyVector = FALSE
  )
}

# The OpenAI backend with the network stood in for by `respond`, a function
# of the httr2 request returning an httr2 response; every request is kept in
# `seen$reqs`. The key comes from the environment, as in use.
mock_transcription_endpoint <- function(respond = fixture_responder(), seen = new.env(),
                                        env = parent.frame()) {
  withr::local_envvar(c(OPENAI_API_KEY = "test-key"), .local_envir = env)
  seen$reqs <- list()
  httr2::local_mocked_responses(function(req) {
    seen$reqs[[length(seen$reqs) + 1L]] <- req
    respond(req)
  }, env = env)
  seen
}

# Answers every request with the gpt-4o-mini-transcribe fixture for the
# American clip, except the paths in `fail`, which get the captured 404
# error body under `status`, and the paths in `empty`, which get a body
# with no transcript, and the paths in `blank`, which get whitespace for one.
fixture_responder <- function(fail = character(), status = 500L, empty = character(),
                              blank = character()) {
  function(req) {
    path <- req$body$data$file$path
    if (basename(path) %in% basename(fail)) {
      return(httr2::response_json(status_code = status, body = transcription_fixture("error")))
    }
    if (basename(path) %in% basename(empty)) {
      return(httr2::response_json(body = list(usage = list(type = "duration", seconds = 3))))
    }
    if (basename(path) %in% basename(blank)) {
      return(httr2::response_json(body = list(text = "  \n ", usage = list(type = "duration", seconds = 2))))
    }
    httr2::response_json(body = transcription_fixture("gpt_4o_mini_transcribe_us"))
  }
}

# A transcript built directly, as qlm_transcribe() would leave it: one row
# per id, `failed` ids with NA text and a reason
fake_transcript <- function(ids, text = paste("transcript of", ids), failed = character(),
                            model = "openai/gpt-4o-mini-transcribe") {
  n <- length(ids)
  is_failed <- ids %in% failed
  text[is_failed] <- NA_character_
  provenance <- data.frame(
    .id = ids,
    status = ifelse(is_failed, "failed", "ok"),
    source = paste0(ids, ".wav"),
    .error = ifelse(is_failed, "HTTP 500 Internal Server Error.", NA_character_),
    size = rep(1000, n),
    sha256 = paste0("hash-", ids),
    model = rep(model, n),
    language = NA_character_,
    prompt = NA_character_,
    base_url = NA_character_,
    timestamp = rep(Sys.time(), n),
    stringsAsFactors = FALSE
  )
  provenance$usage <- lapply(seq_len(n), function(i) {
    if (is_failed[i]) NULL else list(type = "duration", seconds = 10)
  })
  new_qlm_transcript(text, provenance)
}

# qlm_code() over text with the structured call stubbed: `results` is a
# table for rows_as_turns(), or a function of the prompts. The prompts of
# each call are kept in `calls$prompts`.
text_runner <- function(results, calls = new.env(), env = parent.frame()) {
  withr::local_envvar(c(OPENAI_API_KEY = "test"), .local_envir = env)
  calls$n <- 0L
  tsc <- try_structured_call
  mockery::stub(tsc, "structured_chat_turns", function(chat, prompts, type, batch = FALSE,
                                                    execution_args = list()) {
    calls$n <- calls$n + 1L
    calls$prompts <- prompts
    out <- if (is.function(results)) results(prompts) else results
    if (is.data.frame(out)) rows_as_turns(out) else out
  })
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  f
}

score_codebook <- function() {
  qlm_codebook("Scores", "Score the text.",
               ellmer::type_object(score = ellmer::type_number("Score")))
}

# The Authorization header of a request, which httr2 stores redacted
bearer_of <- function(req) {
  headers <- httr2::req_get_headers(req, "reveal")
  unname(headers[["Authorization"]])
}
