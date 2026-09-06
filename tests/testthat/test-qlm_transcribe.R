# Arguments -------------------------------------------------------------------

test_that("qlm_transcribe checks its arguments before anything is sent", {
  seen <- mock_transcription_endpoint()
  wav <- audio_file()

  expect_error(qlm_transcribe(123), "must be a character vector")
  expect_error(qlm_transcribe(character()), "At least one file")
  expect_error(qlm_transcribe(c(wav, NA)), "must not contain.*NA")
  expect_error(qlm_transcribe(wav, model = c("a", "b")), "must be a single string")
  expect_error(qlm_transcribe(wav, language = 1), "language.*single string")
  expect_error(qlm_transcribe(wav, prompt = c("a", "b")), "prompt.*single string")
  expect_error(qlm_transcribe(wav, base_url = NA_character_), "base_url.*single string")
  expect_error(qlm_transcribe(wav, max_active = 0), "max_active.*at least 1")
  expect_error(qlm_transcribe(wav, rpm = 2.5), "rpm.*at least 1")
  expect_error(qlm_transcribe(wav, on_error = "ignore"), "on_error")
  expect_error(qlm_transcribe(wav, temperature = 0), "must be empty")
  expect_error(qlm_transcribe(wav, model = "nosuch/model"), "nosuch")
  expect_length(seen$reqs, 0)
})


test_that("qlm_transcribe refuses files the backend could not take, before any request", {
  seen <- mock_transcription_endpoint()
  wav <- audio_file()
  txt <- audio_file(ext = "txt")
  aac <- audio_file(ext = "aac")

  expect_error(qlm_transcribe(c(wav, "/nowhere/missing.wav")), "1 audio file does not exist.*missing.wav")
  expect_error(qlm_transcribe(c(wav, txt)), "format this route does not accept.*\\.txt")
  # The lists differ by backend: aac is a Gemini format, not an OpenAI one
  expect_error(qlm_transcribe(aac), "does not accept.*\\.aac")
  expect_equal(transcription_extensions("chat"), audio_extensions())
  expect_true("webm" %in% transcription_extensions("endpoint"))

  testthat::local_mocked_bindings(transcription_size_limit = function(backend) 10)
  expect_error(qlm_transcribe(wav), "over the 0 MB limit")
  expect_length(seen$reqs, 0)
})


test_that("qlm_transcribe needs a key before it sends anything", {
  withr::local_envvar(c(OPENAI_API_KEY = ""))
  wav <- audio_file()
  expect_error(qlm_transcribe(wav), "OPENAI_API_KEY.*or pass.*api_key")
})


# Names ----------------------------------------------------------------------

test_that("names are settled from the input as given", {
  seen <- mock_transcription_endpoint()
  a <- audio_file()
  b <- audio_file()

  # Unnamed files take their basenames; supplied names are kept exactly
  out <- suppressWarnings(qlm_transcribe(c(a, b)))
  expect_equal(names(out), basename(c(a, b)))
  out <- suppressWarnings(qlm_transcribe(c(first = a, b)))
  expect_equal(names(out), c("first", basename(b)))
  out <- suppressWarnings(qlm_transcribe(c(first = a, second = b)))
  expect_equal(names(out), c("first", "second"))
  expect_equal(attr(out, "provenance")$.id, c("first", "second"))

  # An unnamed URL is text<position>, the position in the whole input
  testthat::local_mocked_bindings(download_audio = function(url, dest) file.copy(a, dest))
  out <- suppressWarnings(qlm_transcribe(c(a, "https://example.org/x.wav", named = b, "https://example.org/y.wav")))
  expect_equal(names(out), c(basename(a), "text2", "named", "text4"))
  expect_equal(attr(out, "provenance")$source, c(basename(a), "https://example.org/x.wav", basename(b), "https://example.org/y.wav"))
})


test_that("resolved names must be unique and non-empty", {
  seen <- mock_transcription_endpoint()
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  same1 <- file.path(dir1, "clip.wav")
  same2 <- file.path(dir2, "clip.wav")
  writeBin(as.raw(1:8), same1)
  writeBin(as.raw(1:8), same2)

  expect_error(qlm_transcribe(c(same1, same2)), "must be unique.*clip.wav.*share one need names")
  expect_error(qlm_transcribe(c(a = same1, a = same2)), "must be unique.*become the.*\\.id")
  expect_error(qlm_transcribe(stats::setNames(c(same1, same2), c("a", NA))), "must not be.*NA")
  expect_length(seen$reqs, 0)
})


# The OpenAI backend ----------------------------------------------------------

test_that("qlm_transcribe sends one multipart request per file and records what came back", {
  seen <- mock_transcription_endpoint()
  a <- audio_file(as.raw(1:64))
  b <- audio_file(as.raw(65:128))

  out <- qlm_transcribe(c(a, b), language = "en", prompt = "Harvard sentences")
  expect_s3_class(out, "qlm_transcript")
  expect_true(is.character(out))
  expect_length(out, 2)
  expect_match(out[[1]], "^The birch canoe")

  expect_length(seen$reqs, 2)
  req <- seen$reqs[[1]]
  expect_equal(req$url, "https://api.openai.com/v1/audio/transcriptions")
  expect_equal(bearer_of(req), "Bearer test-key")
  fields <- req$body$data
  expect_equal(names(fields), c("file", "model", "response_format", "language", "prompt"))
  expect_equal(fields$model, "gpt-4o-mini-transcribe")
  expect_equal(fields$language, "en")
  expect_equal(fields$prompt, "Harvard sentences")
  expect_equal(normalizePath(fields$file$path), normalizePath(a))
  expect_equal(req$policies$retry_max_tries, 3)
  expect_true("throttle_realm" %in% names(req$policies))

  prov <- attr(out, "provenance")
  expect_equal(prov$.id, basename(c(a, b)))
  expect_equal(prov$status, c("ok", "ok"))
  expect_equal(prov$source, basename(c(a, b)))
  expect_true(all(is.na(prov$.error)))
  expect_equal(prov$size, c(64, 64))
  expect_equal(prov$sha256, c(hash_file(a), hash_file(b)))
  expect_equal(prov$model, rep("openai/gpt-4o-mini-transcribe", 2))
  expect_equal(prov$language, c("en", "en"))
  expect_equal(prov$prompt, rep("Harvard sentences", 2))
  # The host is recorded even when it is the provider's own default
  expect_equal(prov$base_url, rep("https://api.openai.com/v1", 2))
  expect_equal(names(prov), c(".id", "status", "source", ".error", "size", "sha256", "model",
                              "language", "prompt", "base_url", "timestamp", "usage"))
  expect_s3_class(prov$timestamp, "POSIXct")
  expect_false(any(is.na(prov$timestamp)))
  expect_equal(prov$usage[[1]]$type, "tokens")
  expect_equal(prov$usage[[1]]$input_token_details$audio_tokens, 336)
})


test_that("optional fields are left out, and the endpoint and model follow the arguments", {
  seen <- mock_transcription_endpoint()
  wav <- audio_file()

  out <- qlm_transcribe(wav, model = "openai/whisper-1",
                        base_url = "https://user:secret@proxy.example.org/v1?api_key=abc&v=2",
                        api_key = "given-key")
  req <- seen$reqs[[1]]
  expect_equal(names(req$body$data), c("file", "model", "response_format"))
  expect_equal(req$body$data$model, "whisper-1")
  expect_equal(req$url, "https://user:secret@proxy.example.org/v1/audio/transcriptions?api_key=abc&v=2")
  expect_equal(bearer_of(req), "Bearer given-key")

  # What is recorded carries no credential
  prov <- attr(out, "provenance")
  expect_equal(prov$base_url, "https://proxy.example.org/v1?api_key=<redacted>&v=2")
  flat <- unlist(lapply(prov, function(col) if (is.list(col)) unlist(col) else col))
  expect_false(any(grepl("given-key", flat, fixed = TRUE)))
  expect_false(any(grepl("secret", flat, fixed = TRUE)))
  expect_false(any(grepl("abc", flat, fixed = TRUE)))
})


test_that("the pool is given the concurrency asked for", {
  seen <- mock_transcription_endpoint()
  wav <- audio_file()
  given <- NULL
  testthat::local_mocked_bindings(perform_transcription_requests = function(reqs, max_active, on_error) {
    given <<- list(n = length(reqs), max_active = max_active, on_error = on_error)
    lapply(reqs, function(r) httr2::response_json(body = transcription_fixture("whisper_1_us")))
  })
  out <- qlm_transcribe(c(wav, audio_file()), max_active = 3, on_error = "return")
  expect_equal(given, list(n = 2L, max_active = 3, on_error = "return"))
  expect_equal(attr(out, "provenance")$usage[[1]], list(type = "duration", seconds = 34L))
})


# Failures -------------------------------------------------------------------

test_that("a failed transcription stays in place as NA with its reason", {
  a <- audio_file(as.raw(1:8))
  b <- audio_file(as.raw(9:16))
  c_ <- audio_file(as.raw(17:24))
  seen <- mock_transcription_endpoint(fixture_responder(fail = b))

  expect_warning(out <- qlm_transcribe(c(a, b, c_)), "1 of 3 transcriptions failed.*does not exist")
  expect_length(out, 3)
  expect_equal(names(out), basename(c(a, b, c_)))
  expect_equal(is.na(out), c(FALSE, TRUE, FALSE), ignore_attr = TRUE)
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "ok"))
  expect_match(prov$.error[2], "HTTP 500.*no-such-model.*does not exist")
  expect_true(is.na(prov$.error[1]))
  # The failed unit was still hashed: its bytes were sent
  expect_equal(prov$sha256[2], hash_file(b))
  expect_length(seen$reqs, 3)
  expect_equal(which(is.na(out)), c(2L), ignore_attr = TRUE)
})


test_that("the bytes recorded are the ones sent, whatever happens to the file after", {
  a <- audio_file(as.raw(1:16))
  b <- audio_file(as.raw(17:32))
  original <- c(hash_file(a), hash_file(b))
  seen <- mock_transcription_endpoint(function(req) {
    # While the requests run, one file is replaced and the other deleted
    writeBin(as.raw(99:120), a)
    unlink(b)
    httr2::response_json(body = transcription_fixture("whisper_1_us"))
  })
  out <- qlm_transcribe(c(a, b))
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "ok"))
  expect_equal(prov$sha256, original)
  expect_equal(prov$size, c(16, 16))
})


test_that("a response without a transcript is a failure of that unit, under every policy", {
  a <- audio_file(as.raw(1:8))
  b <- audio_file(as.raw(9:16))
  c_ <- audio_file(as.raw(17:24))
  seen <- mock_transcription_endpoint(fixture_responder(empty = b))
  expect_warning(out <- qlm_transcribe(c(a, b, c_)), "carried no transcript text")
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "ok"))
  expect_equal(prov$.error[2], "the response carried no transcript text")
  # The usage it did report is kept
  expect_equal(prov$usage[[2]], list(type = "duration", seconds = 3L))

  # The empty answer is an error of the request, so the pool's policy
  # applies to it as to a refusal
  seen <- mock_transcription_endpoint(fixture_responder(empty = b))
  expect_warning(out <- qlm_transcribe(c(a, b, c_), on_error = "return"), "not submitted")
  expect_equal(attr(out, "provenance")$status, c("ok", "failed", "unsubmitted"))
  expect_length(seen$reqs, 2)
  expect_error(qlm_transcribe(c(a, b, c_), on_error = "stop"), "no transcript text")

  # A blank transcript is no transcript either, under every policy
  seen <- mock_transcription_endpoint(fixture_responder(blank = b))
  expect_warning(out <- qlm_transcribe(c(a, b, c_)), "carried no transcript text")
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "ok"))
  expect_true(is.na(out[[2]]))
  expect_equal(prov$usage[[2]], list(type = "duration", seconds = 2L))
  seen <- mock_transcription_endpoint(fixture_responder(blank = b))
  expect_warning(out <- qlm_transcribe(c(a, b, c_), on_error = "return"), "not submitted")
  expect_equal(attr(out, "provenance")$status, c("ok", "failed", "unsubmitted"))
  expect_error(qlm_transcribe(c(a, b, c_), on_error = "stop"), "no transcript text")
})


test_that("every transcription can fail and the shape holds", {
  a <- audio_file(as.raw(1:8))
  b <- audio_file(as.raw(9:16))
  seen <- mock_transcription_endpoint(fixture_responder(fail = c(a, b)))
  expect_warning(out <- qlm_transcribe(c(a, b)), "2 of 2 transcriptions failed")
  expect_true(all(is.na(out)))
  expect_equal(names(out), basename(c(a, b)))
  expect_equal(attr(out, "provenance")$status, c("failed", "failed"))
})


test_that("on_error = 'return' stops submitting after a failure, and 'stop' raises it", {
  a <- audio_file(as.raw(1:8))
  b <- audio_file(as.raw(9:16))
  c_ <- audio_file(as.raw(17:24))
  seen <- mock_transcription_endpoint(fixture_responder(fail = b))

  expect_warning(out <- qlm_transcribe(c(a, b, c_), on_error = "return"), "1 file was not submitted")
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "unsubmitted"))
  expect_match(prov$.error[3], "not submitted")
  expect_true(is.na(prov$timestamp[3]))
  expect_true(is.na(out[[3]]))
  expect_length(seen$reqs, 2)

  expect_error(qlm_transcribe(c(a, b, c_), on_error = "stop"), "no-such-model")
})


# URLs -----------------------------------------------------------------------

test_that("a URL is downloaded, hashed, recorded redacted, and the download removed", {
  seen <- mock_transcription_endpoint()
  local <- audio_file(as.raw(1:32))
  dests <- character()
  testthat::local_mocked_bindings(download_audio = function(url, dest) {
    dests <<- c(dests, dest)
    file.copy(local, dest)
  })

  url <- "https://user:pw@files.example.org/clips/OSR_us_000_0010_8k.wav?token=abc"
  out <- qlm_transcribe(c(harvard = url))
  prov <- attr(out, "provenance")
  expect_equal(prov$.id, "harvard")
  expect_equal(prov$status, "ok")
  expect_equal(prov$source, "https://files.example.org/clips/OSR_us_000_0010_8k.wav?token=<redacted>")
  expect_equal(prov$sha256, hash_file(local))
  expect_equal(prov$size, 32)
  # The request carried the download, under a name that keeps the extension
  sent <- seen$reqs[[1]]$body$data$file$path
  expect_equal(basename(sent), basename(dests[[1]]))
  expect_match(basename(sent), "^001_OSR_us_000_0010_8k\\.wav$")
  expect_match(dirname(sent), "quallmer_transcribe_")
  expect_false(file.exists(dests[[1]]))
  expect_false(dir.exists(dirname(dests[[1]])))
})


test_that("a URL is refused before download when its format cannot be read", {
  seen <- mock_transcription_endpoint()
  called <- FALSE
  testthat::local_mocked_bindings(download_audio = function(url, dest) called <<- TRUE)
  expect_error(qlm_transcribe("https://example.org/stream?id=1"), "format this route does not accept")
  expect_false(called)
  expect_length(seen$reqs, 0)
})


test_that("a failed download follows on_error", {
  local <- audio_file(as.raw(1:32))
  seen <- mock_transcription_endpoint()
  testthat::local_mocked_bindings(download_audio = function(url, dest) {
    if (grepl("bad", url)) stop("Could not resolve host: bad.example.org")
    file.copy(local, dest)
  })
  urls <- c(good = "https://example.org/a.wav", bad = "https://bad.example.org/b.wav",
            after = "https://example.org/c.wav")

  expect_warning(out <- qlm_transcribe(urls), "download failed.*Could not resolve host")
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "ok"))
  expect_true(is.na(prov$sha256[2]))
  expect_length(seen$reqs, 2)

  seen <- mock_transcription_endpoint()
  expect_warning(out <- qlm_transcribe(urls, on_error = "return"), "not submitted")
  expect_equal(attr(out, "provenance")$status, c("ok", "failed", "unsubmitted"))
  expect_length(seen$reqs, 1)

  expect_error(qlm_transcribe(urls, on_error = "stop"), "Downloading.*bad.example.org.*failed")

  # An oversized download is a failure of that unit, too
  testthat::local_mocked_bindings(transcription_size_limit = function(backend) 10)
  expect_warning(out <- qlm_transcribe(urls[1]), "over the 0 MB limit")
  expect_equal(attr(out, "provenance")$status, "failed")
})


# The object ------------------------------------------------------------------

test_that("subsetting keeps each transcript with its provenance", {
  tr <- fake_transcript(c("a", "b", "c"))
  prov <- function(x) attr(x, "provenance")

  expect_equal(names(tr[3:1]), c("c", "b", "a"))
  expect_equal(prov(tr[3:1])$.id, c("c", "b", "a"))
  expect_equal(prov(tr[3:1])$source, c("c.wav", "b.wav", "a.wav"))
  expect_equal(prov(tr[c("b", "c")])$.id, c("b", "c"))
  expect_equal(prov(tr[c(TRUE, FALSE, TRUE)])$.id, c("a", "c"))
  expect_equal(prov(tr[-2])$.id, c("a", "c"))
  expect_s3_class(tr[2], "qlm_transcript")
  expect_identical(tr[], tr)
  expect_equal(names(rev(tr)), c("c", "b", "a"))
  expect_equal(prov(utils::head(tr, 2))$.id, c("a", "b"))
  # A single element is plain text
  expect_equal(tr[["b"]], "transcript of b")
  # A repeated identifier is refused, as for a coded object
  expect_error(tr[c(1, 1)], "must be unique")
})


test_that("assigning a transcript replaces the provenance rows with it", {
  tr <- fake_transcript(c("a", "b", "c"), failed = "b")
  retry <- fake_transcript("b_again", text = "second try", model = "openai/whisper-1")
  before <- attr(tr, "provenance")
  tr[is.na(tr)] <- retry
  prov <- attr(tr, "provenance")
  expect_equal(names(tr), c("a", "b", "c"))
  expect_equal(unname(tr[["b"]]), "second try")
  expect_equal(prov$.id, c("a", "b", "c"))
  expect_equal(prov$status, c("ok", "ok", "ok"))
  expect_equal(prov$model, c("openai/gpt-4o-mini-transcribe", "openai/whisper-1", "openai/gpt-4o-mini-transcribe"))
  expect_true(is.na(prov$.error[2]))
  expect_equal(prov$usage[[2]], list(type = "duration", seconds = 10))
  expect_equal(prov$source[2], "b_again.wav")
  expect_identical(prov[c(1, 3), ], before[c(1, 3), ])
  # By name and by position alike, one row per position
  tr[c("a", "c")] <- fake_transcript(c("x", "y"))
  expect_equal(attr(tr, "provenance")$source, c("x.wav", "b_again.wav", "y.wav"))
  expect_error(tr[1:3] <- fake_transcript(c("p", "q")), "one element, or one per position")
})


test_that("assigning plain text records an edit, and NA a failure", {
  tr <- fake_transcript(c("a", "b"))
  tr[2] <- "corrected"
  expect_s3_class(tr, "qlm_transcript")
  expect_equal(unname(tr[[2]]), "corrected")
  prov <- attr(tr, "provenance")
  expect_equal(prov$.id, c("a", "b"))
  expect_equal(prov$status, c("ok", "edited"))
  expect_null(prov$usage[[2]])
  expect_equal(prov$sha256[2], "hash-b")
  expect_equal(prov$model[2], "openai/gpt-4o-mini-transcribe")
  tr["a"] <- NA
  prov <- attr(tr, "provenance")
  expect_equal(prov$status[1], "failed")
  expect_equal(prov$.error[1], "set to NA by assignment")

  # [[<- follows the same rules, one element at a time
  tr[["a"]] <- "by hand"
  expect_equal(unname(tr[["a"]]), "by hand")
  expect_equal(attr(tr, "provenance")$status[1], "edited")
  tr[[2]] <- fake_transcript("b_retry", text = "retried", model = "openai/whisper-1")
  expect_equal(attr(tr, "provenance")$model[2], "openai/whisper-1")
  expect_equal(attr(tr, "provenance")$status[2], "ok")
  expect_equal(names(tr), c("a", "b"))
  expect_error(tr[[3]] <- "more", "cannot be extended")
  expect_error(tr[[1:2]] <- "x", "replaces one transcript")
  expect_error(tr[[1]] <- c("x", "y"), "replaces one transcript")
  expect_error(tr[3] <- "more", "cannot be extended")
  expect_error(tr["zzz"] <- "more", "cannot be extended")

  names(tr) <- c("x", "y")
  expect_equal(names(tr), c("x", "y"))
  expect_equal(attr(tr, "provenance")$.id, c("x", "y"))
  expect_error(names(tr) <- c("x", "x"), "must be unique")
  expect_error(names(tr) <- "only", "one name per transcript")
  expect_error(names(tr) <- NULL, "one name per transcript")
})


test_that("c() joins independent runs and refuses a collision or plain text", {
  one <- fake_transcript(c("a", "b"))
  two <- fake_transcript("c", model = "openai/whisper-1")
  both <- c(one, two)
  expect_s3_class(both, "qlm_transcript")
  expect_equal(names(both), c("a", "b", "c"))
  prov <- attr(both, "provenance")
  expect_equal(nrow(prov), 3)
  expect_equal(prov$model, c(rep("openai/gpt-4o-mini-transcribe", 2), "openai/whisper-1"))
  expect_equal(prov$usage[[3]]$seconds, 10)
  expect_error(c(one, one), "must be unique")
  expect_error(c(one, "plain"), "argument 2 is.*character")
})


test_that("as.character() drops the provenance and keeps the names", {
  tr <- fake_transcript(c("a", "b"))
  plain <- as.character(tr)
  expect_identical(class(plain), "character")
  expect_null(attr(plain, "provenance"))
  expect_equal(plain, c(a = "transcript of a", b = "transcript of b"))
})


test_that("a transcript prints a header and one line per element", {
  tr <- fake_transcript(c("a", "b", "c"), failed = "b")
  tr[1] <- paste(rep("long", 40), collapse = " ")
  expect_snapshot(print(tr, width = 60))
  expect_snapshot(print(tr, n = 1))
  expect_output(print(fake_transcript("a")), "1 transcript; model openai")
})


test_that("a transcript is a corpus with its names as document names", {
  skip_if_not_installed("quanteda")
  tr <- fake_transcript(c("first", "second"))
  corp <- quanteda::corpus(tr)
  expect_equal(quanteda::docnames(corp), c("first", "second"))
  expect_equal(as.character(corp)[["second"]], "transcript of second")
})


# The Gemini backend ----------------------------------------------------------

gemini_runner <- function(answer = function(prompts) lapply(prompts, function(p) fake_chat("verbatim words")),
                          seen = new.env(), env = parent.frame()) {
  withr::local_envvar(c(GEMINI_API_KEY = "test"), .local_envir = env)
  seen$uploads <- character()
  testthat::local_mocked_bindings(
    upload_input_file = function(chat, path) {
      seen$uploads <- c(seen$uploads, basename(path))
      fake_upload(chat, path)
    },
    .env = env
  )
  testthat::local_mocked_bindings(
    parallel_chat = function(chat, prompts, max_active = 10, rpm = 500,
                             on_error = c("return", "continue", "stop")) {
      seen$chat <- chat
      seen$prompts <- prompts
      seen$max_active <- max_active
      seen$rpm <- rpm
      seen$on_error <- on_error
      answer(prompts)
    },
    .package = "ellmer", .env = env
  )
  seen
}

fake_chat <- function(text, cost = 0.002) {
  turn <- text_turn(text, tokens = c(600, 40, 0))
  list(last_turn = function() turn, get_cost = function() cost)
}

test_that("a Gemini model transcribes through an upload and a fresh conversation per file", {
  seen <- gemini_runner()
  a <- audio_file(as.raw(1:8))
  b <- audio_file(as.raw(9:16))

  out <- qlm_transcribe(c(a, b), model = "google_gemini/gemini-2.5-flash",
                        language = "fr", prompt = "A read passage.", max_active = 2, rpm = 30)
  expect_equal(unname(as.character(out)), c("verbatim words", "verbatim words"))
  expect_equal(seen$uploads, basename(c(a, b)))
  expect_length(seen$prompts, 2)
  expect_s3_class(seen$prompts[[1]], "ellmer::ContentUploaded")
  expect_equal(seen$max_active, 2)
  expect_equal(seen$rpm, 30)
  expect_equal(seen$on_error, "continue")
  # The chat starts from the instruction alone
  expect_length(seen$chat$get_turns(), 0)
  instruction <- seen$chat$get_system_prompt()
  expect_match(instruction, "Transcribe the recording verbatim")
  expect_match(instruction, "ISO 639-1 code \"fr\"")
  expect_match(instruction, "Context from the caller: A read passage.")

  prov <- attr(out, "provenance")
  expect_equal(prov$model, rep("google_gemini/gemini-2.5-flash", 2))
  expect_equal(prov$language, c("fr", "fr"))
  expect_equal(prov$usage[[1]]$cost, 0.002)
  expect_equal(prov$usage[[1]]$note, audio_cost_note())
  expect_equal(unname(unlist(prov$usage[[1]]$tokens)), c(600, 40, 0))
})


test_that("the chat route asks the provider rather than a list of what models can do", {
  seen <- gemini_runner(function(prompts) list(request_error("HTTP 400 Bad Request.", 400L)))
  wav <- audio_file()
  # No model name is refused up front: the recording goes up and the
  # provider answers, here with a refusal, recorded and explained
  expect_warning(
    out <- qlm_transcribe(wav, model = "google_gemini/gemini-2.5-flash-tts"),
    "HTTP 400.*chat model must be able to hear"
  )
  expect_length(seen$uploads, 1)
  expect_equal(attr(out, "provenance")$status, "failed")

  # Any other ellmer provider takes the same route
  withr::local_envvar(c(ANTHROPIC_API_KEY = "test"))
  seen <- gemini_runner()
  out <- qlm_transcribe(wav, model = "anthropic/claude-sonnet-4-5")
  expect_equal(unname(as.character(out)), "verbatim words")
  expect_equal(seen$chat$get_provider()@name, "Anthropic")
})


test_that("a registered provider's model goes to its own transcription endpoint", {
  withr::local_envvar(c(MOONSHOT_API_KEY = "moon-key"))
  seen <- new.env()
  seen$reqs <- list()
  httr2::local_mocked_responses(function(req) {
    seen$reqs[[length(seen$reqs) + 1L]] <- req
    httr2::response_json(body = transcription_fixture("whisper_1_us"))
  })
  wav <- audio_file()
  out <- qlm_transcribe(wav, model = "moonshot/whisper-large-v3")
  req <- seen$reqs[[1]]
  expect_equal(req$url, "https://api.moonshot.ai/v1/audio/transcriptions")
  expect_equal(bearer_of(req), "Bearer moon-key")
  expect_equal(req$body$data$model, "whisper-large-v3")
  expect_equal(attr(out, "provenance")$model, "moonshot/whisper-large-v3")
  # The registered host is what distinguishes this run from one against
  # another registration of the same prefix, so it is recorded
  expect_equal(attr(out, "provenance")$base_url, "https://api.moonshot.ai/v1")
})


test_that("a failed upload on the chat route follows on_error like a failed request", {
  paths <- c(audio_file(as.raw(1:8)), audio_file(as.raw(9:16)), audio_file(as.raw(17:24)))
  bad <- basename(paths[2])
  failing_upload <- function(chat, path) {
    if (basename(path) == bad) stop("HTTP 400 Bad Request: unsupported audio.", call. = FALSE)
    fake_upload(chat, path)
  }

  seen <- gemini_runner()
  testthat::local_mocked_bindings(upload_input_file = failing_upload)
  expect_warning(out <- qlm_transcribe(paths, model = "google_gemini/gemini-2.5-flash"),
                 "1 of 3 transcriptions failed.*unsupported audio")
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "ok"))
  expect_match(prov$.error[2], "^upload failed: HTTP 400")
  # The other two were transcribed: one prompt each, the failed one absent
  expect_length(seen$prompts, 2)
  expect_equal(unname(as.character(out))[c(1, 3)], c("verbatim words", "verbatim words"))

  seen <- gemini_runner()
  testthat::local_mocked_bindings(upload_input_file = failing_upload)
  expect_warning(out <- qlm_transcribe(paths, model = "google_gemini/gemini-2.5-flash", on_error = "return"),
                 "not submitted")
  expect_equal(attr(out, "provenance")$status, c("ok", "failed", "unsubmitted"))
  expect_length(seen$prompts, 1)

  seen <- gemini_runner()
  testthat::local_mocked_bindings(upload_input_file = failing_upload)
  expect_error(qlm_transcribe(paths, model = "google_gemini/gemini-2.5-flash", on_error = "stop"),
               "Uploading.*failed.*unsupported audio")
  expect_null(seen$prompts)
})


test_that("an empty answer on the chat route is raised under on_error = 'stop'", {
  seen <- gemini_runner(function(prompts) list(fake_chat("words"), fake_chat("   ")))
  paths <- c(audio_file(as.raw(1:8)), audio_file(as.raw(9:16)))
  expect_error(qlm_transcribe(paths, model = "google_gemini/gemini-2.5-flash", on_error = "stop"),
               "returned no transcript text")
})


test_that("a Gemini failure stays in place, and an empty answer is one", {
  seen <- gemini_runner(function(prompts) list(
    fake_chat("first"),
    request_error("HTTP 503 Service Unavailable.", 503L),
    fake_chat("  ")
  ))
  paths <- c(audio_file(as.raw(1:8)), audio_file(as.raw(9:16)), audio_file(as.raw(17:24)))
  expect_warning(out <- qlm_transcribe(paths, model = "google_gemini/gemini-2.5-flash"),
                 "2 of 3 transcriptions failed")
  prov <- attr(out, "provenance")
  expect_equal(prov$status, c("ok", "failed", "failed"))
  expect_match(prov$.error[2], "HTTP 503")
  expect_match(prov$.error[3], "no transcript text")
  expect_equal(unname(out[[1]]), "first")
})
