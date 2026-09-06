# Helpers ---------------------------------------------------------------------

# qlm_code() with the network stubbed out. The chat is built for real by
# ellmer::chat(), offline, from a dummy key in the environment, so the
# capability check reads a real provider object. Uploads are answered by
# `upload`; the structured call by `results`, or by an error in `errors`.
# Every step is logged in `calls$log`, in order.
audio_runner <- function(results = function(prompts) data.frame(language = rep("en", length(prompts))),
                         errors = NULL, upload = fake_upload, calls = new.env(),
                         env = parent.frame()) {
  withr::local_envvar(
    c(GEMINI_API_KEY = "test", OPENAI_API_KEY = "test", ANTHROPIC_API_KEY = "test"),
    .local_envir = env
  )
  calls$log <- character()
  testthat::local_mocked_bindings(
    upload_input_file = function(chat, path) {
      calls$log <- c(calls$log, paste0("upload:", basename(path)))
      upload(chat, path)
    },
    .env = env
  )
  tsc <- try_structured_call
  i <- 0L
  adapter <- function(chat, prompts, type, batch = FALSE, execution_args = list()) {
    i <<- i + 1L
    calls$log <- c(calls$log, "inference")
    calls$prompts <- prompts
    calls$dots <- execution_args
    err <- if (!is.null(errors) && i <= length(errors)) errors[[i]] else NA_character_
    if (!is.na(err)) stop(err, call. = FALSE)
    out <- if (is.function(results)) results(prompts) else results
    if (is.data.frame(out)) rows_as_turns(out) else out
  }
  mockery::stub(tsc, "structured_chat_turns", adapter)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  f
}

json_calls_stub <- function(calls) {
  function(...) {
    calls$json <- TRUE
    stop("the JSON handler must not be reached for a file input")
  }
}

# Input types and file checks ---------------------------------------------------

test_that("input types are declared in one place", {
  expect_equal(input_types(), c("text", "image", "audio"))
  expect_equal(file_input_types(), c("image", "audio"))
  expect_equal(audio_codebook()$input_type, "audio")
})


test_that("file inputs must be paths of existing files", {
  expect_error(check_file_inputs(123, "audio"), "expects audio file paths")
  expect_error(check_file_inputs(list("a"), "image"), "expects image file paths")

  wav <- audio_file()
  expect_invisible(check_file_inputs(wav, "audio"))
  expect_error(
    check_file_inputs(c(wav, "/nowhere/missing.wav"), "audio"),
    "1 audio file does not exist.*missing.wav"
  )
  expect_error(
    check_file_inputs(c("/nowhere/a.jpg", "/nowhere/b.jpg"), "image"),
    "2 image files do not exist"
  )
})


test_that("audio files must carry an extension the upload can label", {
  wav <- audio_file()
  txt <- audio_file(ext = "txt")
  expect_error(check_file_inputs(c(wav, txt), "audio"), "extension the upload cannot label.*\\.txt")
  # Case does not matter
  upper <- audio_file(ext = "MP3")
  expect_invisible(check_file_inputs(upper, "audio"))
})


test_that("qlm_code checks the files before anything else is built", {
  calls <- new.env()
  f <- audio_runner(calls = calls)
  expect_error(
    f("/nowhere/x.wav", audio_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "does not exist"
  )
  expect_length(calls$log, 0L)
})


# The capability gate ------------------------------------------------------------

test_that("the family pattern accepts recognised Gemini chat models only", {
  families <- input_capabilities()$audio$families
  accepted <- c(
    "gemini-2.5-flash", "gemini-2.5-flash-lite", "gemini-2.5-pro",
    "gemini-3-flash-preview", "gemini-3.7-flash", "gemini-2.5-flash-preview-05-20",
    "gemini-2.0-flash-001", "gemini-2.5-pro-latest"
  )
  refused <- c(
    "gemini-2.5-flash-preview-tts", "gemini-2.5-flash-image", "gemini-3.5-transcribe",
    "gemini-2.5-flash-live", "gemini-embedding-001", "gemini-4-ultra",
    "gemini-2.5-flash-preview-native-audio-dialog", "imagen-4", "gpt-4o"
  )
  expect_true(all(grepl(families, accepted)))
  expect_false(any(grepl(families, refused)))
})


test_that("text and image inputs are not gated", {
  chat <- structure(list(), class = "Chat")
  expect_null(check_input_capability("text", chat, "openai/gpt-4o-mini"))
  expect_null(check_input_capability("image", chat, "openai/gpt-4o-mini"))
})


test_that("a recognised Gemini model passes, and the default model is resolved from the chat", {
  withr::local_envvar(c(GEMINI_API_KEY = "test"))
  chat <- ellmer::chat("google_gemini/gemini-2.5-flash")
  out <- check_input_capability("audio", chat, "google_gemini/gemini-2.5-flash")
  expect_null(out$registered)

  # No model given: the check reads the one ellmer filled in
  default <- suppressMessages(ellmer::chat("google_gemini"))
  expect_match(default$get_model(), "^gemini-")
  expect_no_error(check_input_capability("audio", default, "google_gemini"))
})


test_that("a Gemini model outside the recognised families is refused with the registration call", {
  withr::local_envvar(c(GEMINI_API_KEY = "test"))
  tts <- ellmer::chat("google_gemini/gemini-2.5-flash-preview-tts")
  expect_error(
    check_input_capability("audio", tts, "google_gemini/gemini-2.5-flash-preview-tts"),
    "not recognised as accepting audio input"
  )
  unknown <- ellmer::chat("google_gemini/gemini-4-ultra")
  expect_error(
    check_input_capability("audio", unknown, "google_gemini/gemini-4-ultra"),
    'qlm_register_input_model\\("google_gemini/gemini-4-ultra", input_type = "audio"\\)'
  )
})


test_that("a provider without audio upload is refused before the model is looked at", {
  withr::local_envvar(c(OPENAI_API_KEY = "test", ANTHROPIC_API_KEY = "test"))
  openai <- ellmer::chat("openai/gpt-4o-mini")
  expect_error(
    check_input_capability("audio", openai, "openai/gpt-4o-mini"),
    "reached through OpenAI, which does not"
  )
  anthropic <- ellmer::chat("anthropic/claude-sonnet-4-5")
  expect_error(
    check_input_capability("audio", anthropic, "anthropic/claude-sonnet-4-5"),
    "transcribe the recordings first"
  )
})


test_that("Vertex shares Gemini's provider class but has no file upload, and is refused", {
  # The gate reads the provider's name, which is what separates the two
  vertex <- list(
    get_provider = function() structure(list(), class = "ellmer::ProviderGoogleGemini", name = "Google/Vertex"),
    get_model = function() "gemini-2.5-flash"
  )
  expect_error(
    check_input_capability("audio", vertex, "google_vertex/gemini-2.5-flash"),
    "reached through Google/Vertex, which does not"
  )
})


test_that("a chat whose provider cannot be read is refused rather than trusted", {
  chat <- structure(list(), class = "Chat")
  expect_error(
    check_input_capability("audio", chat, "google_gemini/gemini-2.5-flash"),
    "needs a provider that accepts audio"
  )
})


# Registration -------------------------------------------------------------------

test_that("qlm_register_input_model validates its arguments", {
  withr::defer(reset_registered_input_models())
  expect_error(qlm_register_input_model("google_gemini/x", input_type = "text"), "must be one of")
  expect_error(qlm_register_input_model("google_gemini", input_type = "audio"), "provider/model")
  expect_error(qlm_register_input_model(c("a/b", "c/d")), "single string")
  expect_error(qlm_register_input_model("qwen/qwen3-max"), "Can't reach provider")
})


test_that("a registration accepts one exact pair for the session, and the run records it", {
  withr::defer(reset_registered_input_models())
  withr::local_envvar(c(GEMINI_API_KEY = "test"))

  chat <- ellmer::chat("google_gemini/gemini-4-ultra")
  expect_error(check_input_capability("audio", chat, "google_gemini/gemini-4-ultra"))

  expect_message(
    out <- qlm_register_input_model("google_gemini/gemini-4-ultra", input_type = "audio"),
    "Accepting"
  )
  expect_equal(out, "google_gemini/gemini-4-ultra")
  expect_true(is_registered_input_model("audio", "google_gemini", "gemini-4-ultra"))
  # Exact: a sibling is not accepted
  expect_false(is_registered_input_model("audio", "google_gemini", "gemini-4-ultra-preview"))

  accepted <- check_input_capability("audio", chat, "google_gemini/gemini-4-ultra")
  expect_equal(accepted$registered, "google_gemini/gemini-4-ultra")

  # Through qlm_code(), the acceptance is recorded on the run
  wav <- audio_file()
  f <- audio_runner()
  coded <- f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-4-ultra")
  expect_equal(qlm_meta(coded, type = "user")$input_model_registered, "google_gemini/gemini-4-ultra")

  # A run whose model the table accepts records nothing
  plain <- f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash")
  expect_null(qlm_meta(plain, type = "user")$input_model_registered)

  reset_registered_input_models()
  expect_false(is_registered_input_model("audio", "google_gemini", "gemini-4-ultra"))
})


test_that("a registration bypasses only the model check", {
  withr::defer(reset_registered_input_models())
  withr::local_envvar(c(OPENAI_API_KEY = "test"))
  suppressMessages(qlm_register_input_model("openai/gpt-audio", input_type = "audio"))
  chat <- ellmer::chat("openai/gpt-audio")
  expect_error(
    check_input_capability("audio", chat, "openai/gpt-audio"),
    "reached through OpenAI, which does not"
  )
})


# Uploads and the coding run ------------------------------------------------------

test_that("an audio run uploads every file, then sends one request with the references", {
  calls <- new.env()
  paths <- c(first = audio_file(as.raw(1:10)), second = audio_file(as.raw(11:30)))
  f <- audio_runner(calls = calls)

  coded <- f(paths, audio_codebook(), model = "google_gemini/gemini-2.5-flash")

  expect_s3_class(coded, "qlm_coded")
  expect_equal(coded$.id, c("first", "second"))
  expect_equal(coded$language, c("en", "en"))
  # Both uploads precede the single inference call
  expect_equal(calls$log, c(
    paste0("upload:", basename(paths[[1]])),
    paste0("upload:", basename(paths[[2]])),
    "inference"
  ))
  expect_length(calls$prompts, 2L)
  expect_true(all(vapply(calls$prompts, inherits, logical(1), "ellmer::ContentUploaded")))
  expect_equal(qlm_meta(coded, type = "object")$input_type, "audio")
  expect_equal(qlm_meta(coded, type = "object")$backend, "structured")
})


test_that("a failed upload stops the run with the provider's message and no inference call", {
  calls <- new.env()
  paths <- c(ok = audio_file(), bad = audio_file())
  flaky <- function(chat, path) {
    if (basename(path) == basename(paths[["bad"]])) stop("HTTP 503 Service Unavailable.", call. = FALSE)
    fake_upload(chat, path)
  }
  f <- audio_runner(upload = flaky, calls = calls)

  expect_error(
    f(paths, audio_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "Uploading 1 of 2 audio files failed, so no coding request was sent"
  )
  err <- tryCatch(
    f(paths, audio_codebook(), model = "google_gemini/gemini-2.5-flash"),
    error = function(e) conditionMessage(e)
  )
  # The provider's own message is kept, and the cause is not called deterministic
  expect_match(err, "HTTP 503 Service Unavailable")
  expect_match(err, "transient")
  expect_false("inference" %in% calls$log)
})


test_that("the run records each file's hash, keyed by .id", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  f <- audio_runner()
  coded <- f(paths, audio_codebook(), model = "google_gemini/gemini-2.5-flash")

  files <- qlm_meta(coded, "input_files")
  expect_s3_class(files, "data.frame")
  expect_equal(names(files), c(".id", "file", "size", "sha256"))
  expect_equal(files$.id, c("a", "b"))
  expect_equal(files$file, basename(unname(paths)))
  expect_equal(files$size, c(10, 10))
  expect_equal(files$sha256, unname(vapply(paths, hash_file, "")))

  # Unnamed input is keyed by position, as .id is
  coded2 <- f(unname(paths), audio_codebook(), model = "google_gemini/gemini-2.5-flash")
  expect_equal(qlm_meta(coded2, "input_files")$.id, c("1", "2"))
})


test_that("the audio cost note joins the existing note and is said once", {
  wav <- audio_file()
  f <- audio_runner(results = function(prompts) {
    data.frame(language = "en", input_tokens = 100, output_tokens = 10,
               cached_input_tokens = 0, cost = 0.001)
  })

  expect_message(
    coded <- f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash",
               include_cost = TRUE),
    "potentially underestimated"
  )
  expect_match(qlm_meta(coded, "cost_note"), "^audio input tokens are priced at the text rate")

  # Without a cost there is nothing to qualify
  plain <- f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash")
  expect_null(qlm_meta(plain, type = "user")$cost_note)

  # With supplied rates the qualification follows the rates note
  priced <- f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash",
              prices = c(input = 1, output = 2))
  note <- qlm_meta(priced, "cost_note")
  expect_match(note, "potentially underestimated")
})


test_that("batch is refused for audio before any upload", {
  calls <- new.env()
  wav <- audio_file()
  f <- audio_runner(calls = calls)
  expect_error(
    f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash", batch = TRUE),
    "batch = TRUE.*not supported for audio"
  )
  expect_length(calls$log, 0L)
})


test_that("a provider that cannot take audio is refused before any upload", {
  calls <- new.env()
  wav <- audio_file()
  f <- audio_runner(calls = calls)
  expect_error(
    f(c(a = wav), audio_codebook(), model = "openai/gpt-4o-mini"),
    "reached through OpenAI"
  )
  expect_length(calls$log, 0L)
})


# No JSON path for a file input ---------------------------------------------------

test_that("structured = 'json' is refused for a file input, before any upload", {
  calls <- new.env()
  wav <- audio_file()
  f <- audio_runner(calls = calls)
  mockery::stub(f, "code_handler_json", json_calls_stub(calls))
  expect_error(
    f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash",
      structured = "json"),
    'structured = "json".*not supported for audio input'
  )
  expect_null(calls$json)
  expect_length(calls$log, 0L)
})


test_that("under auto, a failed structured call on a file input reports the provider's error", {
  calls <- new.env()
  wav <- audio_file()
  f <- audio_runner(errors = "HTTP 400 Bad Request. The model cannot read this file.", calls = calls)
  mockery::stub(f, "code_handler_json", json_calls_stub(calls))
  expect_error(
    f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "The model cannot read this file"
  )
  expect_null(calls$json)
  # The upload and the one failed request were made; nothing after
  expect_equal(calls$log, c(paste0("upload:", basename(wav)), "inference"))
})


test_that("the same holds for an image codebook, which used to reach the JSON handler", {
  # A one-pixel PNG, so content_image_file() has something to read
  png <- withr::local_tempfile(fileext = ".png")
  writeBin(as.raw(c(
    0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
    0x89, 0x00, 0x00, 0x00, 0x0a, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9c, 0x63, 0x00, 0x01, 0x00, 0x00,
    0x05, 0x00, 0x01, 0x0d, 0x0a, 0x2d, 0xb4, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae,
    0x42, 0x60, 0x82
  )), png)
  image_codebook <- qlm_codebook(
    "Image", "Describe.", ellmer::type_object(x = ellmer::type_string("x")),
    input_type = "image", image_file_resize = "none"
  )
  calls <- new.env()
  withr::local_envvar(c(OPENAI_API_KEY = "test"))
  # content_image_file() is reached through as_input_content(), and its
  # default resize needs magick, which CI does not have: replace the binding
  testthat::local_mocked_bindings(
    content_image_file = function(path, ...) ellmer::ContentText(path),
    .package = "ellmer"
  )
  tsc <- try_structured_call
  mockery::stub(tsc, "structured_chat_turns",
                function(...) stop("HTTP 400 Bad Request. Image too large.", call. = FALSE))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  mockery::stub(f, "code_handler_json", json_calls_stub(calls))

  expect_error(
    f(c(a = png), image_codebook, model = "openai/gpt-4o-mini"),
    "Image too large"
  )
  expect_null(calls$json)
  expect_error(
    f(c(a = png), image_codebook, model = "openai/gpt-4o-mini", structured = "json"),
    "not supported for image input"
  )
})


# Provenance ----------------------------------------------------------------------

test_that("verify_input_files passes unchanged files and refuses changed or missing ones", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run <- audio_run(paths)

  expect_silent(verify_input_files(run))
  expect_silent(verify_input_files(run, ids = "b"))

  writeBin(as.raw(99:110), paths[["b"]])
  expect_error(verify_input_files(run), "file of 1 unit differs.*\"b\"")
  expect_error(verify_input_files(run, ids = "b"), "differs")
  # The untouched unit alone still passes
  expect_silent(verify_input_files(run, ids = "a"))

  unlink(paths[["a"]])
  expect_error(verify_input_files(run, ids = "a"), "no longer exists")
})


test_that("a run coded without hashes proceeds with a notice, and text runs are not checked", {
  paths <- c(a = audio_file())
  legacy <- audio_run(paths, with_hashes = FALSE)
  expect_message(verify_input_files(legacy), "cannot be verified")

  text_run <- new_qlm_coded(
    results = data.frame(id = "a", score = 1),
    codebook = qlm_codebook("T", "P", ellmer::type_object(score = ellmer::type_number("s"))),
    data = c(a = "some text"), input_type = "text",
    chat_args = list(name = "openai/gpt-4o-mini"), execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 1),
    name = "t", call = quote(qlm_code(...))
  )
  expect_silent(verify_input_files(text_run))
})


test_that("a run that relied on a registration needs it again in this session", {
  withr::defer(reset_registered_input_models())
  paths <- c(a = audio_file())
  run <- audio_run(paths, registered = "google_gemini/gemini-4-ultra",
                   model = "google_gemini/gemini-4-ultra")

  expect_error(
    check_registered_input_model(run, "google_gemini/gemini-4-ultra"),
    'qlm_register_input_model\\("google_gemini/gemini-4-ultra", input_type = "audio"\\)'
  )
  # Another model is checked afresh by qlm_code(), not here
  expect_silent(check_registered_input_model(run, "google_gemini/gemini-2.5-flash"))

  suppressMessages(qlm_register_input_model("google_gemini/gemini-4-ultra", input_type = "audio"))
  expect_silent(check_registered_input_model(run, "google_gemini/gemini-4-ultra"))

  # A run that never relied on one is left alone
  plain <- audio_run(paths)
  expect_silent(check_registered_input_model(plain, "google_gemini/gemini-2.5-flash"))
})


test_that("check_file_inputs lets an image be a URL, and only an image (#177)", {
  png <- withr::local_tempfile(fileext = ".png")
  writeLines("", png)
  urls <- c("https://example.org/a.jpg", "http://example.org/b.png",
            "data:image/png;base64,AAAA")

  expect_invisible(check_file_inputs(c(png, urls), "image"))
  # A URL without its scheme is a path, and is named
  expect_error(
    check_file_inputs(c(png, "example.org/a.jpg"), "image"),
    "1 image file does not exist.*example.org/a.jpg.*or a URL"
  )
  expect_error(check_file_inputs(123, "image"), "image file paths or URLs")
  # Audio is uploaded from disk, so a URL is a missing file there
  expect_error(check_file_inputs(urls[1], "audio"), "1 audio file does not exist")
})


test_that("file provenance leaves a URL unsized and unhashed (#177)", {
  png <- withr::local_tempfile(fileext = ".png")
  writeLines("", png)
  url <- "https://example.org/a.jpg"

  table <- file_provenance(c(png, url), c("a", "b"))
  expect_equal(table$file, c(basename(png), url))
  expect_equal(table$sha256, c(hash_file(png), NA_character_))
  expect_true(is.na(table$size[2]))
  expect_false(is.na(table$size[1]))
})


test_that("verify_input_files skips URLs and checks the files beside them (#177)", {
  png <- withr::local_tempfile(fileext = ".png")
  writeLines("v1", png)
  url <- "https://example.org/a.jpg"
  codebook <- qlm_codebook(
    "Image", "Describe.", ellmer::type_object(x = ellmer::type_string("x")),
    input_type = "image", image_file_resize = "none"
  )
  coded <- new_qlm_coded(
    results = data.frame(id = c("a", "b"), x = c("p", "q")),
    codebook = codebook,
    data = c(a = png, b = url),
    input_type = "image",
    chat_args = list(name = "openai/gpt-4o"),
    execution_args = list(),
    metadata = list(
      timestamp = Sys.time(), n_units = 2,
      input_files = file_provenance(c(png, url), c("a", "b"))
    ),
    name = "posters",
    call = quote(qlm_code(...))
  )

  expect_silent(verify_input_files(coded))
  # Only the URL: nothing to check, nothing said
  expect_silent(verify_input_files(coded, ids = "b"))
  # The file beside it is still checked
  writeLines("v2", png)
  expect_error(verify_input_files(coded), "differs? from the one")
})


test_that("as_input_content dispatches on the input type", {
  expect_equal(as_input_content(c("a", "b"), list(input_type = "text"), NULL),
               list("a", "b"))
  expect_error(as_input_content("a", list(input_type = "video"), NULL),
               "Unknown input type")
})


# Provenance is taken before inference, and survives a backfill -------------------

test_that("hashes record the bytes sent, not the file as it stands after inference", {
  paths <- c(a = audio_file(as.raw(1:10)))
  original <- hash_file(paths[[1]])
  f <- qlm_code
  mockery::stub(f, "try_structured_call", function(...) {
    # The file is replaced while the request is in flight
    writeBin(as.raw(80:90), paths[[1]])
    list(ok = TRUE, value = data.frame(language = "en"))
  })

  coded <- f(paths, audio_codebook(), model = "google_gemini/gemini-2.5-flash")
  expect_equal(qlm_meta(coded, "input_files")$sha256, original)
  # So the replacement is refused by a replication, not accepted as the input
  expect_error(verify_input_files(coded), "differs from the one this run coded")
})


test_that("a file deleted during inference does not lose the results", {
  paths <- c(a = audio_file(as.raw(1:10)))
  original <- hash_file(paths[[1]])
  f <- qlm_code
  mockery::stub(f, "try_structured_call", function(...) {
    unlink(paths[[1]])
    list(ok = TRUE, value = data.frame(language = "en"))
  })
  coded <- f(paths, audio_codebook(), model = "google_gemini/gemini-2.5-flash")
  expect_equal(coded$language, "en")
  expect_equal(qlm_meta(coded, "input_files")$sha256, original)
})


test_that("a unit without a recorded hash is reported as unverifiable, not as changed", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  run <- audio_run(paths)
  meta_attr <- attr(run, "meta")
  meta_attr$user$input_files$sha256[2] <- NA_character_
  attr(run, "meta") <- meta_attr

  expect_message(verify_input_files(run), 'no recorded file hash.*"b"')
  expect_silent(verify_input_files(run, ids = "a"))
  expect_message(verify_input_files(run, ids = "b"), "cannot be verified")

  # A change to the unit that does have a hash is still caught
  writeBin(as.raw(99:110), paths[["a"]])
  expect_error(suppressMessages(verify_input_files(run)), 'differs.*"a"')
})


test_that("merge_input_files fills a pass's rows and builds a table for a legacy run", {
  paths <- c(a = audio_file(as.raw(1:10)), b = audio_file(as.raw(11:20)))
  legacy <- audio_run(paths, with_hashes = FALSE)
  pass <- audio_run(paths["b"])

  merged <- merge_input_files(legacy, pass)
  table <- qlm_meta(merged, "input_files")
  expect_equal(table$.id, c("a", "b"))
  expect_equal(table$sha256, c(NA_character_, hash_file(paths[["b"]])))
  expect_equal(table$file, basename(unname(paths)))
  expect_true(is.na(table$size[1]))

  # A run with a table has the pass's rows replaced, others untouched
  run <- audio_run(paths)
  writeBin(as.raw(99:110), paths[["b"]])
  pass2 <- audio_run(paths["b"])
  merged2 <- merge_input_files(run, pass2)
  table2 <- qlm_meta(merged2, "input_files")
  expect_equal(table2$sha256, c(hash_file(paths[["a"]]), hash_file(paths[["b"]])))

  # A text run, or a pass without a table, is left alone
  expect_identical(merge_input_files(run, legacy), run)
})


test_that("a registration a backfill pass relied on is required again, like the run's", {
  withr::defer(reset_registered_input_models())
  paths <- c(a = audio_file(), b = audio_file())
  run <- audio_run(paths, failed = "b")
  meta_attr <- attr(run, "meta")
  meta_attr$object$backfill <- list(backfill_pass(
    model = "google_gemini/gemini-4-ultra", overrides = list(), attempted = "b",
    recovered = "b", registered = "google_gemini/gemini-4-ultra"
  ))
  attr(run, "meta") <- meta_attr

  expect_equal(recorded_registrations(run), "google_gemini/gemini-4-ultra")
  expect_error(
    check_registered_input_model(run, "google_gemini/gemini-4-ultra"),
    "qlm_register_input_model"
  )
  # The run's own model needed no registration
  expect_silent(check_registered_input_model(run, "google_gemini/gemini-2.5-flash"))
  suppressMessages(qlm_register_input_model("google_gemini/gemini-4-ultra", input_type = "audio"))
  expect_silent(check_registered_input_model(run, "google_gemini/gemini-4-ultra"))
})
