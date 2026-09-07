# Helpers ---------------------------------------------------------------------

# qlm_code() with the network stubbed out. The chat is built for real by
# ellmer::chat(), offline, from a dummy key in the environment, so the run
# sees a real provider object. Uploads are answered by `upload`, downloads
# by `download`; the structured call by `results`, or by an error in
# `errors`. Every step is logged in `calls$log`, in order.
audio_runner <- function(results = function(prompts) data.frame(language = rep("en", length(prompts))),
                         errors = NULL, upload = fake_upload, download = fake_download(),
                         calls = new.env(), env = parent.frame()) {
  withr::local_envvar(
    c(GEMINI_API_KEY = "test", OPENAI_API_KEY = "test", ANTHROPIC_API_KEY = "test",
      DEEPSEEK_API_KEY = "test"),
    .local_envir = env
  )
  calls$log <- character()
  calls$downloads <- character()
  testthat::local_mocked_bindings(
    upload_input_file = function(chat, path) {
      calls$log <- c(calls$log, paste0("upload:", basename(path)))
      upload(chat, path)
    },
    download_input_url = function(url, dest) {
      calls$log <- c(calls$log, paste0("download:", url))
      calls$downloads <- c(calls$downloads, dest)
      download(url, dest)
    },
    # The fixtures are not real video, so the durations notice takes the
    # no-av branch whether or not av is installed; the notice has its own test
    has_av = function() FALSE,
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
  expect_equal(input_types(), c("text", "image", "audio", "video"))
  expect_equal(file_input_types(), c("image", "audio", "video"))
  expect_equal(uploaded_input_types(), c("audio", "video"))
  expect_equal(audio_codebook()$input_type, "audio")
  expect_equal(video_codebook()$input_type, "video")
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


test_that("batch is refused for audio and video before any upload", {
  calls <- new.env()
  wav <- audio_file()
  f <- audio_runner(calls = calls)
  expect_error(
    f(c(a = wav), audio_codebook(), model = "google_gemini/gemini-2.5-flash", batch = TRUE),
    "batch = TRUE.*not supported for audio"
  )
  expect_error(
    f(c(a = video_file()), video_codebook(), model = "google_gemini/gemini-2.5-flash",
      batch = TRUE),
    "batch = TRUE.*not supported for video"
  )
  expect_length(calls$log, 0L)
})


# No gate: a provider is tried, and its refusal reported with what is known -------

test_that("a provider without file management fails at the upload, with the hint", {
  # The real ellmer upload, so that its `not_implemented` refusal is what
  # the run sees; DeepSeek has no file management through ellmer
  calls <- new.env()
  wav <- audio_file()
  f <- audio_runner(upload = function(chat, path) chat$file_upload(path), calls = calls)
  err <- tryCatch(
    f(c(a = wav), audio_codebook(), model = "deepseek/deepseek-chat"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Uploading 1 of 1 audio file failed")
  expect_match(err, "doesn't support file management")
  expect_match(err, "only Google Gemini .* is known to accept audio input")
  expect_false("inference" %in% calls$log)
})


test_that("a provider that uploads and then refuses is reported with the hint", {
  calls <- new.env()
  clip <- video_file()
  f <- audio_runner(errors = rep("HTTP 400 Bad Request. Unsupported file type.", 3), calls = calls)
  mockery::stub(f, "code_handler_json", json_calls_stub(calls))
  err <- tryCatch(
    f(c(a = clip), video_codebook(), model = "openai/gpt-4o-mini"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Unsupported file type")
  expect_match(err, "only Google Gemini .* is known to accept video input")
  expect_match(err, "Video input.*section")
  expect_null(calls$json)
  # The upload and the one refused request were made
  expect_equal(calls$log, c(paste0("upload:", basename(clip)), "inference"))

  # The same under structured = "structured"
  err2 <- tryCatch(
    f(c(a = clip), video_codebook(), model = "openai/gpt-4o-mini", structured = "structured"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err2, "only Google Gemini")

  # A text codebook refused the same way gets no such hint
  text <- qlm_codebook("T", "P", ellmer::type_object(language = ellmer::type_string("l")))
  err3 <- tryCatch(
    f(c(a = "hello"), text, model = "openai/gpt-4o-mini", structured = "structured"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err3, "Unsupported file type")
  expect_no_match(err3, "only Google Gemini")
})


test_that("the hint names the input type, and is nothing for text and images", {
  expect_null(known_input_providers_hint("text"))
  expect_null(known_input_providers_hint("image"))
  expect_match(known_input_providers_hint("audio"), "accept audio input")
  expect_match(known_input_providers_hint("video"), "Video input.* section")
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
  expect_error(as_input_content("a", list(input_type = "hologram"), NULL),
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




# Video input (#179) ---------------------------------------------------------------

test_that("check_file_inputs takes video paths, YouTube links and video URLs", {
  clip <- video_file()
  expect_invisible(check_file_inputs(clip, "video"))
  expect_invisible(check_file_inputs(video_file(ext = "MP4"), "video"))
  expect_invisible(check_file_inputs(video_file(ext = "webm"), "video"))
  urls <- c("https://www.youtube.com/watch?v=jNQXAC9IVRw", "https://youtu.be/jNQXAC9IVRw",
            "https://www.youtube.com/shorts/abc", "https://m.youtube.com/live/abc",
            "https://archive.org/download/PolAd_x/PolAd_x.mp4?download=1")
  expect_invisible(check_file_inputs(c(clip, urls), "video"))

  expect_error(check_file_inputs(123, "video"), "video file paths or URLs")
  expect_error(
    check_file_inputs(c(clip, "/nowhere/missing.mp4"), "video"),
    "1 video file does not exist.*missing.mp4.*or a URL"
  )
  err <- tryCatch(check_file_inputs("/nowhere/missing.mp4", "video"), error = function(e) conditionMessage(e))
  expect_no_match(err, "data:")
  # A URL without its scheme is a path
  expect_error(check_file_inputs("youtube.com/watch?v=x", "video"), "does not exist")
})


test_that("video files and URLs must carry an extension the upload can label", {
  clip <- video_file()
  mkv <- video_file(ext = "mkv")
  txt <- video_file(ext = "txt")
  expect_error(check_file_inputs(c(clip, mkv), "video"), "extension the upload cannot label.*\\.mkv")
  expect_error(check_file_inputs(txt, "video"), "Video files must be one of")
  # A video URL is downloaded and uploaded like a file, so it is checked like one
  expect_error(
    check_file_inputs("https://example.org/clip", "video"),
    "file or URL has an extension the upload cannot label.*must end in one of these extensions"
  )
  expect_error(check_file_inputs("https://example.org/clip.mkv", "video"), "cannot label")
  # A YouTube link has no file to label
  expect_invisible(check_file_inputs("https://youtu.be/jNQXAC9IVRw", "video"))
  # No inline video
  expect_error(check_file_inputs("data:video/mp4;base64,AAAA", "video"), "data:.*URI.*video input does not accept")
})


test_that("URL predicates tell paths, URLs and YouTube links apart", {
  x <- c("/a/b.mp4", "https://example.org/b.mp4", "http://example.org/b.mp4",
         "data:image/png;base64,AA", "https://www.youtube.com/watch?v=x", "youtu.be/x", NA)
  expect_equal(is_input_url(x), c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(is_input_url(x, data = TRUE), c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(is_image_url(x), is_input_url(x, data = TRUE))
  expect_equal(is_youtube_url(x), c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(url_extension("https://a.org/x/clip.MP4?dl=1#t"), "mp4")
  expect_equal(url_extension("https://a.org/clip"), "")
})


test_that("a video run sends a path by upload, a YouTube link by reference and a URL by download then upload", {
  calls <- new.env()
  clip <- video_file(as.raw(1:10))
  x <- c(clip = clip, zoo = "https://www.youtube.com/watch?v=jNQXAC9IVRw",
         ad = "https://user:secret@archive.org/download/ad/ad.mp4")
  f <- audio_runner(calls = calls, download = fake_download(as.raw(11:30)))

  expect_message(
    coded <- f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "Uploading 2 video files"
  )
  expect_equal(coded$.id, c("clip", "zoo", "ad"))
  expect_equal(coded$language, rep("en", 3))

  # The download precedes the uploads, which precede the one inference call
  expect_length(calls$downloads, 1L)
  expect_equal(calls$log, c(
    "download:https://user:secret@archive.org/download/ad/ad.mp4",
    paste0("upload:", basename(clip)),
    paste0("upload:", basename(calls$downloads)),
    "inference"
  ))
  # The downloaded file has the URL's extension, and is gone when the run returns
  expect_match(calls$downloads, "\\.mp4$")
  expect_false(file.exists(calls$downloads))

  # One reference per element, in order; the YouTube one carries the URL itself
  expect_length(calls$prompts, 3L)
  expect_true(all(vapply(calls$prompts, inherits, logical(1), "ellmer::ContentUploaded")))
  expect_equal(calls$prompts[[2]]@uri, "https://www.youtube.com/watch?v=jNQXAC9IVRw")
  expect_equal(calls$prompts[[2]]@provider, "Google/Gemini")
  expect_match(calls$prompts[[1]]@uri, "^files/")
  expect_match(calls$prompts[[3]]@uri, "^files/")

  # Provenance: hash for the path and the downloaded bytes, the redacted URL, nothing for YouTube
  files <- qlm_meta(coded, "input_files")
  expect_equal(files$.id, c("clip", "zoo", "ad"))
  expect_equal(files$file, c(basename(clip), "https://www.youtube.com/watch?v=jNQXAC9IVRw",
                             "https://archive.org/download/ad/ad.mp4"))
  expect_equal(files$size, c(10, NA, 20))
  expect_equal(files$sha256[1], hash_file(clip))
  expect_true(is.na(files$sha256[2]))
  expect_equal(files$sha256[3], digest::digest(as.raw(11:30), algo = "sha256", serialize = FALSE))
  expect_equal(qlm_meta(coded, type = "object")$input_type, "video")
  # No audio cost note on a video run
  expect_null(qlm_meta(coded, type = "user")$cost_note)
})


test_that("a failed download stops the run before any upload, naming the URL", {
  calls <- new.env()
  x <- c(a = video_file(), b = "https://example.org/gone.mp4", c = "https://example.org/ok.mp4")
  flaky <- function(url, dest) {
    if (grepl("gone", url)) stop("cannot open URL 'https://example.org/gone.mp4': HTTP status was '404 Not Found'", call. = FALSE)
    fake_download()(url, dest)
  }
  f <- audio_runner(download = flaky, calls = calls)
  err <- tryCatch(
    suppressMessages(f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Downloading 1 of 2 video URLs failed, so nothing was uploaded or sent")
  expect_match(err, "gone.mp4: cannot open URL")
  expect_match(err, "404")
  expect_false(any(grepl("^upload:", calls$log)))
  # The download that did succeed is cleaned up
  expect_false(any(file.exists(calls$downloads)))

  # A warning from download.file() is a failure too, and the URL it quotes
  # is redacted along with the label
  secret <- c(a = "https://user:hunter2@example.org/ad.mp4?api_key=SECRET123")
  warns <- function(url, dest) {
    warning(paste0("cannot open URL '", url, "': HTTP status was '403 Forbidden'"))
    invisible(dest)
  }
  g <- audio_runner(download = warns, calls = calls)
  err <- tryCatch(
    suppressMessages(g(secret, video_codebook(), model = "google_gemini/gemini-2.5-flash")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "403 Forbidden")
  expect_match(err, "https://example.org/ad.mp4\\?api_key=<redacted>")
  expect_no_match(err, "SECRET123|hunter2")
})


test_that("redact_urls_in_text redacts every URL a message quotes", {
  text <- "cannot open URL 'https://k:pw@a.org/x.mp4?token=abc&v=1' after http://b.org/y.mp4; done"
  expect_equal(
    redact_urls_in_text(text),
    "cannot open URL 'https://a.org/x.mp4?token=<redacted>&v=1' after http://b.org/y.mp4; done"
  )
  expect_equal(redact_urls_in_text("no url here"), "no url here")
})


test_that("a download that fails the size check is removed", {
  calls <- new.env()
  x <- c(a = "https://example.org/big.mp4")
  f <- audio_runner(calls = calls)
  testthat::local_mocked_bindings(max_upload_bytes = function() 10)
  expect_error(
    suppressMessages(f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash")),
    "exceeds the"
  )
  expect_length(calls$downloads, 1L)
  expect_false(file.exists(calls$downloads))
})


test_that("verify_input_files checks files and leaves every URL to the run that downloads it", {
  clip <- video_file(as.raw(1:10))
  downloaded <- video_file(as.raw(11:30))
  x <- c(clip = clip, ad = "https://example.org/ad.mp4", zoo = "https://youtu.be/x")
  run <- media_run(x, input_type = "video", local = c(clip, downloaded, NA))

  seen <- character()
  testthat::local_mocked_bindings(download_input_url = function(url, dest) {
    seen <<- c(seen, url)
    writeBin(as.raw(11:30), dest)
    invisible(dest)
  })
  expect_silent(verify_input_files(run))
  expect_silent(verify_input_files(run, ids = c("ad", "zoo")))
  expect_length(seen, 0L)

  writeBin(as.raw(99:110), clip)
  expect_error(verify_input_files(run), 'differs from the one this run coded: "clip"')
  expect_silent(verify_input_files(run, ids = "ad"))
})


test_that("the download a run uploads is checked against the hashes it was handed", {
  clip <- video_file(as.raw(1:10))
  downloaded <- video_file(as.raw(11:30))
  x <- c(clip = clip, ad = "https://example.org/ad.mp4", zoo = "https://youtu.be/x")
  run <- media_run(x, input_type = "video", local = c(clip, downloaded, NA))

  expected <- expected_url_hashes(run)
  expect_equal(expected$url, "https://example.org/ad.mp4")
  expect_equal(expected$.id, "ad")
  expect_equal(expected$sha256, hash_file(downloaded))
  expect_equal(nrow(expected_url_hashes(run, ids = c("clip", "zoo"))), 0L)
  expect_equal(nrow(expected_url_hashes(media_run(c(a = audio_file())))), 0L)

  calls <- new.env()
  # The same bytes: the run proceeds, with one download
  f <- audio_runner(calls = calls, download = fake_download(as.raw(11:30)))
  coded <- with_expected_hashes(expected, suppressMessages(
    f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash")
  ))
  expect_equal(sum(grepl("^download:", calls$log)), 1L)
  expect_equal(qlm_meta(coded, "input_files")$sha256[2], hash_file(downloaded))

  # Different bytes: refused after the download and before any upload
  g <- audio_runner(calls = calls, download = fake_download(as.raw(99:110)))
  err <- tryCatch(
    with_expected_hashes(expected, suppressMessages(
      g(x, video_codebook(), model = "google_gemini/gemini-2.5-flash")
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, 'The video file of 1 unit differs from the one this run coded: "ad"')
  expect_match(err, "URL now serves different bytes")
  expect_false(any(grepl("^upload:", calls$log)))
  expect_false(any(file.exists(calls$downloads)))
  # The expectation does not outlive the call
  expect_null(input_state$expected)

  # With no expectation in force, any bytes are accepted
  coded2 <- suppressMessages(g(x, video_codebook(), model = "google_gemini/gemini-2.5-flash"))
  expect_s3_class(coded2, "qlm_coded")
})



test_that("a failed upload after a download removes the temporary file, and names the URL", {
  calls <- new.env()
  clip <- video_file()
  x <- c(a = clip, b = "https://example.org/ad.mp4")
  flaky <- function(chat, path) {
    if (basename(path) != basename(clip)) stop("HTTP 503 Service Unavailable.", call. = FALSE)
    fake_upload(chat, path)
  }
  f <- audio_runner(upload = flaky, calls = calls)
  err <- tryCatch(
    suppressMessages(f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Uploading 1 of 2 video files failed")
  # Named by the URL the user gave, not the temporary file
  expect_match(err, "https://example.org/ad.mp4: HTTP 503")
  expect_false(any(file.exists(calls$downloads)))
  expect_false("inference" %in% calls$log)
})


test_that("what is about to be uploaded is said once, with durations when av can read them", {
  calls <- new.env()
  x <- c(a = video_file(as.raw(rep(1:100, 10))), b = "https://youtu.be/x")
  f <- audio_runner(calls = calls)

  testthat::local_mocked_bindings(has_av = function() FALSE)
  expect_message(
    f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "Uploading 1 video file, 1.0 KB in all.*install the av package"
  )

  testthat::local_mocked_bindings(has_av = function() TRUE, video_duration = function(path) 12.4)
  expect_message(
    f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "12 seconds in all.*roughly 3,720 input tokens\\.$"
  )

  # A duration av cannot read is left out of the estimate and said
  y <- c(a = video_file(as.raw(1:50)), b = video_file(as.raw(51:100)))
  testthat::local_mocked_bindings(
    has_av = function() TRUE,
    video_duration = function(path) if (basename(path) == basename(y[["a"]])) 10 else NA_real_
  )
  expect_message(
    f(y, video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "Uploading 2 video files, 10 seconds in all.*3,000 input tokens, not counting 1 file whose duration could not be read"
  )
  testthat::local_mocked_bindings(has_av = function() TRUE, video_duration = function(path) NA_real_)
  expect_message(
    f(y, video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "Uploading 2 video files, 0.1 KB in all.*the durations could not be read"
  )

  # A YouTube-only run has nothing to upload, and says nothing
  expect_no_message(
    f(x["b"], video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    message = "Uploading"
  )
  expect_equal(format_seconds(3600 * 1.5), "1.5 hours")
  expect_equal(format_seconds(90), "1.5 minutes")
  expect_equal(format_bytes(3 * 1024^3), "3.0 GB")
  expect_equal(format_bytes(1.5 * 1024^2), "1.5 MB")
})


test_that("a video file over the upload's limit is refused before anything is sent", {
  calls <- new.env()
  small <- video_file(as.raw(1:10))
  big <- video_file(as.raw(1:100))
  f <- audio_runner(calls = calls)
  testthat::local_mocked_bindings(max_upload_bytes = function() 50)
  expect_error(
    suppressMessages(f(c(a = small, b = big), video_codebook(), model = "google_gemini/gemini-2.5-flash")),
    "1 video file exceeds the .* the provider's upload accepts"
  )
  expect_length(calls$log, 0L)
})


test_that("file provenance records downloaded bytes against the URL, and nothing for a YouTube link", {
  clip <- video_file(as.raw(1:10))
  downloaded <- video_file(as.raw(11:30))
  x <- c(clip, "https://key:abc@example.org/ad.mp4?token=xyz", "https://youtu.be/x")
  table <- file_provenance(x, c("a", "b", "c"), local = c(clip, downloaded, NA))
  expect_equal(table$file, c(basename(clip), "https://example.org/ad.mp4?token=<redacted>", "https://youtu.be/x"))
  expect_equal(table$size, c(10, 20, NA))
  expect_equal(table$sha256, c(hash_file(clip), hash_file(downloaded), NA))
  # Without `local`, a URL has no bytes here (the image case)
  expect_true(all(is.na(file_provenance(x[2:3], c("b", "c"))$sha256)))
})


test_that("merge_input_files names a legacy run's URLs by their URL", {
  clip <- video_file(as.raw(1:10))
  x <- c(a = clip, b = "https://example.org/ad.mp4")
  legacy <- media_run(x, input_type = "video", with_hashes = FALSE)
  pass <- media_run(x["a"], input_type = "video")
  table <- qlm_meta(merge_input_files(legacy, pass), "input_files")
  expect_equal(table$file, c(basename(clip), "https://example.org/ad.mp4"))
  expect_equal(table$sha256, c(hash_file(clip), NA))
})


test_that("resolve_input_files leaves audio and image inputs where they are", {
  wav <- audio_file()
  expect_equal(resolve_input_files(c(a = wav), "audio"), list(local = wav, temp = character()))
  png <- withr::local_tempfile(fileext = ".png")
  writeLines("", png)
  out <- resolve_input_files(c(png, "https://example.org/a.jpg"), "image")
  expect_equal(out$local, c(png, NA))
  expect_length(out$temp, 0L)
})


test_that("two units sharing a URL are each held to their own recorded bytes", {
  first <- video_file(as.raw(1:10))
  second <- video_file(as.raw(11:30))
  url <- "https://example.org/ad.mp4"
  x <- c(a = url, b = url)
  run <- media_run(x, input_type = "video", local = c(first, second))
  expected <- expected_url_hashes(run)
  expect_equal(expected$.id, c("a", "b"))
  expect_equal(expected$sha256, c(hash_file(first), hash_file(second)))

  # Both downloads now return the first unit's bytes: the second has changed
  calls <- new.env()
  f <- audio_runner(calls = calls, download = fake_download(as.raw(1:10)))
  err <- tryCatch(
    with_expected_hashes(expected, suppressMessages(
      f(x, video_codebook(), model = "google_gemini/gemini-2.5-flash")
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, 'differs from the one this run coded: "b"')
  expect_no_match(err, '"a"')
  expect_false(any(grepl("^upload:", calls$log)))

  # Unnamed input is matched by position, as .id is
  unnamed <- media_run(unname(x), input_type = "video", local = c(first, second))
  expect_equal(expected_url_hashes(unnamed)$.id, c("1", "2"))
  g <- audio_runner(calls = calls, download = fake_download(as.raw(11:30)))
  err2 <- tryCatch(
    with_expected_hashes(expected_url_hashes(unnamed), suppressMessages(
      g(unname(x), video_codebook(), model = "google_gemini/gemini-2.5-flash")
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(err2, 'differs from the one this run coded: "1"')
})


test_that("video_duration gives NA for a file av cannot read, and never an error", {
  skip_if_not_installed("av")
  garbage <- video_file(as.raw(1:64))
  expect_identical(video_duration(garbage), NA_real_)
  # And so a run over such a file is not stopped by the notice
  calls <- new.env()
  f <- audio_runner(calls = calls)
  testthat::local_mocked_bindings(has_av = function() TRUE)
  expect_message(
    coded <- f(c(a = garbage), video_codebook(), model = "google_gemini/gemini-2.5-flash"),
    "durations could not be read"
  )
  expect_s3_class(coded, "qlm_coded")
})
