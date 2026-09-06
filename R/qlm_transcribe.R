#' Transcribe audio recordings
#'
#' Transcribes audio files with a speech-to-text model and returns the
#' transcripts as a character vector that carries the provenance of each
#' one: the file it came from and its hash, the model, the language and
#' prompt given, the time, and the usage the provider reported. Passed to
#' [qlm_code()] with a text codebook, the transcripts are coded as ordinary
#' text and that provenance is recorded with the run, so [qlm_trail()]
#' documents the transcription as part of the measurement instrument and the
#' same text can be coded again by [qlm_replicate()] or [qlm_backfill()]
#' without another transcription request.
#'
#' This is the two-stage route to audio: transcribe once, then code the text
#' with any provider. The single-pass route, a codebook with
#' `input_type = "audio"`, sends the recording itself to a model that can
#' hear it; see the "Audio input" section of [qlm_code()] for the providers
#' that accept it.
#'
#' @section Routes:
#'
#' The route is chosen from the provider prefix of `model`, not from a list
#' of models known to transcribe. Whether a model can is for the provider
#' to say, and asking costs nothing: an upload is free and a refused
#' request is not billed.
#'
#' - **The transcription endpoint**, `/audio/transcriptions`, for `openai/`
#'   and for any provider registered with [qlm_register_provider()], at the
#'   host it was registered with. OpenAI's models are
#'   `gpt-4o-mini-transcribe` (the default), `gpt-4o-transcribe` and
#'   `whisper-1`; a registered host serves whatever it serves, such as
#'   Whisper on Groq. Each file must be at most 25 MB and one of `flac`,
#'   `mp3`, `mp4`, `mpeg`, `mpga`, `m4a`, `ogg`, `wav` or `webm`. OpenAI
#'   reports usage as audio and text tokens for the `gpt-4o` models and as
#'   seconds of audio for `whisper-1`.
#' - **A chat model that hears the recording**, for every other provider
#'   ellmer reaches: the file is uploaded through ellmer's file upload and
#'   the model is asked for a verbatim transcript. Known to work: Google
#'   Gemini's `pro`, `flash` and `flash-lite` models (`google_gemini/`).
#'   Anthropic takes no audio, and OpenAI's chat models refuse it; a provider
#'   that cannot take the recording says so in the failure recorded for each
#'   unit. Usage is the token count and the cost ellmer computes, with the
#'   qualification that ellmer prices audio tokens at the text rate.
#'   Gemini's dedicated transcription model, `gemini-3.5-transcribe`, cannot
#'   be reached through ellmer yet.
#'
#' No dollar cost is computed on the endpoint route: ellmer has no rates for
#' transcription models, and per-minute pricing does not fit a per-token
#' table. The usage is recorded as reported so it can be costed by hand.
#'
#' @section Names and identifiers:
#'
#' The names of the result become the `.id` of each unit when it is coded,
#' and the document names when the vector is made a corpus. A supplied name
#' is kept exactly. An unnamed local file is named by its basename; an
#' unnamed URL is named `text1`, `text2`, ... by its position in `x`. The
#' resolved names must be unique and non-empty: two files that share a
#' basename need names supplied, and the error says so.
#'
#' @section Failures:
#'
#' Requests run in parallel. Under `on_error = "continue"` every file is
#' attempted and the result has an element for each, `NA` where the
#' transcription failed, with the provider's message in the `.error` column
#' of the provenance table. `"return"` stops submitting after the first
#' failure and marks the files it never sent as such; `"stop"` raises the
#' first error. A failed download, a failed upload on the chat route, a
#' refused request and a response with no transcript in it are all
#' failures of the unit, under the same policy. The one limit is on the
#' chat route, where an empty answer is known only after every request
#' has returned, so `"return"` cannot withhold submissions on its account.
#' Validation of the arguments, the files and the model all happen before
#' anything is downloaded or sent, and abort whatever `on_error` says.
#'
#' A missing transcript passed to [qlm_code()] is never sent to the model:
#' its unit is recorded as failed with the transcription's reason, and
#' [qlm_backfill()] leaves it alone. Transcribe the file again and assign the
#' result at that position, `transcripts[failed] <- qlm_transcribe(files[failed])`,
#' which replaces the record with it; or concatenate independent runs with
#' `c()`.
#'
#' @section URLs:
#'
#' An element of `x` that is an `http://` or `https://` URL is downloaded
#' to a temporary file, which is removed when the function returns. The
#' hash and size recorded are those of the downloaded bytes, and the URL is
#' recorded, with any credential it carried redacted, as the `source`. The format is read from the URL's path, so a URL with no file
#' extension is refused before anything is fetched.
#'
#' @param x character; paths of audio files, or `http(s)` URLs of them,
#'   optionally named. See the section "Names and identifiers".
#' @param model character; the transcription model in `"provider/model"`
#'   form. See the section "Routes".
#' @param language character; the ISO 639-1 code of the language spoken,
#'   such as `"en"` or `"zh"`, or `NULL` to let the model detect it. Sent to
#'   the OpenAI endpoint as its `language` field; added to the instruction
#'   for a Gemini model.
#' @param prompt character; optional text to guide the transcription, such
#'   as names and terms the recording contains or the style of punctuation
#'   wanted. Sent to the OpenAI endpoint as its `prompt` field; added to the
#'   instruction for a Gemini model.
#' @param api_key character; the API key. `NULL` reads the environment
#'   variable the provider uses, `OPENAI_API_KEY`, the variable a registered
#'   provider was given, or the one ellmer reads for a chat provider. On the
#'   chat route the value is passed to ellmer as the credential itself,
#'   which is what Gemini and Anthropic take. The key is never recorded.
#' @param base_url character; the endpoint to send the requests to. On the
#'   endpoint route, the prefix before `/audio/transcriptions`; `NULL` is
#'   the provider's own host. On the chat route, passed to ellmer's chat
#'   constructor. Recorded with any credential it carries redacted.
#' @param max_active integer; the number of requests in flight at once, as
#'   in [ellmer::parallel_chat()].
#' @param rpm integer; the request rate in requests per minute. The default
#'   is below ellmer's because transcription endpoints have their own,
#'   lower, rate limits. A rate-limited request is retried after the delay
#'   the provider asks for.
#' @param on_error character; what to do when a transcription fails. See the
#'   section "Failures".
#' @param ... Reserved; must be empty.
#'
#' @return A named character vector of class `qlm_transcript`, one element
#'   per element of `x` in the same order, with attribute `provenance`, a
#'   data frame with one row per element:
#'   \describe{
#'     \item{`.id`}{the element's name.}
#'     \item{`status`}{`"ok"`, `"failed"` or `"unsubmitted"`.}
#'     \item{`source`}{the basename of a local file, or the URL with any
#'       credential it carried redacted.}
#'     \item{`.error`}{the failure message, or `NA`.}
#'     \item{`size`, `sha256`}{the bytes transcribed and their hash; `NA`
#'       when a download failed.}
#'     \item{`model`}{as given.}
#'     \item{`language`, `prompt`}{as given, or `NA`.}
#'     \item{`base_url`}{the host the requests went to, redacted: on the
#'       endpoint route always, on the chat route when given.}
#'     \item{`timestamp`}{when the response arrived, or `NA`.}
#'     \item{`usage`}{a list column holding what the provider reported, or
#'       on the chat route ellmer's tokens, cost and version.}
#'   }
#'   Subsetting with `[`, renaming with `names<-` and concatenating with
#'   `c()` keep the table aligned with the elements. Assigning a
#'   `qlm_transcript` with `[<-` or `[[<-` replaces rows of the table too,
#'   so a retried transcription replaces the failure it retries; assigning
#'   plain text records an edit. `as.character()` drops the table.
#'
#' @examples
#' \dontrun{
#' files <- list.files("recordings", pattern = "\\.wav$", full.names = TRUE)
#' transcripts <- qlm_transcribe(files)
#' transcripts
#' attr(transcripts, "provenance")
#'
#' # Code the transcripts with any provider; the run records the transcription
#' coded <- qlm_code(transcripts, codebook_sentiment, model = "anthropic/claude-sonnet-5")
#' qlm_trail(coded, path = "sentiment_trail")
#'
#' # A Gemini chat model as the transcriber, with a language hint
#' transcripts <- qlm_transcribe(files, model = "google_gemini/gemini-2.5-flash",
#'                               language = "fr")
#'
#' # Whisper on a registered OpenAI-compatible host
#' qlm_register_provider("groq", "https://api.groq.com/openai/v1", "GROQ_API_KEY")
#' transcripts <- qlm_transcribe(files, model = "groq/whisper-large-v3")
#'
#' # A recording on the web, named so the name becomes its .id
#' url <- c(harvard = "https://www.voiptroubleshooter.com/open_speech/american/OSR_us_000_0010_8k.wav")
#' qlm_transcribe(url)
#' }
#'
#' @seealso [qlm_code()] for coding the transcripts, and its "Audio input"
#'   section for the single-pass route; [qlm_trail()] for the record a coded
#'   transcript leaves.
#' @export
qlm_transcribe <- function(x, model = "openai/gpt-4o-mini-transcribe",
                           language = NULL, prompt = NULL,
                           api_key = NULL, base_url = NULL,
                           max_active = 10, rpm = 60,
                           on_error = c("continue", "return", "stop"), ...) {
  rlang::check_dots_empty()
  on_error <- rlang::arg_match(on_error)

  if (!is.character(x) || !length(x)) {
    cli::cli_abort(c(
      "{.arg x} must be a character vector of audio file paths or URLs.",
      "i" = "At least one file is needed."
    ))
  }
  if (anyNA(x)) {
    cli::cli_abort("{.arg x} must not contain {.code NA}: {sum(is.na(x))} element{?s} {?is/are} missing.")
  }
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    cli::cli_abort(c(
      "{.arg model} must be a single string.",
      "i" = "Use the form {.val provider/model}, for example {.val openai/gpt-4o-mini-transcribe}."
    ))
  }
  check_optional_string(language, "language")
  check_optional_string(prompt, "prompt")
  check_optional_string(api_key, "api_key")
  check_optional_string(base_url, "base_url")
  if (!is_count(max_active, 1L)) {
    cli::cli_abort("{.arg max_active} must be a single whole number of at least 1.")
  }
  if (!is_count(rpm, 1L)) {
    cli::cli_abort("{.arg rpm} must be a single whole number of at least 1.")
  }

  check_model_provider(model)
  backend <- transcription_backend(model)

  # Names are settled first, from the input as given, so that neither the
  # order of completion nor a failure can change an identifier
  is_url <- is_http_url(x)
  ids <- transcript_ids(x, is_url)
  check_transcription_sources(x, is_url, backend)

  # Whatever the backend needs to know before a byte is fetched or sent
  setup <- switch(backend,
    endpoint = endpoint_transcription_setup(model, api_key, base_url),
    chat = chat_transcription_setup(model, language, prompt, api_key, base_url)
  )

  # URLs are fetched to a directory that goes when the function returns
  workdir <- tempfile("quallmer_transcribe_")
  dir.create(workdir)
  on.exit(unlink(workdir, recursive = TRUE), add = TRUE)
  prepared <- prepare_transcription_files(x, is_url, workdir, backend, on_error)

  # Under "return", a download failure stops the submissions after it, as
  # a request failure would
  ready <- is.na(prepared$error)
  if (on_error == "return" && any(!ready)) {
    ready[seq_along(x) > which(!ready)[1]] <- FALSE
  }

  outcomes <- vector("list", length(x))
  if (any(ready)) {
    outcomes[ready] <- switch(backend,
      endpoint = transcribe_endpoint(
        prepared$path[ready], model_id = model_id(model),
        language = language, prompt = prompt, api_key = setup$api_key,
        base_url = setup$base_url, max_active = max_active, rpm = rpm,
        on_error = on_error
      ),
      chat = transcribe_chat(
        prepared$path[ready], chat = setup$chat,
        max_active = max_active, rpm = rpm, on_error = on_error
      )
    )
  }

  result <- assemble_transcript(
    ids = ids, x = x, is_url = is_url, prepared = prepared,
    outcomes = outcomes, model = model, language = language, prompt = prompt,
    base_url = setup$recorded_base_url
  )
  report_transcription_failures(result, backend)
  result
}


# Naming and validation --------------------------------------------------------

#' Is an element a web address rather than a path?
#' @keywords internal
#' @noRd
is_http_url <- function(x) {
  grepl("^https?://", x, ignore.case = TRUE)
}

#' The identifiers a transcript vector will carry
#'
#' A supplied name is kept; an unnamed file takes its basename, as readtext
#' does; an unnamed URL takes `text<position>`, as quanteda does. Decided
#' from the input as given, before anything runs.
#'
#' @keywords internal
#' @noRd
transcript_ids <- function(x, is_url, call = rlang::caller_env()) {
  ids <- names(x) %||% rep("", length(x))
  if (anyNA(ids)) {
    cli::cli_abort(c(
      "The names of {.arg x} must not be {.code NA}: {sum(is.na(ids))} {?is/are}.",
      "i" = "Leave a name empty to have it filled in, or supply one."
    ), call = call)
  }
  blank <- !nzchar(ids)
  filled <- ifelse(is_url, paste0("text", seq_along(x)), basename(x))
  ids[blank] <- filled[blank]

  dups <- unique(ids[duplicated(ids)])
  if (length(dups)) {
    from_basename <- dups %in% ids[blank]
    cli::cli_abort(c(
      "The names of {.arg x} must be unique: {length(dups)} value{?s} occur{?s/} more than once.",
      "x" = "Duplicated: {.val {utils::head(dups, 5)}}{if (length(dups) > 5) ', ...' else ''}",
      "i" = if (any(from_basename)) {
        "An unnamed file is named by its basename; files in different directories that share one need names supplied: {.code names(x) <- ...}."
      } else {
        "The names become the {.field .id} of each transcript when it is coded, and a repeated value would pair the wrong rows."
      }
    ), call = call)
  }
  check_ids(ids, what = "The names of {.arg x}", call = call)
  ids
}

#' Which route a model string takes
#'
#' `openai/` and every provider registered with [qlm_register_provider()]
#' have a transcription endpoint; every other provider ellmer reaches gets
#' the recording through a chat. Nothing here says what a model can do.
#'
#' @keywords internal
#' @noRd
transcription_backend <- function(model) {
  provider <- model_provider(model)
  if (identical(provider, "openai") || !provider %in% ellmer_providers()) {
    "endpoint"
  } else {
    "chat"
  }
}

#' The model part of a `"provider/model"` string
#' @keywords internal
#' @noRd
model_id <- function(model) {
  sub("^[^/]*/", "", model)
}

#' The file formats each route accepts, by extension
#'
#' The endpoint's list is OpenAI's documented one. The chat route's is
#' ellmer's MIME table, the same list `qlm_code()` checks for audio input,
#' since that is what the upload can label.
#'
#' @keywords internal
#' @noRd
transcription_extensions <- function(backend) {
  switch(backend,
    endpoint = c("flac", "mp3", "mp4", "mpeg", "mpga", "m4a", "ogg", "wav", "webm"),
    chat = audio_extensions()
  )
}

#' The size limit each route enforces, in bytes; `NA` for none checked here
#' @keywords internal
#' @noRd
transcription_size_limit <- function(backend) {
  switch(backend, endpoint = 25 * 1024^2, chat = NA_real_)
}

#' The file extension of a path or of a URL's path
#' @keywords internal
#' @noRd
source_extension <- function(x, is_url) {
  path <- ifelse(is_url, sub("[?#].*$", "", x), x)
  tolower(tools::file_ext(path))
}

#' The basename a source is recorded under
#' @keywords internal
#' @noRd
source_basename <- function(x, is_url) {
  basename(ifelse(is_url, sub("[?#].*$", "", x), x))
}

#' What a source is recorded as: a basename, or the URL without credentials
#' @keywords internal
#' @noRd
source_label <- function(x, is_url) {
  ifelse(is_url, vapply(x, redact_url, character(1), USE.NAMES = FALSE), basename(x))
}

#' Refuse what the backend could not take, before anything is fetched or sent
#'
#' Every problem of a kind is reported at once. The format is read from the
#' extension, for a URL from its path, since that is what the endpoint reads
#' too. A local file over the size limit is refused here; a downloaded one is
#' checked after the download.
#'
#' @keywords internal
#' @noRd
check_transcription_sources <- function(x, is_url, backend, call = rlang::caller_env()) {
  paths <- x[!is_url]
  missing <- is.na(paths) | !file.exists(paths) | dir.exists(paths)
  if (any(missing)) {
    cli::cli_abort(c(
      "{sum(missing)} audio {cli::qty(sum(missing))}file{?s} {?does/do} not exist: {.path {paths[missing]}}.",
      "i" = "Every element of {.arg x} must be the path of an existing file or an {.code http(s)://} URL."
    ), call = call)
  }

  accepted <- transcription_extensions(backend)
  ext <- source_extension(x, is_url)
  unknown <- !ext %in% accepted
  if (any(unknown)) {
    cli::cli_abort(c(
      "{sum(unknown)} {cli::qty(sum(unknown))}element{?s} of {.arg x} {?has/have} a format this route does not accept: {.path {x[unknown]}}.",
      "i" = "The format is read from the file extension, for a URL from its path.",
      "i" = "Accepted: {.val {accepted}}."
    ), call = call)
  }

  limit <- transcription_size_limit(backend)
  if (!is.na(limit) && length(paths)) {
    over <- file.size(paths) > limit
    if (any(over)) {
      cli::cli_abort(c(
        "{sum(over)} {cli::qty(sum(over))}file{?s} {?is/are} over the {format_bytes(limit)} limit of this endpoint: {.path {paths[over]}}.",
        "i" = "Split a long recording before transcribing it."
      ), call = call)
    }
  }
  invisible(x)
}

format_bytes <- function(n) {
  paste0(format(round(n / 1024^2), big.mark = ","), " MB")
}

#' `NULL`, or a single non-missing string
#' @keywords internal
#' @noRd
check_optional_string <- function(value, arg, call = rlang::caller_env()) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort("{.arg {arg}} must be a single string, or {.code NULL}.", call = call)
  }
  invisible(NULL)
}


# Fetching ---------------------------------------------------------------------

#' Every file as a local path, with the downloads done and the bytes recorded
#'
#' A URL is fetched into `workdir` under its own basename, prefixed by its
#' position so two URLs with the same basename cannot collide. A download
#' that fails, or that fetched more than the backend takes, is a failure of
#' that unit under `"continue"` and `"return"`, and aborts under `"stop"`.
#'
#' The size and hash are taken here, before any request, so the record is
#' of the bytes about to be sent: a file replaced while requests run cannot
#' be recorded in their place, and one deleted then cannot stop the results
#' coming back.
#'
#' @return A list: `path`, one per element (`NA` where a download failed),
#'   `error`, the message or `NA`, and `size` and `sha256` of each file
#'   that is there.
#' @keywords internal
#' @noRd
prepare_transcription_files <- function(x, is_url, workdir, backend, on_error,
                                        call = rlang::caller_env()) {
  n <- length(x)
  path <- ifelse(is_url, NA_character_, x)
  error <- rep(NA_character_, n)
  limit <- transcription_size_limit(backend)

  for (i in which(is_url)) {
    dest <- file.path(workdir, paste0(sprintf("%03d_", i), source_basename(x[[i]], TRUE)))
    outcome <- tryCatch(
      {
        download_audio(x[[i]], dest)
        if (!is.na(limit) && file.size(dest) > limit) {
          stop(paste0("the download is over the ", format_bytes(limit), " limit of this endpoint"), call. = FALSE)
        }
        NULL
      },
      error = function(e) strip_ansi(conditionMessage(e))
    )
    if (is.null(outcome)) {
      path[[i]] <- dest
    } else {
      if (on_error == "stop") {
        cli::cli_abort(c(
          "Downloading {.url {x[[i]]}} failed.",
          "x" = outcome
        ), call = call)
      }
      error[[i]] <- paste0("download failed: ", outcome)
    }
  }

  fetched <- !is.na(path)
  size <- rep(NA_real_, n)
  sha256 <- rep(NA_character_, n)
  size[fetched] <- as.numeric(file.size(path[fetched]))
  sha256[fetched] <- vapply(path[fetched], hash_file, character(1), USE.NAMES = FALSE)
  list(path = path, error = error, size = size, sha256 = sha256)
}

#' One download, kept apart so that tests can stand in for the network
#' @keywords internal
#' @noRd
download_audio <- function(url, dest) {
  curl::curl_download(url, dest, quiet = TRUE, mode = "wb")
  invisible(dest)
}


# The object -------------------------------------------------------------------

#' Build the transcript vector and its provenance from the outcomes
#'
#' @param outcomes One entry per element: `NULL` for a unit never submitted,
#'   otherwise a list with `text`, `usage` and `error`.
#' @keywords internal
#' @noRd
assemble_transcript <- function(ids, x, is_url, prepared, outcomes, model,
                                language, prompt, base_url) {
  n <- length(x)
  text <- rep(NA_character_, n)
  status <- rep("unsubmitted", n)
  error <- prepared$error
  usage <- vector("list", n)
  timestamp <- rep(as.POSIXct(NA), n)
  now <- Sys.time()

  for (i in seq_len(n)) {
    out <- outcomes[[i]]
    if (!is.na(error[[i]])) {
      status[[i]] <- "failed"
      timestamp[[i]] <- now
    } else if (!is.null(out)) {
      usage[i] <- list(out$usage)
      timestamp[[i]] <- now
      if (is.na(out$error)) {
        status[[i]] <- "ok"
        text[[i]] <- out$text
      } else {
        status[[i]] <- "failed"
        error[[i]] <- out$error
      }
    } else {
      error[[i]] <- "not submitted: an earlier transcription failed and on_error is \"return\""
    }
  }

  provenance <- data.frame(
    .id = ids,
    status = status,
    source = source_label(x, is_url),
    .error = error,
    size = prepared$size,
    sha256 = prepared$sha256,
    model = rep(model, n),
    language = rep(language %||% NA_character_, n),
    prompt = rep(prompt %||% NA_character_, n),
    base_url = rep(base_url %||% NA_character_, n),
    timestamp = timestamp,
    stringsAsFactors = FALSE
  )
  provenance$usage <- usage
  new_qlm_transcript(text, provenance)
}

#' The constructor: text and table of the same length, names from the table
#' @keywords internal
#' @noRd
new_qlm_transcript <- function(text, provenance) {
  if (length(text) != nrow(provenance)) {
    cli::cli_abort(
      "A transcript has {length(text)} element{?s} but its provenance {nrow(provenance)} row{?s}.",
      .internal = TRUE
    )
  }
  rownames(provenance) <- NULL
  text <- as.vector(text, "character")
  names(text) <- provenance$.id
  structure(text, provenance = provenance, class = c("qlm_transcript", "character"))
}

#' Say how a run of transcriptions went, once, as qlm_code() does
#' @keywords internal
#' @noRd
report_transcription_failures <- function(x, backend = "endpoint") {
  prov <- attr(x, "provenance")
  failed <- prov$status == "failed"
  unsubmitted <- prov$status == "unsubmitted"
  if (!any(failed)) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{sum(failed)} of {nrow(prov)} transcription{?s} failed.",
    set_bullets(unique(prov$.error[failed])),
    if (any(unsubmitted)) c("i" = "{sum(unsubmitted)} file{?s} {?was/were} not submitted after the failure."),
    if (identical(backend, "chat")) c("i" = "A chat model must be able to hear an uploaded recording; Google Gemini's models are known to. See {.code ?qlm_transcribe}."),
    "i" = "{cli::qty(sum(failed))}{?Its/Their} transcript{?s} {?is/are} {.code NA}; {.code attr(x, \"provenance\")} has the reason for each."
  ))
  invisible(NULL)
}


# Methods ----------------------------------------------------------------------

#' Subset a transcript vector
#'
#' The provenance rows follow the elements, by position, so a selection or a
#' reordering keeps each transcript with its own record. As for a coded
#' object, a subset that would repeat an identifier is refused.
#'
#' @param x A `qlm_transcript`.
#' @param i An index: positions, names or a logical vector.
#' @param ... Ignored.
#'
#' @return A `qlm_transcript`.
#' @keywords internal
#' @export
`[.qlm_transcript` <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }
  pos <- stats::setNames(seq_along(x), names(x))[i]
  text <- as.character(x)[i]
  provenance <- attr(x, "provenance")[pos, , drop = FALSE]
  check_ids(names(text), what = "The names of the subset")
  new_qlm_transcript(text, provenance)
}

#' Replace transcripts, and their provenance with them
#'
#' Assigning a `qlm_transcript` puts its rows in place of the old ones, so a
#' retried transcription replaces the failure it retries, record and all.
#' Assigning plain text keeps the file and its hash, since the recording is
#' the same, and records that the text was edited: `status` becomes
#' `"edited"`, the usage is cleared and the time is now. Assigning `NA`
#' records a failure set by hand. The vector cannot be extended.
#'
#' @param x A `qlm_transcript`.
#' @param i An index.
#' @param value A `qlm_transcript`, or replacement text.
#'
#' @return A `qlm_transcript`.
#' @keywords internal
#' @export
`[<-.qlm_transcript` <- function(x, i, value) {
  provenance <- attr(x, "provenance")
  text <- as.character(x)
  pos <- if (missing(i)) seq_along(x) else stats::setNames(seq_along(x), names(x))[i]
  if (anyNA(pos) || any(pos > length(x))) {
    cli::cli_abort(c(
      "A {.cls qlm_transcript} cannot be extended by assignment.",
      "i" = "Every element carries a provenance row; add transcripts with {.code c()} instead."
    ))
  }
  if (!length(pos)) {
    return(x)
  }
  if (length(value) != 1L && length(value) != length(pos)) {
    cli::cli_abort(
      "{.arg value} must have one element, or one per position replaced: {length(value)} for {length(pos)}."
    )
  }
  take <- rep_len(seq_along(value), length(pos))

  if (inherits(value, "qlm_transcript")) {
    rows <- attr(value, "provenance")[take, , drop = FALSE]
    # The position keeps its name: that is the unit's identity here
    rows$.id <- provenance$.id[pos]
    provenance[pos, ] <- rows
    text[pos] <- as.character(value)[take]
  } else {
    value <- as.vector(value, "character")[take]
    text[pos] <- value
    provenance$status[pos] <- ifelse(is.na(value), "failed", "edited")
    provenance$.error[pos] <- ifelse(is.na(value), "set to NA by assignment", NA_character_)
    provenance$timestamp[pos] <- Sys.time()
    provenance$usage[pos] <- list(NULL)
  }
  new_qlm_transcript(text, provenance)
}

#' Replace one transcript, by the same rules as `[<-`
#'
#' @param x A `qlm_transcript`.
#' @param i A single position or name.
#' @param value A `qlm_transcript` of length one, or one string.
#'
#' @return A `qlm_transcript`.
#' @keywords internal
#' @export
`[[<-.qlm_transcript` <- function(x, i, value) {
  if (length(i) != 1L) {
    cli::cli_abort("{.code [[<-} replaces one transcript; {.arg i} has length {length(i)}.")
  }
  if (length(value) != 1L) {
    cli::cli_abort("{.code [[<-} replaces one transcript; {.arg value} has length {length(value)}.")
  }
  x[i] <- value
  x
}

#' Rename transcripts, keeping their provenance in step
#'
#' @param x A `qlm_transcript`.
#' @param value The new names, one per element, unique and non-empty.
#'
#' @return A `qlm_transcript`.
#' @keywords internal
#' @export
`names<-.qlm_transcript` <- function(x, value) {
  if (length(value) != length(x)) {
    cli::cli_abort(c(
      "{.arg value} must have one name per transcript: {length(value)} for {length(x)} element{?s}.",
      "i" = "A transcript cannot be unnamed; its name is its {.field .id} when it is coded."
    ))
  }
  check_ids(value, what = "The names of a {.cls qlm_transcript}")
  provenance <- attr(x, "provenance")
  provenance$.id <- as.character(value)
  new_qlm_transcript(as.character(x), provenance)
}

#' Combine transcript vectors
#'
#' Joins independent runs, such as two batches or two models, into one
#' vector, with the provenance rows of each; the names must not collide.
#'
#' @param ... `qlm_transcript` objects.
#'
#' @return A `qlm_transcript`.
#' @keywords internal
#' @export
c.qlm_transcript <- function(...) {
  parts <- list(...)
  is_transcript <- vapply(parts, inherits, logical(1), "qlm_transcript")
  if (!all(is_transcript)) {
    bad <- which(!is_transcript)[1]
    cli::cli_abort(c(
      "Every argument must be a {.cls qlm_transcript}; argument {bad} is {.cls {class(parts[[bad]])}}.",
      "i" = "Use {.code as.character()} on the transcripts to combine them with plain text, dropping their provenance."
    ))
  }
  text <- unlist(unname(lapply(parts, as.character)), use.names = TRUE)
  provenance <- do.call(rbind, unname(lapply(parts, attr, "provenance")))
  check_ids(names(text), what = "The names of the combined transcripts")
  new_qlm_transcript(text, provenance)
}

#' Drop the class and provenance, keeping the names
#'
#' @param x A `qlm_transcript`.
#' @param ... Ignored.
#'
#' @return A named character vector.
#' @keywords internal
#' @export
as.character.qlm_transcript <- function(x, ...) {
  out <- unclass(x)
  attr(out, "provenance") <- NULL
  out
}

#' Print a transcript vector
#'
#' @param x A `qlm_transcript`.
#' @param n integer; how many transcripts to show.
#' @param width integer; the line width to fit each to.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.qlm_transcript <- function(x, n = 10, width = getOption("width", 80), ...) {
  prov <- attr(x, "provenance")
  total <- length(x)
  failed <- sum(prov$status %in% c("failed", "unsubmitted"))
  models <- unique(prov$model)
  cat(sprintf(
    "<qlm_transcript: %d transcript%s%s; model%s %s>\n",
    total, if (total == 1L) "" else "s",
    if (failed) sprintf(", %d failed", failed) else "",
    if (length(models) == 1L) "" else "s",
    paste(models, collapse = ", ")
  ))
  shown <- seq_len(min(n, total))
  for (i in shown) {
    prefix <- paste0(names(x)[i], ": ")
    body <- if (is.na(x[[i]])) {
      paste0("<NA: ", prov$.error[i], ">")
    } else {
      sub("\n.*$", "", x[[i]])
    }
    room <- max(width - nchar(prefix), 10L)
    if (nchar(body) > room) {
      body <- paste0(substr(body, 1L, room - 3L), "...")
    }
    cat(prefix, body, "\n", sep = "")
  }
  if (total > length(shown)) {
    cat(sprintf("# ... with %d more\n", total - length(shown)))
  }
  invisible(x)
}


# What qlm_code() records ------------------------------------------------------

#' The provenance a coded run records for a transcript input
#'
#' The `.id` column is reset to the current names, which are what the run
#' keys on, in case the vector was renamed after transcription.
#'
#' @keywords internal
#' @noRd
transcription_record <- function(x) {
  provenance <- attr(x, "provenance")
  provenance$.id <- as.character(names(x))
  rownames(provenance) <- NULL
  provenance
}

#' The reason a text unit has nothing to code, per unit
#'
#' A transcript that failed carries its reason; any other missing text is
#' said to be missing.
#'
#' @keywords internal
#' @noRd
absent_input_reasons <- function(x, transcription = NULL) {
  reasons <- rep("the input text is NA", length(x))
  if (!is.null(transcription)) {
    failed <- !is.na(transcription$.error)
    reasons[failed] <- paste0("no transcript: ", transcription$.error[failed])
  }
  reasons
}

#' The error recorded on a unit whose text was never there to send
#'
#' Classed so that a backfill knows nothing can be retried for it.
#'
#' @keywords internal
#' @noRd
absent_input_error <- function(message) {
  structure(
    simpleError(message),
    class = c("quallmer_absent_input", "simpleError", "error", "condition")
  )
}

is_absent_input <- function(e) {
  inherits(e, "quallmer_absent_input")
}

#' Put the rows of units that were never sent back among the coded ones
#'
#' @param results The table for the units that were sent.
#' @param absent Which units of the full input were not.
#' @param reasons Their reasons, one per absent unit.
#'
#' @return A table with one row per unit of the full input.
#' @keywords internal
#' @noRd
restore_absent_rows <- function(results, absent, reasons) {
  n <- length(absent)
  if (!".error" %in% names(results)) {
    results$.error <- vector("list", nrow(results))
  }
  full <- vctrs::vec_init(results, n)
  full <- vctrs::vec_assign(full, which(!absent), results)
  full$.error[absent] <- lapply(reasons, absent_input_error)
  full
}
