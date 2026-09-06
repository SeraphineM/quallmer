#' Input types a codebook can declare
#'
#' One place for the values `input_type` may take, so that [qlm_codebook()],
#' [qlm_code()] and the documentation agree. `"text"` sends each element as a
#' string; `"image"` sends each file inline; `"audio"` and `"video"` upload
#' each file to the provider and send a reference to it. See
#' `as_input_content()` for the dispatch.
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
input_types <- function() {
  c("text", "image", "audio", "video")
}

#' The input types whose elements are file paths (or URLs)
#' @keywords internal
#' @noRd
file_input_types <- function() {
  c("image", "audio", "video")
}

#' The input types whose files are uploaded to the provider
#'
#' An upload gets a new reference every time, which is what rules out
#' `batch = TRUE` for these types, and what makes a replication upload the
#' files again.
#'
#' @keywords internal
#' @noRd
uploaded_input_types <- function() {
  c("audio", "video")
}

#' Audio file extensions ellmer knows the MIME type of
#'
#' Mirrors ellmer's own table, which is what its upload uses to label the
#' file; a file it cannot label is refused by the upload anyway, so it is
#' refused here first, before anything is sent.
#'
#' @keywords internal
#' @noRd
audio_extensions <- function() {
  c("mp3", "wav", "ogg", "m4a", "flac", "aac")
}

#' Video file extensions ellmer labels and Gemini lists as accepted
#'
#' The intersection of the two, as of 2026-09-06. ellmer also labels `mkv`,
#' which Gemini does not list, and Gemini also accepts `mpeg`, `mpg`, `flv`
#' and `3gp`, which ellmer cannot label without being told the MIME type.
#'
#' @keywords internal
#' @noRd
video_extensions <- function() {
  c("mp4", "mov", "avi", "wmv", "webm")
}

#' The file extensions a file input type accepts, or `NULL` for no check
#' @keywords internal
#' @noRd
input_extensions <- function(input_type) {
  switch(input_type,
    audio = audio_extensions(),
    video = video_extensions(),
    NULL
  )
}

#' The largest file the provider's upload takes, in bytes
#'
#' Gemini's Files API: 2 GB per file. Audio has never come near it; video
#' can, so it is checked before an upload that would fail after the bytes
#' were sent.
#'
#' @keywords internal
#' @noRd
max_upload_bytes <- function() {
  2 * 1024^3
}


# URLs -------------------------------------------------------------------------

#' Is an element of `x` an http(s) URL rather than a file path?
#'
#' A scheme means a URL, and everything else is a path, so a URL typed
#' without its scheme fails the existence check with its value named. With
#' `data = TRUE` a `data:` URI counts too, which is the rule ellmer applies
#' inside [ellmer::content_image_url()] and the one image inputs use.
#'
#' @param x A character vector.
#' @param data Whether a `data:` URI counts as a URL.
#' @return A logical vector.
#' @keywords internal
#' @noRd
is_input_url <- function(x, data = FALSE) {
  pattern <- if (data) "^(https?:|data:)" else "^https?://"
  !is.na(x) & grepl(pattern, x)
}

#' Is an element of `x` a YouTube link?
#'
#' Gemini fetches a public YouTube video by its URL, so such a link is sent
#' as a reference and never passes through this machine. Recognised forms:
#' `youtube.com/watch?v=`, `youtu.be/`, `youtube.com/shorts/` and
#' `youtube.com/live/`, with or without `www.` or `m.`.
#'
#' @keywords internal
#' @noRd
is_youtube_url <- function(x) {
  !is.na(x) & grepl(
    "^https?://(www\\.|m\\.)?(youtube\\.com/(watch\\?|shorts/|live/)|youtu\\.be/)",
    x
  )
}

#' The file extension of a URL's path, ignoring any query or fragment
#' @keywords internal
#' @noRd
url_extension <- function(url) {
  tolower(tools::file_ext(sub("[?#].*$", "", url)))
}

#' `redact_url()` over a vector
#' @keywords internal
#' @noRd
redact_urls <- function(x) {
  vapply(x, redact_url, character(1), USE.NAMES = FALSE)
}

#' Redact every URL that appears in a piece of text
#'
#' A download failure quotes the URL it was given, credentials included, so
#' the provider's or libcurl's message is passed through this before it is
#' kept or shown.
#'
#' @keywords internal
#' @noRd
redact_urls_in_text <- function(text) {
  found <- gregexpr("[A-Za-z][A-Za-z0-9+.-]*://[^[:space:]'\"<>]+", text)
  regmatches(text, found) <- lapply(regmatches(text, found), redact_urls)
  text
}


# Checks before anything is downloaded, uploaded or sent -----------------------

#' Check file inputs before any download, upload or request
#'
#' Each element must be the path of an existing file or, for images and
#' video, a URL. Audio and video paths must carry an extension the upload can
#' label; a video URL that is not a YouTube link must too, since it is
#' downloaded and uploaded like a file. Checked in one pass so that every
#' problem is reported at once.
#'
#' @param x The input vector.
#' @param input_type One of `file_input_types()`.
#' @param call The calling environment, for the error.
#'
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_file_inputs <- function(x, input_type, call = rlang::caller_env()) {
  urls_ok <- input_type %in% c("image", "video")
  if (!is.character(x)) {
    cli::cli_abort(
      "This codebook expects {input_type} file paths{if (urls_ok) ' or URLs' else ''} (a character vector).",
      call = call
    )
  }

  # A data: URI carries an image inline; there is no such thing for video
  if (input_type == "video") {
    data_uri <- !is.na(x) & grepl("^data:", x)
    if (any(data_uri)) {
      cli::cli_abort(c(
        "{sum(data_uri)} element{?s} of {.arg x} {?is a/are} {.code data:} URI{?s}, which video input does not accept.",
        "i" = "Give each video as a file path, a YouTube link or an {.code http(s)://} URL."
      ), call = call)
    }
  }

  # An image may be given as a URL, which the provider fetches (#177); a
  # video may be given as a YouTube link, which the provider fetches, or as
  # a URL that is downloaded here (#179). A URL is recognised by its scheme
  # and is not a file here.
  is_url <- switch(input_type,
    image = is_input_url(x, data = TRUE),
    video = is_input_url(x),
    rep(FALSE, length(x))
  )
  missing <- !is_url & (is.na(x) | !file.exists(x) | dir.exists(x))
  if (any(missing)) {
    hint <- if (urls_ok) {
      c(
        "i" = "Every element of {.arg x} must be the path of an existing file or a URL.",
        "i" = if (input_type == "image") {
          "A URL is recognised by its scheme: {.code http://}, {.code https://} or {.code data:}."
        } else {
          "A URL is recognised by its scheme: {.code http://} or {.code https://}."
        }
      )
    } else {
      c("i" = "Every element of {.arg x} must be the path of an existing file.")
    }
    cli::cli_abort(c(
      "{sum(missing)} {input_type} {cli::qty(sum(missing))}file{?s} {?does/do} not exist: {.path {x[missing]}}.",
      hint
    ), call = call)
  }

  extensions <- input_extensions(input_type)
  if (!is.null(extensions)) {
    ext <- ifelse(is_url, url_extension(x), tolower(tools::file_ext(x)))
    # A YouTube link is fetched by the provider, so it has no file to label
    checked <- !is_youtube_url(x)
    unknown <- checked & !ext %in% extensions
    if (any(unknown)) {
      cli::cli_abort(c(
        "{sum(unknown)} {cli::qty(sum(unknown))}{if (any(unknown & is_url)) 'file or URL' else 'file'}{?s} {?has/have} an extension the upload cannot label: {.path {x[unknown]}}.",
        "i" = "{cli::qty(2)}{stringr_title(input_type)} files must be one of {.val {extensions}}.",
        if (input_type == "video") c(
          "i" = "A video URL other than a YouTube link is downloaded and uploaded like a file, so its path must end in one of these extensions too."
        )
      ), call = call)
    }
  }
  invisible(x)
}

#' Capitalise the first letter, for a message
#' @keywords internal
#' @noRd
stringr_title <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


#' What is known to accept a file input type, said when a provider refuses
#'
#' There is no gate in advance: a provider is tried, and if it cannot take
#' the input its own refusal is what the run reports (decided 2026-09-06,
#' #179). What quallmer knows about who accepts the type is a snapshot, so
#' it is offered as a hint alongside that refusal, where it can help, rather
#' than as a rule that would refuse a newer model.
#'
#' @param input_type The codebook's input type.
#'
#' @return A named character vector for a cli bullet, or `NULL` for a type
#'   every provider accepts.
#' @keywords internal
#' @noRd
known_input_providers_hint <- function(input_type) {
  if (!input_type %in% uploaded_input_types()) {
    return(NULL)
  }
  version <- tryCatch(
    as.character(utils::packageVersion("quallmer")),
    error = function(e) "this version"
  )
  c("i" = paste0(
    "As of quallmer ", version, ", only Google Gemini ({.code google_gemini/}) ",
    "is known to accept ", input_type, " input; see the \"",
    stringr_title(input_type), " input\" section of {.code ?qlm_code}."
  ))
}


# Resolving URLs to local files ------------------------------------------------

#' Bring every element that has bytes onto this machine
#'
#' A path is already here. A YouTube link never is: the provider fetches it.
#' Any other URL is downloaded to a temporary file, so that it can be hashed
#' and uploaded exactly as a file would be. Downloads are all-or-nothing, in
#' the style of `upload_inputs()`: a failure aborts before anything is
#' uploaded, with the URL and the reason. Only video has URLs to download;
#' for audio every element is a path, and for images a URL is fetched by the
#' provider.
#'
#' After the downloads, video files are checked against the upload's size
#' limit and their total is said once, with a token estimate when the
#' durations can be read.
#'
#' @param x The input vector, already checked.
#' @param input_type One of `file_input_types()`.
#' @param ids The `.id` of each element, for the hash check a replication
#'   or backfill asks for; two units may share a URL and differ in bytes.
#' @param say Whether to say what is about to be uploaded.
#' @param call The calling environment, for the error.
#'
#' @return A list: `local`, a character vector with the local path of each
#'   element or `NA` for one the provider fetches; `temp`, the paths of the
#'   temporary downloads, for the caller to remove.
#' @keywords internal
#' @noRd
resolve_input_files <- function(x, input_type, ids = names(x) %||% seq_along(x),
                                say = TRUE, call = rlang::caller_env()) {
  local <- unname(x)
  temp <- character()

  if (input_type == "image") {
    local[is_input_url(x, data = TRUE)] <- NA_character_
    return(list(local = local, temp = temp))
  }
  if (input_type != "video") {
    return(list(local = local, temp = temp))
  }

  local[is_youtube_url(x)] <- NA_character_
  download <- which(is_input_url(x) & !is_youtube_url(x))
  # The downloads are this function's until it returns them; an abort in
  # any check below must not leave them behind
  done <- FALSE
  on.exit(if (!done) unlink(temp), add = TRUE)
  if (length(download)) {
    temp <- vapply(x[download], function(url) {
      tempfile("quallmer_video_", fileext = paste0(".", url_extension(url)))
    }, character(1), USE.NAMES = FALSE)
    failures <- character()

    cli::cli_progress_bar("Downloading video files", total = length(download))
    for (i in seq_along(download)) {
      url <- x[[download[[i]]]]
      result <- tryCatch(
        download_input_url(url, temp[[i]]),
        error = function(e) e,
        warning = function(w) w
      )
      if (inherits(result, "condition")) {
        failures[redact_url(url)] <- redact_urls_in_text(strip_ansi(conditionMessage(result)))
      }
      cli::cli_progress_update()
    }
    cli::cli_progress_done()

    if (length(failures)) {
      cli::cli_abort(c(
        "Downloading {length(failures)} of {length(download)} video URL{?s} failed, so nothing was uploaded or sent.",
        stats::setNames(
          paste0(names(failures), ": ", failures),
          rep("x", length(failures))
        ),
        "i" = "The provider fetches YouTube links itself; any other URL is downloaded here first."
      ), call = call)
    }
    local[download] <- temp
    check_downloaded_hashes(x[download], temp, as.character(ids)[download], call = call)
  }

  check_upload_sizes(local, x, input_type, call = call)
  if (say) {
    say_video_size(local)
  }
  done <- TRUE
  list(local = local, temp = temp)
}

# What a replication or backfill expects a URL to hold ---------------------------

# Set for the duration of a replication or backfill pass, read when the
# URLs are downloaded for upload
input_state <- new.env(parent = emptyenv())
input_state$expected <- NULL

#' The hashes a run recorded for the URLs it downloaded
#'
#' A URL is downloaded once, by the run that uploads it, and the bytes that
#' arrive are the bytes the model receives. So a replication or backfill
#' cannot check a URL ahead of that download, as it checks a local file;
#' instead it hands the parent's recorded hashes to the run, and
#' `resolve_input_files()` compares the download against them before
#' anything is uploaded. Checking a separate download first would prove
#' nothing about the one that is sent.
#'
#' @param x A `qlm_coded` object.
#' @param ids The units about to be coded, or `NULL` for all of them.
#'
#' @return A data frame with `url`, `.id` and `sha256`, one row per URL unit
#'   with a recorded hash; zero rows when there is none.
#' @keywords internal
#' @noRd
expected_url_hashes <- function(x, ids = NULL) {
  none <- data.frame(url = character(), .id = character(), sha256 = character(),
                     stringsAsFactors = FALSE)
  meta_attr <- attr(x, "meta")
  recorded <- meta_attr$user$input_files
  if (!meta_attr$object$input_type %in% uploaded_input_types() || is.null(recorded)) {
    return(none)
  }
  data <- inputs(x)
  all_ids <- as.character(names(data) %||% seq_along(data))
  if (!is.null(ids)) {
    keep <- all_ids %in% as.character(ids)
    data <- data[keep]
    all_ids <- all_ids[keep]
  }
  data <- unname(data)
  sha256 <- recorded$sha256[match(all_ids, recorded$.id)]
  is_url <- is_input_url(data) & !is_youtube_url(data)
  keep <- is_url & !is.na(sha256)
  data.frame(url = data[keep], .id = all_ids[keep], sha256 = sha256[keep],
             stringsAsFactors = FALSE)
}

#' Run `code` with these expectations in force
#' @keywords internal
#' @noRd
with_expected_hashes <- function(expected, code) {
  old <- input_state$expected
  on.exit(input_state$expected <- old, add = TRUE)
  input_state$expected <- if (nrow(expected)) expected
  force(code)
}

#' Refuse a download whose bytes are not the ones the parent run coded
#'
#' Matched on unit and URL together: two units may share a URL and have
#' recorded different bytes, and each is held to its own record.
#'
#' @param urls The URLs just downloaded.
#' @param paths Where each landed.
#' @param ids The `.id` of each.
#' @param call The calling environment, for the error.
#' @keywords internal
#' @noRd
check_downloaded_hashes <- function(urls, paths, ids, call = rlang::caller_env()) {
  expected <- input_state$expected
  if (is.null(expected)) {
    return(invisible(NULL))
  }
  pos <- match(paste(ids, urls), paste(expected$.id, expected$url))
  known <- !is.na(pos)
  if (!any(known)) {
    return(invisible(NULL))
  }
  current <- vapply(paths[known], hash_file, character(1), USE.NAMES = FALSE)
  changed <- current != expected$sha256[pos[known]]
  if (any(changed)) {
    ids <- expected$.id[pos[known]][changed]
    cli::cli_abort(c(
      "{cli::qty(sum(changed))}The video file{?s} of {sum(changed)} unit{?s} differ{?s/} from the one{?s} this run coded: {.val {ids}}.",
      "i" = "The recorded hash is from when the run was coded; the URL now serves different bytes.",
      "i" = "Code the current files as a fresh run with {.fn qlm_code} instead."
    ), call = call)
  }
  invisible(NULL)
}


#' One download, kept as its own function so that tests can stand in for
#' the network
#'
#' `download.file()` signals a warning as well as an error when the server
#' refuses, and its caller treats either as failure.
#'
#' @keywords internal
#' @noRd
download_input_url <- function(url, dest) {
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}

#' Refuse a file the upload would refuse after sending it
#' @keywords internal
#' @noRd
check_upload_sizes <- function(local, x, input_type, call = rlang::caller_env()) {
  here <- !is.na(local)
  size <- rep(NA_real_, length(local))
  size[here] <- as.numeric(file.size(local[here]))
  over <- here & size > max_upload_bytes()
  if (any(over)) {
    cli::cli_abort(c(
      "{sum(over)} {input_type} file{?s} exceed{?s/} the {format_bytes(max_upload_bytes())} the provider's upload accepts: {.path {x[over]}}.",
      "i" = "{cli::qty(sum(over))}Trim or re-encode {?it/them} before coding."
    ), call = call)
  }
  invisible(NULL)
}

#' Say what is about to be uploaded, once, before it is
#'
#' Video is priced by the second, at about 300 input tokens per second at
#' the provider's default resolution, so the durations are what the cost
#' turns on. They can be read only with the \pkg{av} package, which is
#' optional; without it the total size is said instead, as it is for a file
#' whose duration cannot be read. A YouTube link has neither here. This is
#' a notice: nothing in it may stop the run.
#'
#' @keywords internal
#' @noRd
say_video_size <- function(local) {
  here <- local[!is.na(local)]
  if (!length(here)) {
    return(invisible(NULL))
  }
  bytes <- sum(file.size(here))
  n <- length(here)
  seconds <- if (has_av()) vapply(here, video_duration, numeric(1)) else rep(NA_real_, n)
  known <- !is.na(seconds)
  if (any(known)) {
    total <- sum(seconds[known])
    unread <- sum(!known)
    cli::cli_inform(c("i" = paste0(
      "Uploading {n} video file{?s}, {format_seconds(total)} in all ",
      "({format_bytes(bytes)}); at the provider's default sampling that is ",
      "roughly {format(round(total * 300), big.mark = ',')} input tokens",
      if (unread) ", not counting {unread} file{?s} whose duration could not be read" else "",
      "."
    )))
  } else {
    cli::cli_inform(c("i" = paste0(
      "Uploading {n} video file{?s}, {format_bytes(bytes)} in all. Video costs ",
      "roughly 300 input tokens per second at the provider's default sampling; ",
      if (has_av()) "the durations could not be read." else
        "install the {.pkg av} package to see the durations before uploading."
    )))
  }
  invisible(NULL)
}

#' Whether durations can be read, and one duration; both replaceable in tests
#'
#' A file av cannot open (a wrong container, a truncated download) gives
#' `NA` rather than an error: the duration is for a notice, and the upload
#' is where the provider's verdict on the file belongs.
#'
#' @keywords internal
#' @noRd
has_av <- function() {
  requireNamespace("av", quietly = TRUE)
}

video_duration <- function(path) {
  tryCatch(
    {
      info <- av::av_media_info(path)
      as.numeric(info$duration %||% NA_real_)
    },
    error = function(e) NA_real_
  )
}

format_bytes <- function(bytes) {
  if (is.na(bytes)) return("an unknown size")
  if (bytes >= 1024^3) return(paste0(format(round(bytes / 1024^3, 2), nsmall = 1), " GB"))
  if (bytes >= 1024^2) return(paste0(round(bytes / 1024^2, 1), " MB"))
  paste0(format(round(bytes / 1024, 1), nsmall = 1), " KB")
}

format_seconds <- function(seconds) {
  if (is.na(seconds)) return("of unknown duration")
  if (seconds >= 3600) return(paste0(round(seconds / 3600, 1), " hours"))
  if (seconds >= 60) return(paste0(round(seconds / 60, 1), " minutes"))
  paste0(round(seconds), " seconds")
}


# Building the prompts ---------------------------------------------------------

#' Turn the input vector into what the structured call sends
#'
#' Text goes as strings. Images go inline through
#' [ellmer::content_image_file()], which every provider accepts. Audio and
#' video are uploaded to the provider and sent as a reference; every upload
#' completes before this returns, so either all the inputs are ready or no
#' coding request is made. A YouTube link is sent as a reference without an
#' upload, since the provider fetches it.
#'
#' @param x The input vector, already checked.
#' @param codebook The codebook: its `input_type` chooses the route, and for
#'   images its `image_file_resize` is applied to each file and its
#'   `image_url_detail` to each URL.
#' @param chat The chat the run will use; its provider receives the uploads.
#' @param local The local path of each element, from `resolve_input_files()`,
#'   or `NULL` when every element is what `x` says.
#' @param call The calling environment, for the error.
#'
#' @return A list of prompts, one per element of `x`.
#' @keywords internal
#' @noRd
as_input_content <- function(x, codebook, chat, local = NULL, call = rlang::caller_env()) {
  input_type <- codebook$input_type
  switch(input_type,
    text = as.list(x),
    image = as_image_content(x, codebook$image_file_resize,
                             codebook$image_url_detail %||% "auto"),
    audio = upload_inputs(x, chat, input_type, call = call),
    video = as_video_content(x, local %||% unname(x), chat, call = call),
    cli::cli_abort("Unknown input type {.val {input_type}}.", .internal = TRUE)
  )
}


#' Video: YouTube by reference, everything else uploaded
#'
#' A YouTube link becomes an [ellmer::ContentUploaded] carrying the URL
#' itself, which Gemini's serializer sends as the file URI; the MIME type is
#' nominal, and ignored by the provider for such a link (probe of
#' 2026-09-06). Every other element has a local path by now and goes through
#' `upload_inputs()`, all or nothing.
#'
#' @keywords internal
#' @noRd
as_video_content <- function(x, local, chat, call = rlang::caller_env()) {
  n <- length(x)
  content <- vector("list", n)
  by_reference <- is.na(local)
  provider <- tryCatch(chat$get_provider()@name, error = function(e) NA_character_)
  for (i in which(by_reference)) {
    content[[i]] <- ellmer::ContentUploaded(
      uri = x[[i]], mime_type = "video/mp4",
      provider = if (is.na(provider)) "Google/Gemini" else provider
    )
  }
  if (any(!by_reference)) {
    content[!by_reference] <- upload_inputs(
      local[!by_reference], chat, "video", names = x[!by_reference], call = call
    )
  }
  content
}


#' Upload every file, or none
#'
#' Uploads cost nothing, so a failed one aborts before a single coding
#' request is spent. The provider's message is kept for each failure: it may
#' be transient (a network error), permanent (a file the provider cannot
#' read), or the provider having no file upload at all through ellmer, and
#' only that message says which. What is known to accept the input type is
#' added as a hint, since a refusal is where it helps.
#'
#' @param x Paths, already checked and on this machine.
#' @param chat The chat whose provider receives the files.
#' @param input_type For the messages.
#' @param names What to call each file in a message: the element as the user
#'   gave it, so that a downloaded URL is named by its URL rather than by its
#'   temporary file. Defaults to the basenames of `x`.
#' @param call The calling environment, for the error.
#'
#' @return A list of [ellmer::ContentUploaded] objects, one per path.
#' @keywords internal
#' @noRd
upload_inputs <- function(x, chat, input_type, names = basename(x), call = rlang::caller_env()) {
  n <- length(x)
  uploaded <- vector("list", n)
  failures <- character()
  labels <- ifelse(is_input_url(names), redact_urls(names), basename(names))

  cli::cli_progress_bar(paste("Uploading", input_type, "files"), total = n)
  for (i in seq_len(n)) {
    result <- tryCatch(upload_input_file(chat, x[[i]]), error = function(e) e)
    if (inherits(result, "error")) {
      failures[labels[[i]]] <- strip_ansi(conditionMessage(result))
    } else {
      uploaded[[i]] <- result
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  if (length(failures)) {
    cli::cli_abort(c(
      "Uploading {length(failures)} of {n} {input_type} {cli::qty(n)}file{?s} failed, so no coding request was sent.",
      stats::setNames(
        paste0(names(failures), ": ", failures),
        rep("x", length(failures))
      ),
      "i" = paste0(
        "A failure may be transient, such as a network error, or permanent, ",
        "such as a file the provider cannot read; the message above says which."
      ),
      known_input_providers_hint(input_type)
    ), call = call)
  }
  uploaded
}

#' One upload, through ellmer's provider-neutral method
#'
#' Kept as its own function so that tests can stand in for the network.
#'
#' @keywords internal
#' @noRd
upload_input_file <- function(chat, path) {
  chat$file_upload(path)
}


# Provenance -------------------------------------------------------------------

#' What was coded: the files behind each unit, with a content hash
#'
#' A path can point at different bytes later, and so can a URL. Recording a
#' hash at coding time lets [qlm_replicate()] and [qlm_backfill()] check that
#' the files they are about to upload are the ones the run coded, and lets
#' [qlm_trail()] say which recordings the results rest on.
#'
#' @param x The elements of the input, in order: paths and URLs.
#' @param ids The `.id` of each, as character.
#' @param local The local path holding each element's bytes, or `NA` for an
#'   element the provider fetches (an image URL, a YouTube link). Defaults
#'   to `x` for paths and `NA` for URLs, the case with nothing downloaded.
#'
#' @return A data frame with `.id`, `file` (the basename, or the URL with any
#'   credential redacted), `size` (bytes) and `sha256`; the last two `NA`
#'   for an element with no bytes here.
#' @keywords internal
#' @noRd
file_provenance <- function(x, ids, local = NULL) {
  x <- unname(x)
  is_url <- is_input_url(x, data = TRUE)
  if (is.null(local)) {
    local <- ifelse(is_url, NA_character_, x)
  }
  here <- !is.na(local)
  size <- rep(NA_real_, length(x))
  size[here] <- as.numeric(file.size(local[here]))
  sha256 <- rep(NA_character_, length(x))
  sha256[here] <- vapply(local[here], hash_file, character(1), USE.NAMES = FALSE)
  data.frame(
    .id = as.character(ids),
    file = ifelse(is_url, redact_urls(x), basename(x)),
    size = size,
    sha256 = sha256,
    stringsAsFactors = FALSE
  )
}

hash_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}


#' Check that a run's files are still the ones it coded
#'
#' Called before a replication or a backfill pass uploads anything. A run
#' coded before hashes were recorded is allowed to proceed with a notice,
#' since nothing can be checked; the new run records hashes of its own.
#' URLs are not checked here: one the provider fetches (an image URL, a
#' YouTube link) was never a file on this machine, and one the run
#' downloaded is checked by the new run against the download it uploads,
#' through `expected_url_hashes()` and `with_expected_hashes()`.
#'
#' @param x A `qlm_coded` object, already checked.
#' @param ids The units about to be coded, or `NULL` for all of them.
#' @param call The calling environment, for the error.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
verify_input_files <- function(x, ids = NULL, call = rlang::caller_env()) {
  meta_attr <- attr(x, "meta")
  input_type <- meta_attr$object$input_type
  if (!input_type %in% file_input_types()) {
    return(invisible(NULL))
  }

  data <- inputs(x)
  if (is.null(ids)) {
    ids <- as.character(names(data) %||% seq_along(data))
    paths <- data
  } else {
    ids <- as.character(ids)
    paths <- inputs_by_id(x, ids)
  }
  paths <- unname(paths)

  recorded <- meta_attr$user$input_files
  if (is.null(recorded)) {
    cli::cli_inform(c(
      "i" = paste0(
        "This run recorded no file hashes, so the identity of its {input_type} ",
        "files cannot be verified; the new run records them."
      )
    ))
    return(invisible(NULL))
  }
  expected <- recorded$sha256[match(ids, recorded$.id)]

  # A URL is not a file on this machine: the provider fetches it, or the new
  # run downloads it and checks that download. Only the files are checked here.
  is_url <- is_input_url(paths, data = TRUE)
  ids <- ids[!is_url]
  paths <- paths[!is_url]
  expected <- expected[!is_url]
  if (!length(paths)) {
    return(invisible(NULL))
  }

  missing <- is.na(paths) | !file.exists(paths)
  if (any(missing)) {
    cli::cli_abort(c(
      "{sum(missing)} {input_type} {cli::qty(sum(missing))}file{?s} of this run no longer exist{?s/}: {.path {paths[missing]}}.",
      "i" = "The files are needed again to code the units {.val {ids[missing]}}."
    ), call = call)
  }

  # A unit with no hash was coded before hashes were recorded, or by a run
  # that was later backfilled with hashes for other units only. Nothing can
  # be checked for it, which is said; it is not called changed.
  unknown <- is.na(expected)
  if (any(unknown)) {
    cli::cli_inform(c(
      "i" = paste0(
        "{sum(unknown)} unit{?s} of this run {?has/have} no recorded file hash, so ",
        "the identity of {?its/their} {input_type} file{?s} cannot be verified: ",
        "{.val {ids[unknown]}}. The new run records {?it/them}."
      )
    ))
  }

  current <- file_provenance(paths, ids)
  changed <- !unknown & current$sha256 != expected
  if (any(changed)) {
    cli::cli_abort(c(
      "{cli::qty(sum(changed))}The {input_type} file{?s} of {sum(changed)} unit{?s} differ{?s/} from the one{?s} this run coded: {.val {ids[changed]}}.",
      "i" = "The recorded hash is from when the run was coded; the path now points at different bytes.",
      "i" = "Code the current files as a fresh run with {.fn qlm_code} instead."
    ), call = call)
  }
  invisible(NULL)
}


#' Carry a backfill pass's file hashes into the run it completed
#'
#' The pass uploaded and coded its units from the files as they were at that
#' moment, so its rows are the provenance of those units now. A run with a
#' table gets those rows replaced; a run coded before hashes were recorded
#' gets a table covering every unit, with the pass's rows filled and the
#' rest left `NA`, so that a later check can say which units it cannot
#' verify rather than treating them as changed.
#'
#' @param x The `qlm_coded` object being backfilled.
#' @param new The object a pass returned.
#'
#' @return `x`, with its `input_files` metadata updated.
#' @keywords internal
#' @noRd
merge_input_files <- function(x, new) {
  meta_attr <- attr(x, "meta")
  if (!meta_attr$object$input_type %in% file_input_types()) {
    return(x)
  }
  added <- attr(new, "meta")$user$input_files
  if (!is.data.frame(added) || !nrow(added)) {
    return(x)
  }

  table <- meta_attr$user$input_files
  if (is.null(table)) {
    data <- unname(inputs(x))
    ids <- as.character(names(inputs(x)) %||% seq_along(data))
    table <- data.frame(
      .id = ids,
      file = ifelse(is_input_url(data, data = TRUE), redact_urls(data), basename(data)),
      size = NA_real_,
      sha256 = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  pos <- match(as.character(added$.id), table$.id)
  keep <- !is.na(pos)
  table[pos[keep], c("file", "size", "sha256")] <- added[keep, c("file", "size", "sha256")]

  meta_attr$user$input_files <- table
  attr(x, "meta") <- meta_attr
  x
}


# Cost -------------------------------------------------------------------------

#' The qualification every audio cost carries
#'
#' ellmer prices a run from its total token count at the model's text rate.
#' Gemini reports audio tokens separately and charges more for them, and
#' `prices` supplied by the caller are per token too, so on either basis the
#' figure can only be low. Video tokens are charged at the text rate, so a
#' video run carries no such note.
#'
#' @keywords internal
#' @noRd
audio_cost_note <- function() {
  paste0(
    "audio input tokens are priced at the text rate, so the cost is ",
    "potentially underestimated"
  )
}
