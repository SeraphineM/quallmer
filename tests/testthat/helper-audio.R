# Fixtures for file inputs (#124 audio, #179 video), shared by the
# input_content, replicate, backfill and trail tests

# A file with the given bytes and extension, removed with the test
media_file <- function(bytes = as.raw(1:64), ext = "wav", env = parent.frame()) {
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = env)
  writeBin(bytes, path)
  path
}

audio_file <- function(bytes = as.raw(1:64), ext = "wav", env = parent.frame()) {
  media_file(bytes, ext, env)
}

video_file <- function(bytes = as.raw(1:64), ext = "mp4", env = parent.frame()) {
  media_file(bytes, ext, env)
}

media_codebook <- function(input_type = "audio") {
  qlm_codebook(
    if (input_type == "audio") "Audio" else "Video",
    "Code the recording.",
    ellmer::type_object(language = ellmer::type_string("Language spoken")),
    input_type = input_type
  )
}

audio_codebook <- function() media_codebook("audio")
video_codebook <- function() media_codebook("video")

# An uploaded-file reference as ellmer returns one, without the network
fake_upload <- function(chat, path) {
  mime <- switch(tolower(tools::file_ext(path)),
    mp4 = "video/mp4", webm = "video/webm", mov = "video/quicktime",
    "audio/wav"
  )
  ellmer::ContentUploaded(uri = paste0("files/", basename(path)), mime_type = mime)
}

# A download that writes the given bytes wherever it is asked to
fake_download <- function(bytes = as.raw(101:132)) {
  function(url, dest) {
    writeBin(bytes, dest)
    invisible(dest)
  }
}

# A coded file-input object built directly, as qlm_code() would leave it.
# `local` gives the local path behind each element (NA for one the provider
# fetches), as resolve_input_files() would; by default every element is a
# path.
media_run <- function(paths, ids = names(paths) %||% seq_along(paths),
                      with_hashes = TRUE, input_type = "audio",
                      model = "google_gemini/gemini-2.5-flash", failed = NULL,
                      local = NULL) {
  ids <- as.character(ids)
  names(paths) <- ids
  results <- data.frame(id = ids, language = rep("en", length(ids)), stringsAsFactors = FALSE)
  if (!is.null(failed)) {
    results$.error <- lapply(ids, function(i) {
      if (i %in% failed) simpleError("HTTP 500 Internal Server Error.") else NULL
    })
  }
  metadata <- list(timestamp = Sys.time(), n_units = length(ids), backend = "structured")
  if (with_hashes) {
    metadata$input_files <- file_provenance(unname(paths), ids, local = local)
  }
  new_qlm_coded(
    results = results,
    codebook = media_codebook(input_type),
    data = paths,
    input_type = input_type,
    chat_args = list(name = model),
    execution_args = list(on_error = "continue"),
    batch = FALSE,
    metadata = metadata,
    name = paste0(input_type, "_run"),
    call = quote(qlm_code(...)),
    parent = NULL
  )
}

audio_run <- function(paths, ids = names(paths) %||% seq_along(paths),
                      with_hashes = TRUE,
                      model = "google_gemini/gemini-2.5-flash", failed = NULL) {
  media_run(paths, ids = ids, with_hashes = with_hashes, input_type = "audio",
            model = model, failed = failed)
}
