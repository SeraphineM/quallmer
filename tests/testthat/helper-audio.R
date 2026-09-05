# Fixtures for audio input (#124), shared by the input_content, replicate,
# backfill and trail tests

# A file with the given bytes and extension, removed with the test
audio_file <- function(bytes = as.raw(1:64), ext = "wav", env = parent.frame()) {
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = env)
  writeBin(bytes, path)
  path
}

audio_codebook <- function() {
  qlm_codebook(
    "Audio", "Code the recording.",
    ellmer::type_object(language = ellmer::type_string("Language spoken")),
    input_type = "audio"
  )
}

# An uploaded-file reference as ellmer returns one, without the network
fake_upload <- function(chat, path) {
  ellmer::ContentUploaded(uri = paste0("files/", basename(path)), mime_type = "audio/wav")
}

# A coded audio object built directly, as qlm_code() would leave it
audio_run <- function(paths, ids = names(paths) %||% seq_along(paths),
                      with_hashes = TRUE, registered = NULL,
                      model = "google_gemini/gemini-2.5-flash", failed = NULL) {
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
    metadata$input_files <- file_provenance(unname(paths), ids)
  }
  if (!is.null(registered)) {
    metadata$input_model_registered <- registered
  }
  new_qlm_coded(
    results = results,
    codebook = audio_codebook(),
    data = paths,
    input_type = "audio",
    chat_args = list(name = model),
    execution_args = list(on_error = "continue"),
    batch = FALSE,
    metadata = metadata,
    name = "audio_run",
    call = quote(qlm_code(...)),
    parent = NULL
  )
}
