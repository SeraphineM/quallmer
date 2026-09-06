#' Input types a codebook can declare
#'
#' One place for the values `input_type` may take, so that [qlm_codebook()],
#' [qlm_code()] and the documentation agree. `"text"` sends each element as a
#' string; `"image"` sends each file inline; `"audio"` uploads each file to
#' the provider and sends a reference to it. See `as_input_content()` for
#' the dispatch and `input_capabilities()` for which providers accept what.
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
input_types <- function() {
  c("text", "image", "audio")
}

#' The input types whose elements are file paths
#' @keywords internal
#' @noRd
file_input_types <- function() {
  c("image", "audio")
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


#' Check file inputs before any upload or request
#'
#' Each element must be the path of an existing file, and for audio the
#' extension must be one the upload can label. Checked in one pass so that
#' every problem is reported at once.
#'
#' @param x The input vector.
#' @param input_type One of `file_input_types()`.
#' @param call The calling environment, for the error.
#'
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_file_inputs <- function(x, input_type, call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort(
      "This codebook expects {input_type} file paths{if (input_type == 'image') ' or URLs' else ''} (a character vector).",
      call = call
    )
  }
  # An image may be given as a URL, which the provider fetches; a URL is
  # recognised by its scheme and is not a file here (#177)
  is_path <- if (input_type == "image") !is_image_url(x) else rep(TRUE, length(x))
  missing <- is_path & (is.na(x) | !file.exists(x) | dir.exists(x))
  if (any(missing)) {
    hint <- if (input_type == "image") {
      c(
        "i" = "Every element of {.arg x} must be the path of an existing file or a URL.",
        "i" = "A URL is recognised by its scheme: {.code http://}, {.code https://} or {.code data:}."
      )
    } else {
      c("i" = "Every element of {.arg x} must be the path of an existing file.")
    }
    cli::cli_abort(c(
      "{sum(missing)} {input_type} {cli::qty(sum(missing))}file{?s} {?does/do} not exist: {.path {x[missing]}}.",
      hint
    ), call = call)
  }
  if (input_type == "audio") {
    ext <- tolower(tools::file_ext(x))
    unknown <- !ext %in% audio_extensions()
    if (any(unknown)) {
      cli::cli_abort(c(
        "{sum(unknown)} file{?s} {?has/have} an extension the upload cannot label: {.path {x[unknown]}}.",
        "i" = "Audio files must be one of {.val {audio_extensions()}}."
      ), call = call)
    }
  }
  invisible(x)
}


# Which provider/model combinations accept a file input type ------------------

#' Capability table for gated input types
#'
#' A snapshot of what providers accepted through ellmer on the date given,
#' written down rather than discovered, because the failure it prevents is an
#' upload followed by a refused request. Text and image inputs are not gated:
#' images travel inline and every provider ellmer reaches takes them.
#'
#' Audio: only Gemini chat models accept an uploaded audio file alongside a
#' schema-constrained request. The provider is matched on the name ellmer
#' gives its provider object, which separates Gemini from Vertex, which has
#' no Files API. The model is matched on recognised families rather than
#' enumerated, so a later release in a known family passes; the suffixes are
#' anchored so that a TTS, image, live or transcription model that shares
#' the family stem never does. `models_google_gemini()` returns only names
#' and prices, so there is no capability metadata to consult at run time.
#'
#' A model outside the recognised families is accepted for the session by
#' [qlm_register_model()].
#'
#' @return A named list, one entry per gated input type, each with
#'   `providers` (accepted provider names), `families` (a regular expression
#'   over the model name) and `describe` (the families in words, for
#'   messages).
#' @keywords internal
#' @noRd
input_capabilities <- function() {
  list(
    # As of 2026-09-05
    audio = list(
      providers = "Google/Gemini",
      families = paste0(
        "^gemini-[0-9]+(\\.[0-9]+)?-(pro|flash|flash-lite)",
        "(-preview)?(-[0-9]{2}-[0-9]{2}|-[0-9]{3}|-latest)?$"
      ),
      describe = paste0(
        "gemini-<version>-pro, -flash and -flash-lite, with an optional ",
        "-preview, date or -latest suffix"
      )
    )
  )
}

# Session registry of provider/model pairs accepted beyond the table
the <- new.env(parent = emptyenv())
the$registered_input_models <- list()

#' Accept a model for a file input type in this session
#'
#' `r lifecycle::badge("experimental")`
#'
#' `qlm_code()` refuses to upload a file to a provider/model combination it
#' does not know to accept that kind of input, because the refusal would
#' otherwise arrive only after the upload, as a failed request. The
#' combinations it knows are a snapshot (see `?qlm_code`, section "Audio
#' input"), so a model released after this version of quallmer may be
#' refused although it works. This function accepts one exact
#' `"provider/model"` pair for one input type, for the rest of the R session.
#'
#' Only the model check is bypassed. The provider must still be one that
#' takes the input type through ellmer's file upload, `batch = TRUE` is still
#' refused for audio, and the files are still checked. Registering a model of
#' a provider that cannot take the input therefore changes nothing.
#'
#' A run whose model was accepted this way records that fact, and
#' [qlm_trail()] reports it. Replicating or backfilling such a run in another
#' session requires registering the model again, and the error says so.
#'
#' @param model character; the model in `"provider/model"` form, exactly as
#'   passed to [qlm_code()], for example `"google_gemini/gemini-4-ultra"`.
#'   The model part is required: a bare provider is resolved to a default
#'   model by ellmer, which cannot be registered ahead of time.
#' @param input_type character; the input type to accept the model for.
#'   Currently only `"audio"` is gated.
#'
#' @return `model`, invisibly.
#'
#' @examples
#' \dontrun{
#' qlm_register_model("google_gemini/gemini-4-ultra", input_type = "audio")
#' coded <- qlm_code(audio_files, audio_codebook, model = "google_gemini/gemini-4-ultra")
#' }
#'
#' @seealso [qlm_register_provider()] for configuring an endpoint;
#'   [qlm_code()], whose "Audio input" section says what is accepted
#'   without registration.
#' @export
qlm_register_model <- function(model, input_type = "audio") {
  gated <- names(input_capabilities())
  if (!is.character(input_type) || length(input_type) != 1L ||
      !input_type %in% gated) {
    cli::cli_abort(c(
      "{.arg input_type} must be one of {.val {gated}}.",
      "i" = "Only these input types are gated by provider and model."
    ))
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !grepl("^[^/]+/[^/]+$", model)) {
    cli::cli_abort(c(
      "{.arg model} must be a single string of the form {.val provider/model}.",
      "i" = "For example {.val google_gemini/gemini-4-ultra}."
    ))
  }
  check_model_provider(model)

  current <- the$registered_input_models[[input_type]]
  the$registered_input_models[[input_type]] <- unique(c(current, model))
  cli::cli_inform(c(
    "i" = "Accepting {.val {model}} for {input_type} input in this session."
  ))
  invisible(model)
}

#' Is a provider/model pair registered for an input type?
#' @keywords internal
#' @noRd
is_registered_input_model <- function(input_type, provider, model_id) {
  paste0(provider, "/", model_id) %in% the$registered_input_models[[input_type]]
}

#' Forget every registration, for tests
#' @keywords internal
#' @noRd
reset_registered_input_models <- function() {
  the$registered_input_models <- list()
  invisible(NULL)
}


#' The endpoint a run will use, read from the chat it built
#'
#' The provider name and model are taken from the chat object, not from the
#' string the user typed, so that `model = "google_gemini"` is checked
#' against the default model ellmer filled in, and a Vertex endpoint is told
#' from a Gemini one. The provider prefix is kept from the string, since that
#' is how a registration names it.
#'
#' @param chat An ellmer `Chat`.
#' @param model The model string, as passed to [qlm_code()].
#'
#' @return A list: `prefix`, `provider` (ellmer's name for it, or `NA`),
#'   `model_id`.
#' @keywords internal
#' @noRd
resolve_endpoint <- function(chat, model) {
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  provider_name <- if (is.null(provider)) NA_character_ else attr(provider, "name")
  model_id <- tryCatch(chat$get_model(), error = function(e) NULL)
  if (is.null(model_id)) {
    model_id <- if (grepl("/", model, fixed = TRUE)) sub("^[^/]*/", "", model) else NA_character_
  }
  list(
    prefix = model_provider(model),
    provider = provider_name %||% NA_character_,
    model_id = model_id
  )
}


#' Refuse a provider/model combination that cannot take the input type
#'
#' Run after the chat is built and before anything is uploaded or sent. Two
#' gates: the provider must accept the input type through ellmer's file
#' upload, and the model must be in a recognised family or registered for the
#' session. Only the second is bypassed by a registration.
#'
#' @param input_type The codebook's input type.
#' @param chat The chat the run will use.
#' @param model The model string, as passed to [qlm_code()].
#' @param call The calling environment, for the error.
#'
#' @return Invisibly, `NULL` for an ungated input type; otherwise a list with
#'   `registered`, the `"provider/model"` string the acceptance rested on
#'   when it came from a registration, or `NULL`.
#' @keywords internal
#' @noRd
check_input_capability <- function(input_type, chat, model, call = rlang::caller_env()) {
  caps <- input_capabilities()[[input_type]]
  if (is.null(caps)) {
    return(invisible(NULL))
  }
  endpoint <- resolve_endpoint(chat, model)
  transcribe <- paste0(
    "For any other provider, transcribe the recordings first and code the ",
    "transcripts with a text codebook."
  )

  if (is.na(endpoint$provider) || !endpoint$provider %in% caps$providers) {
    reached <- if (is.na(endpoint$provider)) "a provider" else endpoint$provider
    cli::cli_abort(c(
      "{input_type} input needs a provider that accepts {input_type} files through ellmer's file upload.",
      "x" = "{.val {model}} is reached through {reached}, which does not.",
      "i" = "Accepted today: {.val {caps$providers}} (a {.code google_gemini/} model).",
      "i" = transcribe
    ), call = call)
  }

  registered <- is_registered_input_model(input_type, endpoint$prefix, endpoint$model_id)
  if (!registered && !isTRUE(grepl(caps$families, endpoint$model_id))) {
    pair <- paste0(endpoint$prefix, "/", endpoint$model_id)
    cli::cli_abort(c(
      "Model {.val {endpoint$model_id}} is not recognised as accepting {input_type} input.",
      "i" = "Recognised families: {caps$describe}.",
      "i" = paste0(
        "If it does accept {input_type}, register it for this session with ",
        "{.code qlm_register_model(\"", pair, "\", input_type = \"",
        input_type, "\")}."
      ),
      "i" = transcribe
    ), call = call)
  }

  invisible(list(
    registered = if (registered) paste0(endpoint$prefix, "/", endpoint$model_id)
  ))
}


# Building the prompts ---------------------------------------------------------

#' Turn the input vector into what the structured call sends
#'
#' Text goes as strings. Images go inline through
#' [ellmer::content_image_file()], which every provider accepts. Audio is
#' uploaded to the provider and sent as a reference; every upload completes
#' before this returns, so either all the inputs are ready or no coding
#' request is made.
#'
#' @param x The input vector, already checked.
#' @param codebook The codebook: its `input_type` chooses the route, and for
#'   images its `image_file_resize` is applied to each file and its
#'   `image_url_detail` to each URL.
#' @param chat The chat the run will use; its provider receives the uploads.
#' @param call The calling environment, for the error.
#'
#' @return A list of prompts, one per element of `x`.
#' @keywords internal
#' @noRd
as_input_content <- function(x, codebook, chat, call = rlang::caller_env()) {
  input_type <- codebook$input_type
  switch(input_type,
    text = as.list(x),
    image = as_image_content(x, codebook$image_file_resize,
                             codebook$image_url_detail %||% "auto"),
    audio = upload_inputs(x, chat, input_type, call = call),
    cli::cli_abort("Unknown input type {.val {input_type}}.", .internal = TRUE)
  )
}


#' Upload every file, or none
#'
#' Uploads cost nothing, so a failed one aborts before a single coding
#' request is spent. The provider's message is kept for each failure: it may
#' be transient (a network error) or permanent (a file the provider cannot
#' read), and only that message says which.
#'
#' @param x Paths, already checked.
#' @param chat The chat whose provider receives the files.
#' @param input_type For the messages.
#' @param call The calling environment, for the error.
#'
#' @return A list of [ellmer::ContentUploaded] objects, one per path.
#' @keywords internal
#' @noRd
upload_inputs <- function(x, chat, input_type, call = rlang::caller_env()) {
  n <- length(x)
  uploaded <- vector("list", n)
  failures <- character()

  cli::cli_progress_bar(paste("Uploading", input_type, "files"), total = n)
  for (i in seq_len(n)) {
    result <- tryCatch(upload_input_file(chat, x[[i]]), error = function(e) e)
    if (inherits(result, "error")) {
      failures[basename(x[[i]])] <- strip_ansi(conditionMessage(result))
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
      )
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
#' A path can point at different bytes later. Recording a hash at coding time
#' lets [qlm_replicate()] and [qlm_backfill()] check that the files they are
#' about to upload are the ones the run coded, and lets [qlm_trail()] say
#' which recordings the results rest on.
#'
#' @param x The paths, in input order.
#' @param ids The `.id` of each, as character.
#'
#' @return A data frame with `.id`, `file` (the basename), `size` (bytes)
#'   and `sha256`.
#' @keywords internal
#' @noRd
file_provenance <- function(x, ids) {
  # A URL has no bytes here to size or hash: the provider fetches it, so
  # the record keeps the URL itself and leaves both NA (#177)
  is_path <- !is_image_url(x)
  size <- rep(NA_real_, length(x))
  size[is_path] <- as.numeric(file.size(x[is_path]))
  sha256 <- rep(NA_character_, length(x))
  sha256[is_path] <- vapply(x[is_path], hash_file, character(1), USE.NAMES = FALSE)
  data.frame(
    .id = as.character(ids),
    file = ifelse(is_path, basename(x), x),
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

  # A URL was never a file on this machine, so there is nothing to verify
  is_path <- !is_image_url(paths)
  ids <- ids[is_path]
  paths <- paths[is_path]
  if (!length(paths)) {
    return(invisible(NULL))
  }

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
  expected <- recorded$sha256[match(ids, recorded$.id)]
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
    data <- inputs(x)
    ids <- as.character(names(data) %||% seq_along(data))
    table <- data.frame(
      .id = ids,
      file = basename(unname(data)),
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


#' Every registration a run and its backfill passes relied on
#'
#' @param x A `qlm_coded` object.
#'
#' @return A character vector of `"provider/model"` pairs, possibly empty.
#' @keywords internal
#' @noRd
recorded_registrations <- function(x) {
  meta_attr <- attr(x, "meta")
  from_passes <- unlist(lapply(
    meta_attr$object$backfill,
    function(pass) pass$input_model_registered
  ))
  unique(c(meta_attr$user$input_model_registered, from_passes))
}


#' Refuse to replay a run whose model was accepted by registration, unless
#' this session has registered it too
#'
#' The registration is session state by design, and a run records when it
#' relied on one. A fresh session that replicates or backfills such a run
#' must register the model again, and the error gives the call.
#'
#' @param x A `qlm_coded` object, already checked.
#' @param model The model the new run will use.
#' @param call The calling environment, for the error.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
check_registered_input_model <- function(x, model, call = rlang::caller_env()) {
  meta_attr <- attr(x, "meta")
  pairs <- recorded_registrations(x)
  if (!length(pairs)) {
    return(invisible(NULL))
  }
  input_type <- meta_attr$object$input_type
  # The run's own model needed the run-level registration; a pass's model
  # needed the pass's. Any other model is checked afresh by qlm_code().
  needed <- character()
  if (identical(model, meta_attr$object$chat_args$name)) {
    needed <- meta_attr$user$input_model_registered
  }
  if (model %in% pairs) {
    needed <- c(needed, model)
  }
  needed <- unique(needed)
  for (registered in needed) {
    pair <- strsplit(registered, "/", fixed = TRUE)[[1]]
    if (is_registered_input_model(input_type, pair[1], pair[2])) {
      next
    }
    cli::cli_abort(c(
      "This run accepted {.val {registered}} for {input_type} input through {.fn qlm_register_model}, and this session has not registered it.",
      "i" = paste0(
        "Run {.code qlm_register_model(\"", registered, "\", input_type = \"",
        input_type, "\")} and try again."
      )
    ), call = call)
  }
  invisible(NULL)
}


# Cost -------------------------------------------------------------------------

#' The qualification every audio cost carries
#'
#' ellmer prices a run from its total token count at the model's text rate.
#' Gemini reports audio tokens separately and charges more for them, and
#' `prices` supplied by the caller are per token too, so on either basis the
#' figure can only be low.
#'
#' @keywords internal
#' @noRd
audio_cost_note <- function() {
  paste0(
    "audio input tokens are priced at the text rate, so the cost is ",
    "potentially underestimated"
  )
}
