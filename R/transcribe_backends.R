# The two ways a recording reaches a model. Each route has a setup step, run
# before anything is fetched or sent, and a run step that takes local paths
# and returns one outcome per path: a list with `text`, `usage` and `error`,
# or `NULL` for a request never submitted.
#
# Which route a model takes is decided by its provider prefix, not by what
# the model is known to do: `openai/` and any provider registered with
# qlm_register_provider() have a transcription endpoint, so the request goes
# there; every other provider ellmer reaches gets the recording uploaded and
# a chat model asked to transcribe it. Whether a model can is for the
# provider to say, at no cost: an upload is free and a refused request is
# not billed.

# The transcription endpoint -----------------------------------------------------

#' What the endpoint route needs before a request: a key and an endpoint
#'
#' `openai/` reads `OPENAI_API_KEY` and the OpenAI host; a registered
#' provider reads the environment variable and host it was registered with.
#'
#' @keywords internal
#' @noRd
endpoint_transcription_setup <- function(model, api_key, base_url, call = rlang::caller_env()) {
  provider <- model_provider(model)
  entry <- if (identical(provider, "openai")) {
    list(base_url = "https://api.openai.com/v1", api_key_env = "OPENAI_API_KEY")
  } else {
    provider_definition(provider)
  }
  key <- api_key %||% Sys.getenv(entry$api_key_env)
  if (!nzchar(key)) {
    cli::cli_abort(c(
      "No API key for the {.val {provider}} transcription endpoint.",
      "i" = "Set the environment variable {.envvar {entry$api_key_env}}, or pass {.arg api_key}."
    ), call = call)
  }
  # The host is recorded whether given or resolved: a registered provider's
  # host can be re-registered, so the prefix alone does not name it
  resolved <- base_url %||% entry$base_url
  list(
    api_key = key,
    base_url = resolved,
    recorded_base_url = redact_url(resolved)
  )
}

#' Transcribe local files through `/audio/transcriptions`, in parallel
#'
#' One multipart request per file, performed through the same pool ellmer's
#' parallel calls use, which is also what draws the progress bar.
#'
#' @keywords internal
#' @noRd
transcribe_endpoint <- function(paths, model_id, language, prompt, api_key,
                                base_url, max_active, rpm, on_error) {
  reqs <- lapply(paths, transcription_request, model_id = model_id,
                 language = language, prompt = prompt, api_key = api_key,
                 base_url = base_url, rpm = rpm)
  resps <- perform_transcription_requests(reqs, max_active = max_active, on_error = on_error)
  lapply(resps, function(resp) {
    if (is.null(resp)) {
      return(NULL)
    }
    if (inherits(resp, "error")) {
      # A response the request itself declared an error, a 200 with no
      # transcript, travels on the condition; its usage is still worth
      # keeping, and its message is the body's, not a misleading status
      failed <- resp$resp
      if (!is.null(failed) && !httr2::resp_is_error(failed)) {
        return(list(text = NA_character_, usage = transcription_usage(failed),
                    error = transcription_error_body(failed)))
      }
      return(list(text = NA_character_, usage = transcription_usage(failed),
                  error = strip_ansi(conditionMessage(resp))))
    }
    parse_transcription_response(resp)
  })
}

#' One transcription request
#'
#' A response without a transcript is an error of the request itself, so
#' that `on_error` governs it inside the pool as it governs a refusal:
#' `"stop"` raises it and `"return"` submits nothing after it. The
#' provider's own sentence is read from an error body, so a refusal says
#' why rather than only its status. A rate-limited request waits as long as
#' the provider asks and is retried; the throttle keeps the pool under `rpm`.
#'
#' @keywords internal
#' @noRd
transcription_request <- function(path, model_id, language, prompt,
                                  api_key, base_url, rpm) {
  fields <- list(
    file = curl::form_file(path),
    model = model_id,
    response_format = "json",
    language = language,
    prompt = prompt
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]

  req <- httr2::request(base_url)
  req <- httr2::req_url_path_append(req, "audio", "transcriptions")
  req <- httr2::req_auth_bearer_token(req, api_key)
  req <- do.call(httr2::req_body_multipart, c(list(req), fields))
  req <- httr2::req_error(req, is_error = transcription_response_is_error,
                          body = transcription_error_body)
  req <- httr2::req_retry(req, max_tries = 3)
  httr2::req_throttle(req, capacity = rpm, fill_time_s = 60)
}

#' The pool, kept apart so that tests can read what was sent
#' @keywords internal
#' @noRd
perform_transcription_requests <- function(reqs, max_active, on_error) {
  httr2::req_perform_parallel(
    reqs, on_error = on_error, progress = "Transcribing", max_active = max_active
  )
}

#' Is a response one the caller cannot use?
#'
#' An HTTP error, or a success that carries no transcript text.
#'
#' @keywords internal
#' @noRd
transcription_response_is_error <- function(resp) {
  httr2::resp_is_error(resp) || is.null(transcription_text(resp))
}

#' The transcript in a response body, or `NULL`
#'
#' A blank string is no transcript, as on the chat route: a recording the
#' model heard nothing in must be recorded as a failure, not as coded
#' silence.
#'
#' @keywords internal
#' @noRd
transcription_text <- function(resp) {
  json <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  text <- json$text
  if (is.character(text) && length(text) == 1L && !is.na(text) && nzchar(trimws(text))) {
    text
  } else {
    NULL
  }
}

#' The usage in a response body, or `NULL`
#' @keywords internal
#' @noRd
transcription_usage <- function(resp) {
  if (is.null(resp)) {
    return(NULL)
  }
  json <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  json$usage
}

#' The message for a failed response, for httr2 to append to the status
#'
#' An OpenAI-shaped error body gives the provider's sentence; a success
#' without a transcript says so.
#'
#' @keywords internal
#' @noRd
transcription_error_body <- function(resp) {
  if (!httr2::resp_is_error(resp)) {
    return("the response carried no transcript text")
  }
  json <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  message <- json$error$message
  if (is.character(message) && length(message) == 1L) message else NULL
}

#' The transcript and usage in a response, or why there is none
#'
#' The usage is kept as it came: the `gpt-4o` models report tokens split by
#' modality, `whisper-1` reports seconds, and another host may report
#' something else again.
#'
#' @keywords internal
#' @noRd
parse_transcription_response <- function(resp) {
  text <- transcription_text(resp)
  if (is.null(text)) {
    return(list(
      text = NA_character_, usage = transcription_usage(resp),
      error = "the response carried no transcript text"
    ))
  }
  list(text = text, usage = transcription_usage(resp), error = NA_character_)
}


# A chat model that hears the recording -------------------------------------------

#' What the chat route needs before an upload: the chat itself
#'
#' Built offline; nothing is sent. No check of what the model can do: the
#' provider says so when asked, and asking costs nothing.
#'
#' @keywords internal
#' @noRd
chat_transcription_setup <- function(model, language, prompt, api_key, base_url) {
  # ellmer takes a key as a function since 0.4.0; the closure holds the key
  # and goes nowhere, since the chat is not recorded
  chat_args <- list(
    credentials = if (!is.null(api_key)) function() api_key,
    base_url = base_url
  )
  chat_args <- chat_args[!vapply(chat_args, is.null, logical(1))]
  chat <- do.call(ellmer::chat, c(
    list(name = model, system_prompt = transcription_instruction(language, prompt)),
    chat_args
  ))
  list(
    chat = chat,
    recorded_base_url = if (!is.null(base_url)) redact_url(base_url)
  )
}

#' The fixed instruction, with the caller's hints appended
#' @keywords internal
#' @noRd
transcription_instruction <- function(language = NULL, prompt = NULL) {
  paste(c(
    "You are a transcription engine. Transcribe the recording verbatim, in the language spoken.",
    "Output only the transcript: no summary, no speaker labels, no timestamps, no commentary, no translation.",
    if (!is.null(language)) {
      paste0("The recording is in the language with ISO 639-1 code \"", language, "\".")
    },
    if (!is.null(prompt)) paste0("Context from the caller: ", prompt)
  ), collapse = "\n")
}

#' Transcribe local files through a chat model, in parallel
#'
#' Every upload completes before the first request. A failed upload follows
#' `on_error` as a failed request does: recorded on its unit under
#' `"continue"`, the uploads after it withheld under `"return"`, raised
#' under `"stop"`. Each file is then one conversation from the system
#' prompt, so no file sees another. An answer with no transcript in it is
#' known only after the pool has returned, so it is raised under `"stop"`
#' and recorded on its unit otherwise; `"return"` cannot withhold what was
#' already submitted.
#'
#' @keywords internal
#' @noRd
transcribe_chat <- function(paths, chat, max_active, rpm, on_error,
                            call = rlang::caller_env()) {
  n <- length(paths)
  uploaded <- vector("list", n)
  errors <- rep(NA_character_, n)
  submit <- rep(TRUE, n)

  cli::cli_progress_bar("Uploading audio files", total = n)
  for (i in seq_len(n)) {
    if (!submit[[i]]) {
      cli::cli_progress_update()
      next
    }
    result <- tryCatch(upload_input_file(chat, paths[[i]]), error = function(e) e)
    if (inherits(result, "error")) {
      message <- strip_ansi(conditionMessage(result))
      if (on_error == "stop") {
        cli::cli_progress_done()
        cli::cli_abort(c(
          "Uploading {.path {basename(paths[[i]])}} failed.",
          "x" = message,
          "i" = "A failure may be transient, such as a network error, or permanent, such as a file the provider cannot read."
        ), call = call)
      }
      errors[[i]] <- paste0("upload failed: ", message)
      if (on_error == "return" && i < n) {
        submit[(i + 1L):n] <- FALSE
      }
    } else {
      uploaded[[i]] <- result
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  ready <- submit & is.na(errors)
  outcomes <- vector("list", n)
  for (i in which(!is.na(errors))) {
    outcomes[[i]] <- list(text = NA_character_, usage = NULL, error = errors[[i]])
  }
  if (!any(ready)) {
    return(outcomes)
  }

  chats <- ellmer::parallel_chat(
    chat, uploaded[ready], max_active = max_active, rpm = rpm, on_error = on_error
  )
  outcomes[ready] <- lapply(chats, function(result) {
    if (is.null(result)) {
      return(NULL)
    }
    if (inherits(result, "error")) {
      return(list(text = NA_character_, usage = NULL,
                  error = strip_ansi(conditionMessage(result))))
    }
    chat_transcription_outcome(result)
  })

  if (on_error == "stop") {
    empty <- which(vapply(outcomes, function(o) !is.null(o) && !is.na(o$error), logical(1)))
    if (length(empty)) {
      cli::cli_abort(c(
        "Transcribing {.path {basename(paths[[empty[1]]])}} failed.",
        "x" = outcomes[[empty[1]]]$error
      ), call = call)
    }
  }
  outcomes
}

#' The transcript and usage from a finished conversation
#'
#' The usage is ellmer's: the token counts, the cost it computed, and the
#' version that computed it, with the qualification that audio tokens are
#' priced at the text rate.
#'
#' @keywords internal
#' @noRd
chat_transcription_outcome <- function(chat) {
  turn <- chat$last_turn()
  text <- trimws(turn@text)
  usage <- list(
    tokens = as.list(turn@tokens),
    cost = tryCatch(as.numeric(chat$get_cost()), error = function(e) NA_real_),
    note = audio_cost_note(),
    ellmer_version = tryCatch(
      as.character(utils::packageVersion("ellmer")),
      error = function(e) NA_character_
    )
  )
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(list(text = NA_character_, usage = usage,
                error = "the model returned no transcript text"))
  }
  list(text = text, usage = usage, error = NA_character_)
}
