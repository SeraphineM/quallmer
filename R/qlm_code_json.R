#' Code data with a provider that does not enforce the output schema
#'
#' Coding handler used by [qlm_code()] for providers whose structured-output
#' endpoint guarantees JSON syntax but not JSON Schema conformance (currently
#' DeepSeek). Rather than trusting the provider, it asks for JSON mode, puts the
#' codebook schema in the system prompt, validates each response locally against
#' `codebook$schema`, and re-prompts with the specific validation error when a
#' response does not conform.
#'
#' Requests go through [ellmer::parallel_chat()] rather than
#' [ellmer::parallel_chat_text()] because the latter reduces each chat to its
#' response text and discards the `Turn`, which is where token counts and cost
#' live.
#'
#' @param x Character vector of text units.
#' @param codebook A `qlm_codebook` object.
#' @param model Provider (and optionally model) name, as passed to [qlm_code()].
#' @param chat_args List of arguments for [ellmer::chat()].
#' @param execution_args List of execution arguments. `max_active`, `rpm` and
#'   `on_error` are forwarded to [ellmer::parallel_chat()]; `include_tokens` and
#'   `include_cost` are honoured here, as they are on the default path.
#' @param batch Logical. Must be `FALSE`; JSON-mode coding has no batch path.
#' @param max_retries Number of repair attempts for each empty, unparsable, or
#'   schema-invalid response. Default is 2.
#'
#' @return A data frame with one row per unit, carrying `.error` for units that
#'   never validated, plus token and cost columns when requested. Handler
#'   metadata is attached as the `qlm_backend_meta` attribute.
#' @keywords internal
#' @noRd
code_handler_json <- function(x, codebook, model, chat_args, execution_args,
                              batch = FALSE, max_retries = 2L) {
  # The handler is reached via do.call(), so report guard failures against
  # qlm_code() rather than against an anonymous function.
  error_call <- rlang::caller_env()

  if (!identical(batch, FALSE)) {
    cli::cli_abort(c(
      "{.arg batch} must be {.code FALSE} for model {.val {model}}.",
      "i" = "This provider is coded in JSON mode, which has no batch API path."
    ), call = error_call)
  }
  if (!identical(codebook$input_type, "text")) {
    cli::cli_abort(c(
      "Model {.val {model}} supports text codebooks only.",
      "i" = "{.arg codebook} has {.field input_type} {.val {codebook$input_type}}."
    ), call = error_call)
  }
  if (!inherits(codebook$schema, "ellmer::TypeObject")) {
    cli::cli_abort(c(
      "{.arg codebook} must have a schema created by {.fn ellmer::type_object}.",
      "i" = "This is what produces the one-row-per-unit result that {.fn qlm_code} returns."
    ), call = error_call)
  }
  if (length(max_retries) != 1L || is.na(max_retries) || !is.numeric(max_retries) ||
      max_retries < 0 || max_retries != trunc(max_retries)) {
    cli::cli_abort("{.arg max_retries} must be a single non-negative integer.",
                   call = error_call)
  }
  max_retries <- as.integer(max_retries)

  # JSON mode belongs in the raw request body. User api_args are kept, but
  # response_format is deliberately overwritten so that validation always has
  # JSON to work with.
  api_args <- chat_args$api_args %||% list()
  if (!is.list(api_args)) {
    cli::cli_abort("{.arg api_args} must be a named list.", call = error_call)
  }
  chat_args$api_args <- utils::modifyList(
    api_args,
    list(response_format = list(type = "json_object"))
  )

  chat <- do.call(ellmer::chat, c(
    list(
      name = model,
      system_prompt = json_system_prompt(codebook)
    ),
    chat_args
  ))

  include_tokens <- isTRUE(execution_args$include_tokens)
  include_cost <- isTRUE(execution_args$include_cost)
  pc_arg_names <- setdiff(names(formals(ellmer::parallel_chat)), c("chat", "prompts"))
  pc_args <- execution_args[names(execution_args) %in% pc_arg_names]
  # Continue lets one bad request be retried without discarding the valid
  # responses from the rest of the parallel call.
  if (is.null(pc_args$on_error)) {
    pc_args$on_error <- "continue"
  }

  parsed <- vector("list", length(x))
  problems <- vector("list", length(x))
  # Requests rejected outright by the provider: a bad model name, bad
  # credentials, a malformed request. Distinguished from other terminal
  # failures because they indicate the run was misconfigured, not that the
  # model declined to answer one particular unit.
  fatal <- rep(FALSE, length(x))
  pending <- seq_along(x)

  # Token and cost accumulators, summed ACROSS retry attempts: a repair attempt
  # is a real billed request, so reporting only the successful attempt would
  # understate what the run cost.
  usage <- matrix(
    0,
    nrow = length(x), ncol = 4,
    dimnames = list(NULL, c("input_tokens", "output_tokens",
                            "cached_input_tokens", "cost"))
  )

  for (attempt in 0L:max_retries) {
    if (!length(pending)) {
      break
    }

    prompts <- if (attempt == 0L) {
      as.list(unname(x[pending]))
    } else {
      lapply(pending, function(i) json_repair_prompt(x[[i]], problems[[i]]))
    }

    turns <- json_chat_turns(chat, prompts, pc_args)

    next_pending <- integer()
    for (j in seq_along(pending)) {
      i <- pending[[j]]
      usage[i, ] <- usage[i, ] + turns$usage[j, ]
      checked <- parse_and_validate_json(turns$text[[j]], codebook$schema)
      # Prefer the transport-level reason over the generic "empty response".
      if (!isTRUE(checked$ok) && !is.na(turns$error[[j]])) {
        checked$error <- paste0("API request failed: ", turns$error[[j]])
      }
      # A response the provider itself reports as incomplete -- cut off at
      # max_tokens, withheld by a content filter -- is a failure whatever the
      # text looks like. Cut-off JSON usually fails to parse, and then the
      # provider's reason is the one to record rather than "Invalid JSON"; an
      # object that happened to close just before the limit parses cleanly,
      # and the provider's word still wins, as it does in ellmer's own
      # check_finish_reason(). `turns$finish` is NA for a request that failed
      # outright, so this can never override a transport error.
      truncated <- FALSE
      reason <- incomplete_response_reason(
        turns$finish[[j]], turns$usage[j, "output_tokens"]
      )
      if (!is.null(reason)) {
        checked <- list(ok = FALSE, error = reason)
        truncated <- is_truncation(turns$finish[[j]])
      }
      if (isTRUE(checked$ok)) {
        parsed[[i]] <- checked$value
        # NB: `problems[[i]] <- NULL` would DELETE the element and shift every
        # later index down by one, silently misattributing errors to the wrong
        # unit. `problems[i] <- list(NULL)` sets it to NULL in place.
        problems[i] <- list(NULL)
        fatal[[i]] <- FALSE
      } else {
        problems[[i]] <- checked$error
        # "Misconfigured" means the request itself was malformed or
        # unauthorised. A refusal or an over-long document also arrives as a
        # fatal status, but says nothing about the run as a whole, so neither
        # counts towards the all-failed abort below.
        refusal <- is_content_refusal(checked$error)
        length_rejection <- is_length_rejection(checked$error)
        fatal[[i]] <- is_fatal_status(turns$status[[j]]) &&
          !refusal && !length_rejection
        # Content refusals are deliberately retried. They look deterministic
        # and are not: the same document is refused on one pass and coded on
        # the next, at more than one provider. Refusals are rejected before
        # generation and billed at zero tokens, so a wasted attempt is free,
        # whereas dropping the unit discards a coding a retry would have got.
        # Only a context-length rejection is provably unrecoverable.
        #
        # A response cut off at the output limit is not retried either. The
        # same limit reproduces the cut, the retry is billed in full, and a
        # repair prompt cannot supply what the limit withheld: it can only
        # press the model into a shorter answer, which changes the coding
        # rather than repairing it. The fix is the caller's, in
        # params(max_tokens = ), so say so and move on.
        if (!length_rejection && !truncated && !fatal[[i]]) {
          next_pending <- c(next_pending, i)
        }
      }
    }

    # A run that fails in its entirety on the first attempt is misconfigured --
    # a wrong model name, a bad credential -- so stop before spending retries
    # on it. Checked at run level so a content refusal does not masquerade as
    # bad configuration; refusals remain retryable, while over-long documents
    # and other terminal per-unit failures have already left the retry queue.
    if (attempt == 0L && all(vapply(parsed, is.null, logical(1))) && all(fatal)) {
      break
    }
    pending <- next_pending
  }

  # Convert after validation, so that nested and array fields get the same R
  # representations the default parallel_chat_structured() path produces.
  results <- ellmer_convert_from_type(parsed, ellmer::type_array(codebook$schema))

  # Key .error off what actually failed to parse, NOT off `pending`. Terminal
  # failures are deliberately dropped from `pending` so they are not retried,
  # so keying on `pending` would give them a NULL .error and leave them out of
  # the count, making a content refusal indistinguishable from a success.
  failed <- vapply(parsed, is.null, logical(1))

  # Nothing was coded and the provider rejected every request outright: the run
  # is misconfigured, so say so rather than handing back a tibble of NAs.
  if (all(failed) && all(fatal)) {
    cli::cli_abort(c(
      "Every request to model {.val {model}} was rejected by the provider.",
      set_bullets(unique(unlist(problems))),
      "i" = "Check the model name, your credentials, and any {.arg base_url}."
    ), call = error_call)
  }

  if (any(failed)) {
    results$.error <- lapply(seq_along(x), function(i) {
      if (failed[[i]]) {
        simpleError(problems[[i]] %||% "failed for an unrecorded reason")
      } else {
        NULL
      }
    })
    cli::cli_warn(c(
      "{sum(failed)} response{?s} could not be coded, out of {length(x)}.",
      set_bullets(unique(unlist(problems[failed]))),
      "i" = "Their coded values are missing; inspect the {.field .error} column for details."
    ))
  }

  # Same column names and order that ellmer produces for
  # parallel_chat_structured(include_tokens =, include_cost =), so that both
  # paths yield identical schemas.
  if (include_tokens) {
    results$input_tokens <- usage[, "input_tokens"]
    results$output_tokens <- usage[, "output_tokens"]
    results$cached_input_tokens <- usage[, "cached_input_tokens"]
  }
  if (include_cost) {
    results$cost <- usage[, "cost"]
  }

  attr(results, "qlm_backend_meta") <- list(
    backend = "json_mode",
    max_retries = max_retries,
    n_invalid = sum(failed)
  )
  results
}


#' Run prompts in parallel, keeping response text, usage, and failure reasons
#'
#' [ellmer::parallel_chat_text()] would be the natural call, but it reduces each
#' chat to its last turn's text and discards the `Turn`, which is where tokens
#' and cost live. With `on_error = "continue"`, [ellmer::parallel_chat()]
#' returns a list whose failed elements are error conditions rather than chats;
#' the condition carries the real reason (context length, rate limit, timeout),
#' so it is captured here. Without it every failure reads as a bare "empty
#' response", which is useless when triaging a large run.
#'
#' The turn also carries ellmer's normalised finish reason (`"success"`,
#' `"max_tokens"`, `"content_filter"`, ...), which is what distinguishes a
#' response the model completed from one the provider cut off. It is `NA` for
#' a request that failed outright and for providers that do not report one.
#'
#' @param chat An [ellmer::Chat] object.
#' @param prompts List of prompts.
#' @param pc_args List of arguments for [ellmer::parallel_chat()].
#'
#' @return A list with `text` (character), `usage` (numeric matrix of input,
#'   output, and cached input tokens and cost), `error` (character), `status`
#'   (integer HTTP status, `NA` when the failure was not an HTTP error), and
#'   `finish` (character finish reason, `NA` when there is none).
#' @keywords internal
#' @noRd
json_chat_turns <- function(chat, prompts, pc_args) {
  chats <- do.call(ellmer::parallel_chat, c(
    list(chat = chat, prompts = prompts),
    pc_args
  ))

  n <- length(chats)
  text <- rep(NA_character_, n)
  error <- rep(NA_character_, n)
  status <- rep(NA_integer_, n)
  finish <- rep(NA_character_, n)
  usage <- matrix(
    0,
    nrow = n, ncol = 4,
    dimnames = list(NULL, c("input_tokens", "output_tokens",
                            "cached_input_tokens", "cost"))
  )

  for (i in seq_len(n)) {
    if (is.null(chats[[i]]) || inherits(chats[[i]], "error")) {
      error[[i]] <- api_error_message(chats[[i]])
      status[[i]] <- api_error_status(chats[[i]])
      next
    }
    turn <- tryCatch(chats[[i]]$last_turn(), error = function(e) e)
    if (is.null(turn) || inherits(turn, "error")) {
      error[[i]] <- api_error_message(turn)
      status[[i]] <- api_error_status(turn)
      next
    }
    text[[i]] <- turn@text
    finish[[i]] <- turn_finish_reason(turn)
    tokens <- turn@tokens
    if (length(tokens) >= 3L) {
      usage[i, 1:3] <- as.numeric(tokens[1:3])
    }
    usage[i, 4] <- as.numeric(turn@cost)
  }

  list(text = text, usage = usage, error = error, status = status,
       finish = finish)
}


#' The finish reason recorded on a turn
#'
#' ellmer (>= 0.4.2) normalises the provider's stop reason onto an
#' [ellmer::AssistantTurn] as `finish_reason`: `"success"`, `"tool_use"`,
#' `"max_tokens"`, `"context_window"`, `"content_filter"`, or, wrapped in
#' `I()`, a value it did not recognise. Turns from an older ellmer have no such
#' property, and a provider that reports nothing leaves it `NA`; both read as
#' "unknown" here rather than as a failure.
#'
#' @param turn An [ellmer::Turn].
#'
#' @return A single string, or `NA_character_`.
#' @keywords internal
#' @noRd
turn_finish_reason <- function(turn) {
  reason <- tryCatch(turn@finish_reason, error = function(e) NULL)
  if (!is.character(reason) || length(reason) != 1L || is.na(reason)) {
    return(NA_character_)
  }
  as.character(unclass(reason))
}


#' Build the JSON-mode system prompt
#'
#' Names the permitted keys explicitly and forbids JSON Schema vocabulary in the
#' answer. Without this, models echo schema keywords into the output or rename
#' properties, which the validator catches but only at the cost of a retry. This
#' is a formatting instruction only: it names no coding criterion, so the
#' substantive task stays identical across models.
#'
#' @param codebook A `qlm_codebook` object.
#'
#' @return A character string.
#' @keywords internal
#' @noRd
json_system_prompt <- function(codebook) {
  task_prompt <- if (!is.null(codebook$role)) {
    paste(codebook$role, codebook$instructions, sep = "\n\n")
  } else {
    codebook$instructions
  }

  schema <- json_schema_from_type(codebook$schema)
  keys <- names(schema$properties)
  key_note <- if (length(keys)) {
    paste0(
      "The object must have exactly these top-level keys, spelled precisely: ",
      paste0("\"", keys, "\"", collapse = ", "), ".\n",
      "Do not rename them, do not add any others, and do not include JSON Schema ",
      "keywords such as \"type\", \"properties\", \"required\", \"description\" or ",
      "\"additionalProperties\" in your answer. The schema below describes the ",
      "shape of the answer; it is not itself the answer."
    )
  } else {
    NULL
  }

  paste(
    task_prompt,
    "Return exactly one valid JSON object and no Markdown, explanation, or text outside that object.",
    "The object must satisfy this JSON Schema. Include every required property and do not add properties when additionalProperties is false.",
    key_note,
    jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    sep = "\n\n"
  )
}


#' Build a repair prompt for a response that failed validation
#'
#' @param text The original text unit.
#' @param problem The validation error message.
#'
#' @return A character string.
#' @keywords internal
#' @noRd
json_repair_prompt <- function(text, problem) {
  paste(
    "Your prior response could not be accepted:",
    problem,
    "Return a corrected answer for the following text.",
    "Return only one JSON object; do not use Markdown or explain the answer.",
    text,
    sep = "\n\n"
  )
}


#' Convert an ellmer type specification to JSON Schema
#'
#' Covers the subset of JSON Schema needed for prompting. The ellmer type object
#' remains the source of truth for validation.
#'
#' @param type An ellmer type object.
#'
#' @return A list suitable for [jsonlite::toJSON()].
#' @keywords internal
#' @noRd
json_schema_from_type <- function(type) {
  if (!inherits(type, "ellmer::Type")) {
    cli::cli_abort(c(
      "Unsupported schema type: {.cls {class(type)[[1]]}}.",
      "i" = "Use {.fn ellmer::type_object}, {.fn ellmer::type_array}, {.fn ellmer::type_enum}, or a basic type."
    ))
  }

  description <- type@description
  description_field <- if (length(description) == 1L && nzchar(description)) {
    list(description = description)
  } else {
    list()
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    return(c(list(type = type@type), description_field))
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    return(c(list(type = "string", enum = as.list(type@values)), description_field))
  }
  if (inherits(type, "ellmer::TypeArray")) {
    return(c(
      list(type = "array", items = json_schema_from_type(type@items)),
      description_field
    ))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    properties <- lapply(type@properties, json_schema_from_type)
    required <- names(type@properties)[
      vapply(type@properties, function(p) isTRUE(p@required), logical(1))
    ]
    return(c(
      list(
        type = "object",
        properties = properties,
        required = as.list(required),
        additionalProperties = type@additional_properties
      ),
      description_field
    ))
  }

  cli::cli_abort(c(
    "Unsupported schema type: {.cls {class(type)[[1]]}}.",
    "i" = "Use {.fn ellmer::type_object}, {.fn ellmer::type_array}, {.fn ellmer::type_enum}, or a basic type."
  ))
}


#' Parse and validate a JSON-mode response
#'
#' @param text The raw response text.
#' @param schema An ellmer type object.
#'
#' @return A list with `ok`, and either `value` (the validated JSON list) or
#'   `error` (a message naming the offending JSON path).
#' @keywords internal
#' @noRd
parse_and_validate_json <- function(text, schema) {
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(trimws(text))) {
    return(list(ok = FALSE, error = "The API returned an empty response."))
  }

  value <- tryCatch(
    jsonlite::fromJSON(strip_code_fence(text), simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    return(list(ok = FALSE, error = paste0("Invalid JSON: ", conditionMessage(value))))
  }
  if (!is.list(value) || is.null(names(value))) {
    return(list(ok = FALSE, error = "JSON output must be an object."))
  }

  checked <- tryCatch(
    validate_against_type(value, schema, path = "$"),
    error = function(e) e
  )
  if (inherits(checked, "error")) {
    return(list(ok = FALSE, error = conditionMessage(checked)))
  }
  list(ok = TRUE, value = checked)
}


#' Validate a parsed JSON value against an ellmer type specification
#'
#' Returns the value unchanged; conversion happens once, after every valid
#' record has passed the same deterministic checks. Signals with `stop()` rather
#' than `cli::cli_abort()` because the condition is caught by
#' `parse_and_validate_json()` and reported as a coding failure, not raised to
#' the user.
#'
#' @param value A parsed JSON value.
#' @param type An ellmer type object.
#' @param path JSON path of `value`, used in error messages.
#'
#' @return `value`, unchanged.
#' @keywords internal
#' @noRd
validate_against_type <- function(value, type, path) {
  if (is.null(value)) {
    if (isTRUE(type@required)) {
      stop(path, " is required and cannot be null.", call. = FALSE)
    }
    return(NULL)
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    valid <- switch(type@type,
      string = is.character(value) && length(value) == 1L && !is.na(value),
      boolean = is.logical(value) && length(value) == 1L && !is.na(value),
      integer = is.numeric(value) && length(value) == 1L && is.finite(value) &&
        value == trunc(value) && abs(value) <= .Machine$integer.max,
      number = is.numeric(value) && length(value) == 1L && is.finite(value),
      FALSE
    )
    if (!isTRUE(valid)) {
      stop(path, " must be a ", type@type, ".", call. = FALSE)
    }
    return(value)
  }

  if (inherits(type, "ellmer::TypeEnum")) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !value %in% type@values) {
      stop(path, " must be one of: ", paste(type@values, collapse = ", "), ".",
           call. = FALSE)
    }
    return(value)
  }

  if (inherits(type, "ellmer::TypeArray")) {
    if (!is.list(value) || !is.null(names(value))) {
      stop(path, " must be a JSON array.", call. = FALSE)
    }
    return(lapply(seq_along(value), function(i) {
      validate_against_type(value[[i]], type@items, paste0(path, "[", i, "]"))
    }))
  }

  if (inherits(type, "ellmer::TypeObject")) {
    if (!is.list(value) || is.null(names(value))) {
      stop(path, " must be a JSON object.", call. = FALSE)
    }
    property_names <- names(type@properties)
    extras <- setdiff(names(value), property_names)
    if (!isTRUE(type@additional_properties) && length(extras)) {
      stop(path, " has unexpected propert", if (length(extras) == 1L) "y: " else "ies: ",
           paste(extras, collapse = ", "), ".", call. = FALSE)
    }
    output <- vector("list", length(type@properties))
    names(output) <- property_names
    for (property in property_names) {
      property_type <- type@properties[[property]]
      if (!property %in% names(value)) {
        if (isTRUE(property_type@required)) {
          stop(path, ".", property, " is required but missing.", call. = FALSE)
        }
        output[property] <- list(NULL)
      } else {
        output[[property]] <- validate_against_type(
          value[[property]], property_type, paste0(path, ".", property)
        )
      }
    }
    if (isTRUE(type@additional_properties) && length(extras)) {
      output[extras] <- value[extras]
    }
    return(output)
  }

  stop(path, " uses an unsupported schema type.", call. = FALSE)
}


#' Strip a Markdown code fence from a response
#'
#' Reasoning models in JSON mode still occasionally wrap the object in a fence.
#' Stripping it is deterministic and concerns only the output format, so it
#' costs nothing and avoids a paid retry. Anything else is left to validation.
#'
#' @param text A character string.
#'
#' @return `text` without its surrounding code fence.
#' @keywords internal
#' @noRd
strip_code_fence <- function(text) {
  trimmed <- trimws(text)
  if (!grepl("^```", trimmed)) {
    return(trimmed)
  }
  trimmed <- sub("^```[[:alnum:]_-]*[[:space:]]*\n", "", trimmed)
  trimmed <- sub("\n[[:space:]]*```[[:space:]]*$", "", trimmed)
  trimws(trimmed)
}


#' Describe a failed request in the provider's own words
#'
#' httr2 reports an HTTP failure as a bare status line ("HTTP 400 Bad
#' Request."), because it will only parse a response body served as JSON.
#' DeepSeek returns the useful part -- "The supported API model names are ...,
#' but you passed deepseek-v3-flash." -- as `application/octet-stream`, so it
#' never reaches the condition message and a mistyped model name reads as a
#' generic failure. The body is on the condition, so read it directly.
#'
#' @param cnd An error condition, or `NULL`.
#'
#' @return A single string.
#' @keywords internal
#' @noRd
api_error_message <- function(cnd) {
  if (is.null(cnd)) {
    return("the request failed")
  }
  message <- strip_ansi(tryCatch(
    conditionMessage(cnd),
    error = function(e) "the request failed"
  ))
  detail <- api_error_detail(cnd)
  if (is.na(detail)) {
    return(message)
  }
  paste(message, detail)
}


#' Extract the provider's error message from a response body
#'
#' @param cnd An error condition.
#'
#' @return The provider's message, or `NA_character_` if there is none.
#' @keywords internal
#' @noRd
api_error_detail <- function(cnd) {
  body <- cnd$resp$body
  if (!is.raw(body) || !length(body)) {
    return(NA_character_)
  }
  text <- tryCatch(rawToChar(body), error = function(e) NULL)
  if (is.null(text) || !nzchar(trimws(text))) {
    return(NA_character_)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(parsed)) {
    return(NA_character_)
  }
  # OpenAI-compatible errors usually nest the message under `error`, but some
  # providers return a scalar `error` string or put `message` at the top level.
  nested_error <- parsed$error
  nested_detail <- if (is.list(nested_error)) {
    nested_error$message
  } else if (is.character(nested_error) && length(nested_error) == 1L &&
             nzchar(nested_error)) {
    nested_error
  } else {
    NULL
  }
  detail <- nested_detail %||% parsed$message
  if (!is.character(detail) || length(detail) != 1L || !nzchar(detail)) {
    return(NA_character_)
  }
  strip_ansi(detail)
}


#' HTTP status of a failed request
#'
#' @param cnd An error condition.
#'
#' @return An integer status, or `NA_integer_` for a non-HTTP failure.
#' @keywords internal
#' @noRd
api_error_status <- function(cnd) {
  status <- cnd$status
  if (!is.numeric(status) || length(status) != 1L) {
    return(NA_integer_)
  }
  as.integer(status)
}


#' Is an HTTP status one the provider will keep rejecting?
#'
#' A bad model name, bad credentials, or a malformed request will fail
#' identically on every retry, so these are not worth re-sending. Rate limits
#' (429) and server errors (5xx) are transient and deliberately excluded.
#'
#' @param status An HTTP status, possibly `NA`.
#'
#' @return `TRUE` if retrying is pointless.
#' @keywords internal
#' @noRd
is_fatal_status <- function(status) {
  !is.na(status) && status %in% c(400L, 401L, 403L, 404L, 422L)
}


#' Format messages as cli bullets, without treating them as templates
#'
#' Provider error text is data, not a cli template: braces in it must not be
#' interpolated. Doubling them makes cli emit them literally.
#'
#' @param x A character vector of messages.
#'
#' @return A character vector named `"x"`, truncated to the first three.
#' @keywords internal
#' @noRd
set_bullets <- function(x, n = 3L) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) {
    return(character())
  }
  if (length(x) > n) {
    x <- c(x[seq_len(n)], paste0("... and ", length(x) - n, " other reason(s)"))
  }
  x <- gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
  stats::setNames(x, rep("x", length(x)))
}


#' Did the provider refuse the input on content grounds?
#'
#' Used only to keep a refusal from counting as a misconfigured run. It must
#' never gate a retry: refusals are non-deterministic, and the same document is
#' refused on one pass and coded on the next.
#'
#' @param msg An error message.
#'
#' @return `TRUE` for a content-filter refusal.
#' @keywords internal
#' @noRd
is_content_refusal <- function(msg) {
  if (!length(msg) || is.na(msg)) {
    return(FALSE)
  }
  grepl(
    paste0("DataInspectionFailed|inappropriate content|content[ _-]?polic|",
           "content[ _-]?filter|considered high risk|safety"),
    msg,
    ignore.case = TRUE
  )
}


#' Did the provider reject the input as too long?
#'
#' The one per-unit failure that is provably unrecoverable: a document longer
#' than the model's context window will be rejected identically however often
#' it is re-sent, so retrying only wastes an attempt.
#'
#' Content refusals are deliberately *not* treated this way. They present as
#' the same class of error and look deterministic, but are not: the same
#' document is refused on one pass and coded on the next, at more than one
#' independently operated provider. Treating them as unrecoverable would
#' permanently discard a unit that a further attempt would have coded.
#'
#' @param msg An error message.
#'
#' @return `TRUE` if retrying cannot succeed.
#' @keywords internal
#' @noRd
is_length_rejection <- function(msg) {
  if (!length(msg) || is.na(msg)) {
    return(FALSE)
  }
  grepl(
    paste0(
      "maximum context length|context[ -]?length|context window|",
      "range of input length|(?:input|prompt|document)(?: is|'s)? too long|",
      "exceeds?.{0,40}(?:token|context)|too many (?:input )?tokens"
    ),
    msg,
    ignore.case = TRUE,
    perl = TRUE
  )
}


#' Why an intact response is nonetheless unusable
#'
#' A response can arrive with HTTP 200 and still not be the model's answer:
#' the provider cut it off at the output limit, or withheld it. The finish
#' reason says which, and is the right thing to record, because the parse
#' error it produces ("premature EOF") describes the symptom and points at the
#' wrong remedy. Wording follows ellmer's own `check_finish_reason()`, which
#' raises the same conditions for a sequential `chat_structured()` call.
#'
#' @param finish A finish reason, as returned by `json_chat_turns()`.
#' @param output_tokens Output tokens spent on the response, for the message.
#'
#' @return A single string, or `NULL` when the finish reason does not explain
#'   a failure (the response completed, or no reason was reported).
#' @keywords internal
#' @noRd
incomplete_response_reason <- function(finish, output_tokens = NA_real_) {
  if (!length(finish) || is.na(finish) ||
      finish %in% c("success", "tool_use", "stop_sequence")) {
    return(NULL)
  }
  spent <- if (length(output_tokens) == 1L && is.finite(output_tokens) &&
               output_tokens > 0) {
    paste0(" after ", format(output_tokens, big.mark = ","), " output tokens")
  } else {
    ""
  }
  switch(finish,
    max_tokens = paste0(
      "The response was cut off at the max_tokens limit", spent,
      "; raise the limit with params(max_tokens = ) to let the model finish."
    ),
    context_window = paste0(
      "The response was cut off because the input and output together ",
      "exceeded the model's context window."
    ),
    content_filter = "The response was withheld by the provider's content filter.",
    paste0(
      "The response may be incomplete: the provider reported finish reason \"",
      finish, "\"."
    )
  )
}


#' Was the response cut short by a limit that a retry cannot lift?
#'
#' @param finish A finish reason, as returned by `json_chat_turns()`.
#'
#' @return `TRUE` for a response cut off at `max_tokens` or at the context
#'   window.
#' @keywords internal
#' @noRd
is_truncation <- function(finish) {
  length(finish) == 1L && !is.na(finish) &&
    finish %in% c("max_tokens", "context_window")
}


#' Remove ANSI escape sequences
#'
#' cli formats API errors with ANSI colour codes, which would otherwise travel
#' inside the `.error` column into CSVs and printed reports as unreadable
#' escape sequences.
#'
#' @param x A character vector.
#'
#' @return `x` without ANSI escapes.
#' @keywords internal
#' @noRd
strip_ansi <- function(x) {
  if (!length(x)) {
    return(x)
  }
  gsub("\033\\[[0-9;]*[A-Za-z]", "", x)
}


#' Convert validated JSON lists to the result tibble
#'
#' Deliberately calls ellmer's internal converter rather than reimplementing it.
#' The invariant that matters is that this path returns the same shape as the
#' default [ellmer::parallel_chat_structured()] path, and that path converts
#' through this same function, so calling it makes the identity true by
#' construction. A local copy would hold only as long as it was kept in step,
#' and would diverge silently, which is the failure mode this handler exists to
#' eliminate.
#'
#' Reached through [utils::getFromNamespace()] rather than `ellmer:::`, which
#' `R CMD check` reports as a NOTE. The dependency is identical either way; this
#' form resolves it on each call, so updating ellmer without reinstalling
#' quallmer picks up the new converter rather than a copy frozen at install
#' time. If ellmer ever exports an equivalent, this helper is the only thing
#' that needs to change.
#'
#' @param x List of validated JSON values, one per unit.
#' @param type An [ellmer::type_array()] over the codebook schema.
#'
#' @return A tibble with one row per unit.
#' @keywords internal
#' @noRd
ellmer_convert_from_type <- function(x, type) {
  converter <- tryCatch(
    utils::getFromNamespace("convert_from_type", "ellmer"),
    error = function(e) NULL
  )
  if (!is.function(converter)) {
    cli::cli_abort(c(
      "This version of {.pkg ellmer} does not provide {.fn convert_from_type}.",
      "i" = "Coding with a JSON-mode provider needs it to build the result table.",
      "i" = "Please report this at {.url https://github.com/quallmer/quallmer/issues}."
    ))
  }
  converter(x, type)
}


#' Required properties whose absence is detectable in the result table
#'
#' Restricted to required scalar properties, because those are the ones
#' [ellmer::parallel_chat_structured()] renders as a single column that can be
#' checked for `NA`. Arrays and nested objects become list-columns, where "the
#' model returned nothing useful" has no simple representation.
#'
#' @param schema An [ellmer::type_object()].
#'
#' @return A character vector of property names.
#' @keywords internal
#' @noRd
required_scalar_fields <- function(schema) {
  if (!inherits(schema, "ellmer::TypeObject")) {
    return(character())
  }
  properties <- schema@properties
  keep <- vapply(properties, function(p) {
    isTRUE(p@required) && inherits(p, c("ellmer::TypeBasic", "ellmer::TypeEnum"))
  }, logical(1))
  names(properties)[keep]
}


#' Did the structured call return nothing usable at all?
#'
#' A structured call can succeed at the HTTP level and still return a table in
#' which every required field is `NA` in every row: the endpoint accepted the
#' JSON schema and ignored it, so ellmer had nothing to map onto the type and
#' emitted `NA` rather than an error. Observed with `qwen3.5-397b-a17b` through
#' Alibaba Model Studio. Erroring is therefore not a sufficient test of whether
#' the structured path worked.
#'
#' @param results The result of [ellmer::parallel_chat_structured()].
#' @param schema An [ellmer::type_object()].
#'
#' @return `TRUE` when the call produced no usable values.
#' @keywords internal
#' @noRd
all_required_missing <- function(results, schema) {
  # A user may have passed convert = FALSE, in which case there is no table to
  # inspect and no basis for calling the attempt a failure.
  if (!is.data.frame(results) || !nrow(results)) {
    return(FALSE)
  }
  fields <- intersect(required_scalar_fields(schema), names(results))
  if (!length(fields)) {
    return(FALSE)
  }
  # A row with a recorded error is evidence only if the endpoint answered. A
  # rejected request, or a response cut off at max_tokens, says nothing about
  # whether the endpoint honours the schema, so those rows are set aside. A
  # response ellmer could extract nothing from -- prose where JSON was asked
  # for -- is exactly what an endpoint that ignored the schema produces, so
  # those rows are judged alongside the intact ones. If nothing is left to
  # judge, there is nothing to conclude.
  judged <- vapply(
    recorded_errors(results),
    function(e) is.null(e) || is_extraction_error(e),
    logical(1)
  )
  if (!any(judged)) {
    return(FALSE)
  }
  all(vapply(fields, function(f) all(is.na(results[[f]][judged])), logical(1)))
}


#' How many rows are missing at least one required field?
#'
#' Partial failure, which is worth reporting but is not grounds for discarding
#' the whole attempt and re-coding in JSON mode. Rows carrying an `.error` are
#' not counted: they are reported as failed already, with their reason.
#'
#' @param results The result of [ellmer::parallel_chat_structured()].
#' @param schema An [ellmer::type_object()].
#'
#' @return An integer count.
#' @keywords internal
#' @noRd
n_incomplete <- function(results, schema) {
  if (!is.data.frame(results) || !nrow(results)) {
    return(0L)
  }
  fields <- intersect(required_scalar_fields(schema), names(results))
  if (!length(fields)) {
    return(0L)
  }
  missing <- Reduce(`|`, lapply(fields, function(f) is.na(results[[f]])))
  sum(missing & !errored_rows(results))
}


#' Does this provider enforce the output schema by construction?
#'
#' Derived from ellmer's own dispatch rather than from a list of vendors.
#' These provider classes define their own `chat_body()` method using a
#' mechanism the provider is documented to enforce: OpenAI's `/responses`
#' format with `strict = TRUE`, Anthropic's native structured output or a
#' forced tool call, Gemini's `response_schema`, Bedrock's forced tool call,
#' Snowflake's typed `response_format`.
#'
#' Everything else falls through to `ProviderOpenAICompatible`'s method, which
#' sends `response_format = {type: "json_schema", strict: true}` and takes the
#' answer on trust. Whether that is honoured is up to the endpoint, and
#' measurement shows it often is not.
#'
#' Deriving this rather than tabulating it means a provider ellmer adds later
#' defaults to "cannot vouch for this", which is the safe direction.
#'
#' @param provider A provider object from `chat$get_provider()`.
#'
#' @return `TRUE` when the schema is enforced by the provider's mechanism.
#' @keywords internal
#' @noRd
provider_enforces_schema <- function(provider) {
  enforcing <- c(
    "ellmer::ProviderOpenAI",
    "ellmer::ProviderAnthropic",
    "ellmer::ProviderGoogleGemini",
    "ellmer::ProviderAWSBedrock",
    "ellmer::ProviderSnowflakeCortex"
  )
  any(class(provider) %in% enforcing)
}


#' Is this the DashScope "messages must contain the word json" rejection?
#'
#' Alibaba Model Studio rejects any request that sets `response_format` unless
#' the word "json" appears somewhere in the messages, and
#' [ellmer::parallel_chat_structured()] sets `response_format` from the schema.
#' The structured call would therefore fail for a reason that has nothing to do
#' with the codebook, on an endpoint that does enforce the schema once the
#' request is accepted. Recognising it lets us satisfy the requirement and keep
#' the enforcement, rather than falling back and losing it.
#'
#' NOT OBSERVED against ellmer's current request shape. The rejection was seen
#' on 2026-08-12 with `response_format` of type `json_object`; ellmer sends type
#' `json_schema`, which Model Studio accepted without complaint when this was
#' checked live on 2026-09-02. This is therefore defensive: if the requirement
#' does apply, one retry preserves enforcement; if it never fires, nothing
#' happens. Do not read its presence as evidence that the quirk is live.
#'
#' @param msg An error message.
#'
#' @return `TRUE` when the request needs the word "json" in its prompt.
#' @keywords internal
#' @noRd
is_json_word_error <- function(msg) {
  if (!length(msg) || is.na(msg)) {
    return(FALSE)
  }
  grepl("must contain the word ['\"]?json", msg, ignore.case = TRUE)
}
