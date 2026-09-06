#' Per-unit records from a run's turns
#'
#' Both coding paths end with one record per input: what the model answered,
#' what the request cost, and why it failed if it did. The JSON path reads a
#' list of chats from [ellmer::parallel_chat()]; the structured path reads
#' the turns `structured_chat_turns()` returns. Either way the elements are
#' the same three things: a turn (or a chat holding one), an error condition
#' for a request the provider refused, or `NULL` for a request never sent
#' after an earlier failure stopped the run.
#'
#' The turn carries ellmer's normalised finish reason (`"success"`,
#' `"max_tokens"`, `"content_filter"`, ...), which is what distinguishes a
#' response the model completed from one the provider cut off. It is `NA`
#' for a request that failed outright and for providers that report none.
#'
#' @param results A list of [ellmer::Chat] objects, [ellmer::AssistantTurn]s,
#'   error conditions and `NULL`s, one per input.
#'
#' @return A list of parallel vectors, each indexed as `results`: `turn`
#'   (list of turns, `NULL` where there is none), `text` (character), `usage`
#'   (numeric matrix of input, output and cached input tokens and cost; zero
#'   for a request that was refused or never sent, `NA` where the provider
#'   reported nothing), `error` (character, `NA` where the request
#'   succeeded), `status` (integer HTTP status, `NA` when the failure was
#'   not an HTTP error) and `finish` (character finish reason).
#' @keywords internal
#' @noRd
turn_records <- function(results) {
  n <- length(results)
  turn <- vector("list", n)
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
    item <- results[[i]]
    if (is.null(item) || inherits(item, "error")) {
      error[[i]] <- api_error_message(item)
      status[[i]] <- api_error_status(item)
      next
    }
    # A chat from parallel_chat() holds its turn; anything else is one
    if (!inherits(item, "ellmer::Turn")) {
      item <- tryCatch(item$last_turn(), error = function(e) e)
      if (is.null(item) || inherits(item, "error")) {
        error[[i]] <- api_error_message(item)
        status[[i]] <- api_error_status(item)
        next
      }
    }
    turn[i] <- list(item)
    text[[i]] <- item@text
    finish[[i]] <- turn_finish_reason(item)
    tokens <- item@tokens
    if (length(tokens) >= 3L) {
      usage[i, 1:3] <- as.numeric(tokens[1:3])
    }
    usage[i, 4] <- as.numeric(item@cost)
  }

  list(turn = turn, text = text, usage = usage, error = error,
       status = status, finish = finish)
}


#' The structured value each turn carries, unconverted
#'
#' Follows ellmer's own extraction: the first JSON content of the turn, with a
#' warning when there are several, unwrapped where ellmer wrapped the type for
#' the provider. The value is exactly what the provider sent, parsed and
#' nothing more, so that validation sees a wrong type, an extra property or a
#' missing array before conversion can hide it (#140).
#'
#' @param records What `turn_records()` returned.
#' @param needs_wrapper Whether the answer arrives as `{"wrapper": <value>}`,
#'   from `structured_needs_wrapper()`.
#'
#' @return `records`, with `value` (a list, `NULL` where nothing could be
#'   extracted) and `problem` (character, the extraction failure, `NA`
#'   otherwise) added.
#' @keywords internal
#' @noRd
add_structured_values <- function(records, needs_wrapper = FALSE) {
  n <- length(records$turn)
  value <- vector("list", n)
  problem <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    turn <- records$turn[[i]]
    if (is.null(turn)) {
      next
    }
    extracted <- extract_structured_value(turn, needs_wrapper)
    if (isTRUE(extracted$ok)) {
      value[i] <- list(extracted$value)
    } else {
      problem[[i]] <- extracted$error
    }
  }
  records$value <- value
  records$problem <- problem
  records
}


#' Extract the parsed JSON from one assistant turn
#'
#' @param turn An [ellmer::AssistantTurn].
#' @param needs_wrapper As in `add_structured_values()`.
#'
#' @return A list with `ok`, and either `value` or `error`.
#' @keywords internal
#' @noRd
extract_structured_value <- function(turn, needs_wrapper = FALSE) {
  contents <- turn@contents
  is_json <- vapply(contents, inherits, logical(1), "ellmer::ContentJson")
  n <- sum(is_json)
  if (n == 0L) {
    return(list(ok = FALSE, error = "Data extraction failed: no JSON responses found."))
  }
  if (n > 1L) {
    cli::cli_warn("Found {n} JSON responses, using the first.")
  }
  content <- contents[[which(is_json)[[1]]]]
  # Parsed lazily from the response text, so a malformed document fails here
  value <- tryCatch(content@parsed, error = function(e) e)
  if (inherits(value, "error")) {
    return(list(ok = FALSE, error = paste0("Invalid JSON: ", conditionMessage(value))))
  }
  if (needs_wrapper) {
    if (!is.list(value) || !"wrapper" %in% names(value)) {
      return(list(ok = FALSE, error = "Data extraction failed: no \"wrapper\" property in the response."))
    }
    value <- value[["wrapper"]]
  }
  list(ok = TRUE, value = value)
}


#' Settle what one response is: usable, or failed at which stage
#'
#' The one order of precedence both paths apply. A request that failed is
#' reported with the provider's reason rather than as "empty response". A
#' response the provider itself reports as incomplete -- cut off at
#' `max_tokens`, withheld by a content filter -- is a failure whatever the
#' text looks like: cut-off JSON usually fails to parse, and then the
#' provider's reason is the one to record; an object that happened to close
#' just before the limit parses cleanly, and the provider's word still wins,
#' as it does in ellmer's own `check_finish_reason()`. `finish` is `NA` for a
#' request that failed outright, so this never overrides a transport error.
#' Only a completed response is checked against the schema.
#'
#' @param checked What `validate_structured_value()` returned for the parsed
#'   value, or `NULL` when there was nothing to parse.
#' @param problem The extraction or parse failure, `NA` when there was none.
#' @param error The transport failure, `NA` when the request succeeded.
#' @param finish The finish reason, `NA` when unknown.
#' @param output_tokens Reported output tokens, for the message.
#'
#' @return A list with `ok`; `value` when usable; otherwise `error` (the
#'   message) and `stage`: `"transport"`, `"incomplete"`, `"extraction"` or
#'   `"schema"`. `truncated` says whether an incomplete response was cut off
#'   at an output limit, which a backfill treats as terminal.
#' @keywords internal
#' @noRd
settle_response <- function(checked, problem, error, finish,
                            output_tokens = NA_real_) {
  if (!is.na(error)) {
    return(list(ok = FALSE, stage = "transport", truncated = FALSE,
                error = paste0("API request failed: ", error)))
  }
  reason <- incomplete_response_reason(finish, output_tokens)
  if (!is.null(reason)) {
    return(list(ok = FALSE, stage = "incomplete", error = reason,
                truncated = is_truncation(finish)))
  }
  if (!is.na(problem)) {
    return(list(ok = FALSE, stage = "extraction", error = problem,
                truncated = FALSE))
  }
  if (is.null(checked)) {
    return(list(ok = FALSE, stage = "extraction", truncated = FALSE,
                error = "The API returned an empty response."))
  }
  if (!isTRUE(checked$ok)) {
    return(list(ok = FALSE, stage = "schema", error = checked$error,
                truncated = FALSE))
  }
  list(ok = TRUE, value = checked$value, stage = "ok", truncated = FALSE)
}


#' The condition recorded for a failed unit
#'
#' Classed by stage, so that later decisions read the class rather than the
#' message: a backfill leaves a truncated unit alone, and the structured path
#' judges whether an endpoint honours the schema from the responses that
#' reached it.
#'
#' @param message The failure message.
#' @param stage,truncated As `settle_response()` returns them.
#'
#' @return A condition inheriting from `simpleError`.
#' @keywords internal
#' @noRd
unit_error <- function(message, stage, truncated = FALSE) {
  message <- message %||% "failed for an unrecorded reason"
  if (isTRUE(truncated)) {
    return(truncation_error(message))
  }
  switch(stage,
    schema = schema_error(message),
    extraction = extraction_error(message),
    simpleError(message)
  )
}


#' An error recorded for a response that did not match the schema
#'
#' The endpoint answered with JSON, and the JSON was not the schema: a wrong
#' type, a missing required property, an extra one. Inherits from the
#' extraction error, since both say the same thing about the endpoint -- it
#' answered, and the answer was not the schema -- which is the evidence the
#' fallback decision reads.
#'
#' @param message The validator's message, naming the JSON path.
#'
#' @return A condition inheriting from `quallmer_extraction_error`.
#' @keywords internal
#' @noRd
schema_error <- function(message) {
  structure(
    simpleError(message),
    class = c("quallmer_schema_error", "quallmer_extraction_error",
              "simpleError", "error", "condition")
  )
}

is_schema_error <- function(e) {
  inherits(e, "quallmer_schema_error")
}


#' Turn validated values into the result table
#'
#' Converts once, after validation, so that nested and array fields get the
#' same R representations on both paths. A failed unit is a `NULL` in
#' `values`, which ellmer's converter renders as a row of `NA`, and carries
#' its condition in `.error`. Column order follows ellmer: the coded values,
#' then `.error`, then any usage columns.
#'
#' @param values A list of validated values, `NULL` for a failed unit.
#' @param errors A list of conditions, `NULL` for a unit that succeeded.
#' @param usage The usage matrix from `turn_records()`, summed over attempts.
#' @param schema The codebook schema.
#' @param include_tokens,include_cost Whether to attach the usage columns.
#'
#' @return A tibble with one row per unit.
#' @keywords internal
#' @noRd
tabulate_results <- function(values, errors, usage, schema,
                             include_tokens = FALSE, include_cost = FALSE) {
  results <- ellmer_convert_from_type(values, ellmer::type_array(schema))
  if (!all(vapply(errors, is.null, logical(1)))) {
    results$.error <- errors
  }
  if (include_tokens) {
    results$input_tokens <- usage[, "input_tokens"]
    results$output_tokens <- usage[, "output_tokens"]
    results$cached_input_tokens <- usage[, "cached_input_tokens"]
  }
  if (include_cost) {
    results$cost <- usage[, "cost"]
  }
  results
}
