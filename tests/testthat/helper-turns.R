# Assistant turns as ellmer builds them from a provider's response, for tests
# that exercise the structured path without a network. A turn carries the
# JSON the provider returned, its token counts, cost and finish reason, so a
# test can stage a valid answer, a malformed one, a truncated one and a
# refused request at chosen positions and check that each survives.

# A structured answer. `data` is the parsed value; `string` is the raw JSON
# text, which ellmer parses lazily and which keeps `{}` and `[]` distinct.
json_turn <- function(data = NULL, string = NULL, tokens = c(10, 5, 0),
                      cost = 0.001, finish_reason = "success") {
  content <- utils::getFromNamespace("ContentJson", "ellmer")(
    data = data, string = string
  )
  ellmer::AssistantTurn(
    list(content), tokens = tokens, cost = cost, finish_reason = finish_reason
  )
}

# A response with no structured content at all: prose where JSON was asked for.
text_turn <- function(text, tokens = c(10, 5, 0), cost = 0.001,
                      finish_reason = "success") {
  ellmer::AssistantTurn(
    list(ellmer::ContentText(text)),
    tokens = tokens, cost = cost, finish_reason = finish_reason
  )
}

# A request the provider refused, in the shape httr2 raises it: the status is
# what quallmer reads to tell a misconfiguration from a transient failure.
request_error <- function(message = "HTTP 500 Internal Server Error.",
                          status = 500L) {
  structure(
    list(message = message, status = status, call = NULL),
    class = c(paste0("httr2_http_", status), "httr2_http", "httr2_error",
              "rlang_error", "error", "condition")
  )
}

# An ellmer chat that never connects: credentials are supplied so that no
# environment variable is consulted, and nothing is sent.
offline_chat <- function(model = "openai/gpt-4.1-mini", ...) {
  ellmer::chat(model, credentials = function() list(Authorization = "Bearer x"),
               ...)
}

# Turns from a table of expected rows, for tests written against the table
# ellmer used to hand back. Each row becomes a JSON turn carrying the schema
# columns, with the usage columns as its tokens and cost when they are
# there, and a row whose `.error` is set becomes that condition in the
# turn's place. An NA scalar is sent as JSON null, which is what a
# provider that left the field out would produce.
rows_as_turns <- function(rows) {
  usage_cols <- c("input_tokens", "output_tokens", "cached_input_tokens")
  meta <- c(".error", usage_cols, "cost")
  errors <- if (".error" %in% names(rows)) {
    as.list(rows$.error)
  } else {
    vector("list", nrow(rows))
  }
  fields <- setdiff(names(rows), meta)
  lapply(seq_len(nrow(rows)), function(i) {
    if (!is.null(errors[[i]])) {
      return(errors[[i]])
    }
    data <- lapply(fields, function(col) row_value(rows[[col]], i))
    names(data) <- fields
    tokens <- if (all(usage_cols %in% names(rows))) {
      as.numeric(c(rows$input_tokens[[i]], rows$output_tokens[[i]],
                   rows$cached_input_tokens[[i]]))
    } else {
      c(10, 5, 0)
    }
    cost <- if ("cost" %in% names(rows)) as.numeric(rows$cost[[i]]) else 0.001
    json_turn(data, tokens = tokens, cost = cost)
  })
}

row_value <- function(column, i) {
  if (is.data.frame(column)) {
    return(lapply(column, row_value, i = i))
  }
  if (is.list(column)) {
    v <- column[[i]]
    if (is.data.frame(v)) {
      return(lapply(seq_len(nrow(v)), function(r) lapply(v, row_value, i = r)))
    }
    return(as.list(v))
  }
  v <- column[[i]]
  if (is.na(v)) {
    return(NULL)
  }
  if (is.factor(v)) as.character(v) else v
}

# A stand-in for structured_chat_turns(): answers with `results`, which may
# be a list of turns, a table for rows_as_turns(), or a function of the
# execution arguments returning either. Records each call in `calls`.
turns_stub <- function(results, calls = NULL) {
  function(chat, prompts, type, batch = FALSE, execution_args = list()) {
    if (!is.null(calls)) {
      calls$n <- (calls$n %||% 0L) + 1L
      calls$dots <- execution_args
      calls$n_prompts <- length(prompts)
      calls$batch <- batch
    }
    out <- if (is.function(results)) results(execution_args) else results
    if (is.data.frame(out)) rows_as_turns(out) else out
  }
}
