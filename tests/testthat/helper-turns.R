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
