#' Is this an ellmer tool object?
#'
#' ellmer has no shared parent class for tools: a custom tool from
#' [ellmer::tool()] is an `ellmer::ToolDef`, a provider's built-in such as
#' [ellmer::openai_tool_web_search()] an `ellmer::ToolBuiltIn`. Both are
#' accepted by `chat$register_tool()`, and both are checked here, in one
#' place, so that a class ellmer adds later has one line to change.
#'
#' @param x Any value.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
is_ellmer_tool <- function(x) {
  inherits(x, "ellmer::ToolDef") || inherits(x, "ellmer::ToolBuiltIn")
}


#' Check and normalise the `tools` argument of qlm_code()
#'
#' A single tool may be given bare; it is wrapped. An empty list means no
#' tools, as `NULL` does. Batch processing is refused with tools because
#' [ellmer::batch_chat_structured()] never sends a chat's registered tools
#' to the provider: they would be recorded on the object as used and have
#' had no effect.
#'
#' @param tools What the caller passed.
#' @param batch The `batch` argument.
#' @param call The call to report an error against.
#'
#' @return A list of tool objects, or `NULL`.
#' @keywords internal
#' @noRd
check_tools <- function(tools, batch = FALSE, call = rlang::caller_env()) {
  if (is.null(tools)) {
    return(NULL)
  }
  if (is_ellmer_tool(tools)) {
    tools <- list(tools)
  }
  if (!is.list(tools) || !all(vapply(tools, is_ellmer_tool, logical(1)))) {
    cli::cli_abort(c(
      "{.arg tools} must be a list of {.pkg ellmer} tool objects, or a single one.",
      "i" = paste0("Create one with a provider's built-in tool function, such as ",
                   "{.fn ellmer::openai_tool_web_search}, or with {.fn ellmer::tool}.")
    ), call = call)
  }
  if (!length(tools)) {
    return(NULL)
  }
  if (isTRUE(batch)) {
    cli::cli_abort(c(
      "{.arg tools} cannot be used with {.code batch = TRUE}.",
      "x" = "{.fn ellmer::batch_chat_structured} does not send registered tools to the provider.",
      "i" = "Use {.code batch = FALSE} to code with tools."
    ), call = call)
  }
  tools
}


#' Describe tools without their code
#'
#' What a run records about its tools for anyone reading the object later:
#' the name, whether the provider runs it (`hosted`) or R does (`custom`),
#' and the description the model saw. A custom tool wraps an R function,
#' and a closure carries its environment with it into a saved file, so the
#' trail keeps these records rather than the objects, as it keeps a
#' credential callback's description rather than the callback.
#'
#' @param tools A list of tool objects.
#'
#' @return A list of records, each `list(name, type, description)`.
#' @keywords internal
#' @noRd
tool_records <- function(tools) {
  lapply(tools, function(tl) list(
    name = attr(tl, "name", exact = TRUE) %||% NA_character_,
    type = if (inherits(tl, "ellmer::ToolBuiltIn")) "hosted" else "custom",
    description = attr(tl, "description", exact = TRUE) %||% NA_character_
  ))
}


#' Tool records from either tool objects or records already made
#'
#' @param tools A list of tool objects, or of records from `tool_records()`.
#'
#' @return A list of records.
#' @keywords internal
#' @noRd
as_tool_records <- function(tools) {
  if (!length(tools)) {
    return(list())
  }
  if (all(vapply(tools, is_ellmer_tool, logical(1)))) {
    return(tool_records(tools))
  }
  tools
}


#' Are these tool records rather than tool objects?
#'
#' @param tools A list.
#'
#' @return `TRUE` when every element is a record.
#' @keywords internal
#' @noRd
is_tool_record <- function(tools) {
  length(tools) > 0L && all(vapply(tools, function(tl) {
    is.list(tl) && !is.null(tl$name) && !is.null(tl$type)
  }, logical(1)))
}


#' Whether any tool is run by the provider
#'
#' A hosted tool's calls are billed by the provider outside token pricing, so
#' a costed run with one is tokens only, and the cost note has to say so.
#'
#' @param tools A list of tool objects or records.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
has_hosted_tool <- function(tools) {
  any(vapply(as_tool_records(tools), function(r) identical(r$type, "hosted"), logical(1)))
}


#' One line naming the tools, for print() and the trail
#'
#' @param tools A list of tool objects or records.
#'
#' @return A single string, such as `"web_search (hosted), lookup (custom)"`.
#' @keywords internal
#' @noRd
format_tools <- function(tools) {
  paste(vapply(as_tool_records(tools), function(r) {
    paste0(r$name, " (", r$type, ")")
  }, character(1)), collapse = ", ")
}
