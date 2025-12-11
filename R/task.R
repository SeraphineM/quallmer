#' Define an annotation task
#'
#' @description
#' A flexible task definition wrapper for ellmer.
#' Supports any structured output type, including `type_object()`, `type_array()`,
#' `type_enum()`, `type_boolean()`, and others.
#'
#' @param name Name of the task.
#' @param system_prompt System prompt to guide the model (as required by ellmer's `chat_fn`).
#' @param type_def Structured output definition, e.g., created by `ellmer::type_object()`,
#'   `ellmer::type_array()`, or `ellmer::type_enum()`.
#' @param input_type Type of input data: `"text"` or `"image"`.
#' @return A task object with a `run()` method.
#' @export
task <- function(name, system_prompt, type_def, input_type = c("text", "image")) {
  input_type <- match.arg(input_type)

  run <- function(.data, chat_fn = NULL, model = NULL, verbose = TRUE, ...) {
    # Basic input validation
    if (input_type == "text" && !is.character(.data)) {
      stop("This task expects text input (a character vector).")
    }

    # Default fallbacks
    chat_fn <- chat_fn %||% ellmer::chat_openai
    model   <- model %||% "gpt-4o"

    if (verbose) message("Running task '", name, "' using model: ", model)

    chat <- chat_fn(
      model = model,
      system_prompt = system_prompt,
      ...
    )

    # Prepare prompts based on input type
    if (input_type == "image") {
      prompts <- lapply(.data, ellmer::content_image_file)
    } else {
      prompts <- as.list(.data)
    }

    # Core execution
    results <- ellmer::parallel_chat_structured(
      chat,
      prompts = prompts,
      type = type_def,
      convert = TRUE,
      include_tokens = FALSE,
      include_cost = FALSE,
      max_active = 10
    )

    # Add ID and reorder columns
    results$id <- names(.data) %||% seq_along(.data)
    results <- results[, c("id", setdiff(names(results), "id"))]

    return(results)
  }

  # Return as a structured task object
  structure(
    list(
      name = name,
      system_prompt = system_prompt,
      type_def = type_def,
      input_type = input_type,
      run = run
    ),
    class = "task"
  )
}
