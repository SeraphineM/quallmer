#' Apply an annotation task to input data
#'
#' Applies an annotation task to input data using a large language model.
#' Arguments in `...` are dynamically routed to either [ellmer::chat()] or to
#' [ellmer::parallel_chat_structured()] based on their names.
#'
#' @param .data Input data: a character vector of texts (for text tasks) or
#'   file paths to images (for image tasks). Named vectors will use names
#'   as identifiers in the output; unnamed vectors will use sequential integers.
#' @param task A task object created with [task()] or one of the predefined
#'   task functions ([task_sentiment()], [task_stance()], [task_ideology()],
#'   [task_salience()], [task_fact()]).
#' @param model_name Provider (and optionally model) name in the form
#'   `"provider/model"` or `"provider"` (which will use the default model for
#'   that provider). Passed to the `name` argument of [ellmer::chat()].
#'   Examples: `"openai/gpt-4o-mini"`, `"anthropic/claude-3-5-sonnet-20241022"`,
#'   `"ollama/llama3.2"`, `"openai"` (uses default OpenAI model).
#' @param ... Additional arguments passed to either [ellmer::chat()] or to
#'   [ellmer::parallel_chat_structured()], based on argument name.
#'   Arguments not recognized by either function will generate a warning.
#'
#' @return A data frame with one row per input element, containing:
#'   \describe{
#'     \item{`id`}{Identifier for each input (from names or sequential integers).}
#'     \item{...}{Additional columns as defined by the task's `type_def`.
#'       For example, [task_sentiment()] returns `score` and `explanation`;
#'       [task_stance()] returns `stance` and `explanation`.}
#'   }
#'
#' @seealso
#' [task()] for creating custom tasks, [task_sentiment()], [task_stance()],
#' [task_ideology()], [task_salience()], [task_fact()] for predefined tasks,
#' [validate()] for computing agreement metrics on annotations.
#'
#' @examples
#' \dontrun{
#' # Basic sentiment analysis
#' texts <- c("I love this product!", "This is terrible.")
#' annotate(texts, task_sentiment(), model_name = "openai")
#'
#' # With named inputs (names become IDs in output)
#' texts <- c(doc1 = "Great service!", doc2 = "Very disappointing.")
#' annotate(texts, task_sentiment(), model_name = "openai")
#'
#' # Specify provider and model
#' annotate(texts, task_sentiment(), model_name = "openai/gpt-4o-mini")
#'
#' # With execution control
#' annotate(texts, task_sentiment(),
#'          model_name = "openai/gpt-4o-mini",
#'          max_active = 5)
#'
#' # Include token usage
#' annotate(texts, task_sentiment(), model_name = "openai", include_tokens = TRUE)
#'
#' # Using Ollama locally
#' annotate(texts, task_sentiment(), model_name = "ollama/llama3.2")
#'
#' # Using Anthropic
#' annotate(texts, task_sentiment(),
#'          model_name = "anthropic/claude-3-5-sonnet-20241022")
#' }
#'
#' @export
annotate <- function(.data, task, model_name, ...) {
  if (!inherits(task, "task")) {
    stop("`task` must be created using task().")
  }

  # Input validation
  if (task$input_type == "text" && !is.character(.data)) {
    stop("This task expects text input (a character vector).")
  }
  if (task$input_type == "image" && !is.character(.data)) {
    stop("This task expects image file paths (a character vector).")
  }

  # Get valid argument names from ellmer functions
  chat_arg_names <- names(formals(ellmer::chat))
  pcs_arg_names <- names(formals(ellmer::parallel_chat_structured))

  # Route ... arguments
  dots <- list(...)
  dot_names <- names(dots)

  chat_args <- dots[dot_names %in% chat_arg_names]
  pcs_args <- dots[dot_names %in% pcs_arg_names]

  # Warn about unrecognized arguments
  all_valid_names <- unique(c(chat_arg_names, pcs_arg_names))
  unknown_names <- setdiff(dot_names, all_valid_names)
  if (length(unknown_names) > 0) {
    warning(
      "The following argument(s) were not recognized and have been ignored: ",
      paste(unknown_names, collapse = ", ")
    )
  }

  # Build chat object using ellmer::chat()
  chat <- do.call(ellmer::chat, c(
    list(
      name = model_name,
      system_prompt = task$system_prompt
    ),
    chat_args
  ))

  # Prepare prompts based on input type
  if (task$input_type == "image") {
    prompts <- lapply(.data, ellmer::content_image_file)
  } else {
    prompts <- as.list(.data)
  }

  # Execute with parallel_chat_structured
  results <- do.call(ellmer::parallel_chat_structured, c(
    list(
      chat = chat,
      prompts = prompts,
      type = task$type_def
    ),
    pcs_args
  ))

  # Add ID and reorder columns
  results$id <- names(.data) %||% seq_along(.data)
  results <- results[, c("id", setdiff(names(results), "id"))]

  results
}
