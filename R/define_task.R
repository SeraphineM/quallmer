#' Define an annotation task
#'
#' @param name Name of the task
#' @param system_prompt System prompt (text to guide the model)
#' @param type_object Structured output definition (ellmer type_object)
#' @param input_type Type of input data: "text", "image", "audio", etc.
#' @return A task object
#' @export
define_task <- function(name, system_prompt, type_object, input_type = "text") {

  run <- function(.data, chat_fn = ellmer::chat_openai, model = "gpt-4o", verbose = TRUE, ...) {
    if (input_type == "text" && !is.character(.data)) {
      stop("This task expects text input (a character vector).")
    }

    chat <- chat_fn(
      model = model,
      system_prompt = system_prompt,
      ...
    )

    results <- ellmer::parallel_chat_structured(
      chat,
      prompts = as.list(.data),
      type = type_object,
      convert = TRUE,
      include_tokens = FALSE,
      include_cost = FALSE,
      max_active = 10
    )

    results$id <- names(.data) %||% seq_along(.data)
    results <- results[, c("id", setdiff(names(results), "id"))]
    return(results)
  }

  structure(
    list(
      name = name,
      system_prompt = system_prompt,
      type_object = type_object,
      input_type = input_type,
      run = run
    ),
    class = "define_task"
  )
}
