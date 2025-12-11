#' Define an annotation task
#'
#' Creates a task definition for use with [annotate()]. A task specifies
#' what information to extract from input data, including the system prompt
#' that guides the LLM and the structured output definition.
#'
#' @param name Name of the task (character).
#' @param system_prompt System prompt to guide the model.
#' @param type_def Structured output definition, e.g., created by
#'   [ellmer::type_object()], [ellmer::type_array()], or [ellmer::type_enum()].
#' @param input_type Type of input data: `"text"` or `"image"`.
#'
#' @return A task object (a list with class `"task"`) containing the task
#'
#'   definition. Use with [annotate()] to apply the task to data.
#'
#' @seealso [annotate()] for applying tasks to data, [task_sentiment()],
#'   [task_stance()], [task_ideology()], [task_salience()], [task_fact()]
#'   for predefined tasks.
#'
#' @examples
#' \dontrun{
#' # Define a custom task
#' my_task <- task(
#'   name = "Sentiment",
#'   system_prompt = "Rate the sentiment from -1 (negative) to 1 (positive).",
#'   type_def = ellmer::type_object(
#'     score = ellmer::type_number("Sentiment score from -1 to 1"),
#'     explanation = ellmer::type_string("Brief explanation")
#'   )
#' )
#'
#' # Use with annotate()
#' texts <- c("I love this!", "This is terrible.")
#' annotate(texts, my_task, model_name = "openai/gpt-4o-mini")
#' }
#'
#' @export
task <- function(name, system_prompt, type_def, input_type = c("text", "image")) {
  input_type <- match.arg(input_type)

  structure(
    list(
      name = name,
      system_prompt = system_prompt,
      type_def = type_def,
      input_type = input_type
    ),
    class = "task"
  )
}
