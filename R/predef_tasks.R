#' Predefined task for sentiment analysis
#' @return A task object
#' @export
task_sentiment <- function() {
  task(
    name = "Sentiment analysis",
    system_prompt = "You are an expert annotator. Rate the sentiment of each text from -1 (very negative) to 1 (very positive) and briefly explain why.",
    type_def = ellmer::type_object(
      score = ellmer::type_number("Sentiment score between -1 (very negative) and 1 (very positive)"),
      explanation = ellmer::type_string("Brief explanation of the rating")
    ),
    input_type = "text"
  )
}


#' Predefined task for stance detection (position taking)
#' @param topic A character string specifying the topic for stance detection.
#' @return A task object
#' @export
task_stance <- function(topic = "the given topic") {
  task(
    name = "Stance detection",
    system_prompt = paste0(
      "You are an expert annotator. Read each short text carefully and determine its stance towards ",
      topic,
      " Classify the stance as Pro, Neutral, or Contra, and provide a brief explanation for your classification."
    ),
    type_def = ellmer::type_object(
      stance = ellmer::type_string("Stance towards the topic: Pro, Neutral, or Contra"),
      explanation = ellmer::type_string("Brief explanation of the classification")
    ),
    input_type = "text"
  )
}

#' Predefined task for ideological scaling on a specified dimension
#'
#' Ideological scaling on a specified dimension, with justification.
#'
#' @param dimension A character string specifying the ideological dimension,
#'   ideally naming both poles, e.g., "liberal - illiberal", "left - right",
#'   or "inclusive - exclusive". The first pole corresponds to 0 and the second to 10.
#' @param definition Optional detailed explanation of what the dimension means.
#'   If provided, it will be included in the system prompt to guide annotation.
#'
#' @return A task object
#' @export
task_ideology <- function(
    dimension = "the specified ideological dimension (0 = first pole, 10 = second pole)",
    definition = NULL
) {

  definition_text <- if (!is.null(definition)) {
    paste0("\n\nDefinition of the dimension:\n", definition, "\n")
  } else {
    ""
  }

  task(
    name = "Ideological scaling",
    system_prompt = paste0(
      "You are an expert political scientist performing ideological text scaling.\n\n",
      "Task:\n",
      "- Read each short text carefully.\n",
      "- Place the text on a 0 - 10 scale for the following ideological dimension: ",
      dimension, ".\n",
      "- Interpret 0 as representing the FIRST pole mentioned in the dimension label,\n",
      "  and 10 as representing the SECOND pole mentioned.\n",
      "- Use the full 0 - 10 range where appropriate and avoid defaulting to middle values.\n",
      "- Base your decision only on the information in the text (do not infer external\n",
      "  knowledge about the author, party, or context).",
      definition_text,
      "\nOutput:\n",
      "- `score`: an integer from 0 to 10 indicating the position on the specified dimension.\n",
      "- `explanation`: a brief, text-based justification explaining why the score was chosen,\n",
      "  citing specific phrases or arguments from the text."
    ),
    type_def = ellmer::type_object(
      score       = ellmer::type_integer("Ideological position on the specified dimension (0 - 10, where 0 = first pole, 10 = second pole)"),
      explanation = ellmer::type_string("Brief justification for the assigned score, referring to specific elements in the text")
    ),
    input_type = "text"
  )
}

