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

#' Predefined task for salience of topics discussed (ranked topics)
#'
#' Ranked list of topics mentioned in a text, ordered by salience.
#'
#' @param topics Optional character vector of predefined topic labels
#'   (e.g., c("economy", "health", "education", "environment")).
#'   If supplied, the model should only classify and rank among these topics.
#'   If NULL, the model may infer topic labels directly from the text.
#' @param max_topics Integer: maximum number of topics to return when topics
#'   are inferred from the text. Default is 5.
#'
#' @return A task object
#' @export
task_salience <- function(topics = NULL, max_topics = 5) {

  system_prompt <- paste(
    "You are an expert analysing the content of texts.",
    "Extract structured data on the salience of topics discussed in texts.",
    "",
    "Task:",
    "- Read the text carefully.",
    "- Identify the topics discussed.",
    "- Return a ranked list of topics, ordered by their salience in the text.",
    "- Salience refers to how prominently and frequently a topic is discussed,",
    "  including the amount of space and emphasis devoted to it.",
    "",
    "Do not infer information that is not in the text.",
    "Base all evaluations solely on the language and arguments in the document.",
    sep = "\n"
  )

  topics_text <- if (!is.null(topics)) {
    paste0(
      "\n\nOnly consider the following topics when constructing the ranked list:\n-",
      paste(topics, collapse = "\n-"),
      "\nIf none of these topics is clearly present, return an empty list."
    )
  } else {
    paste0(
      "\n\nIdentify and return up to", max_topics,
      " of the most salient topics directly from the text, ranked in descending order of salience.",
      " If fewer topics are clearly present, return only those."
    )
  }

  type_def <- ellmer::type_object(
    topics = ellmer::type_array(
      ellmer::type_string(
        paste0("Topics mentioned in the text, listed in order of salience (first = most salient, up to", max_topics," topics).")
      )
    ),
    explanation = ellmer::type_string(
      "A brief explanation of why these topics were selected and ordered in this way, referring to specific wording, emphasis, or sections of the text."
    )
  )

  task(
    name = "Salience (ranked topics)",
    system_prompt = paste0(system_prompt, topics_text),
    type_def = type_def,
    input_type = "text"
  )
}


#' Predefined task for overall truthfulness assessment
#'
#' Assigns an overall truthfulness score to a text and lists topics that reduce
#' confidence in its accuracy.
#'
#' @param max_topics Integer: maximum number of topics or issues to list as
#'   reducing confidence in the truthfulness of the text. Default is 5.
#'
#' @return A task object
#' @export
task_fact <- function(max_topics = 5) {

  system_prompt <- paste(
    "You are an expert fact-checker analysing the content of texts.",
    "Your goal is to provide an overall assessment of how truthful and accurate",
    "the text is, and to highlight any topics that reduce your confidence.",
    "",
    "Task:",
    "- Read the text carefully.",
    "- Consider the factual claims and implications made in the text.",
    "- Assign an overall truthfulness score from 0 to 10, where:",
    "  0 = almost completely false or highly misleading,",
    "  5 = mixed or partially accurate with significant issues,",
    " 10 = highly accurate, with no obvious false or misleading claims.",
    "- If specific topics or issues in the text reduce your confidence in its",
    "  truthfulness, list them as \"low confidence topics\".",
    "- If no particular topics stand out as problematic, leave the list empty.",
    "",
    "Do not invent new facts or details that are not widely known.",
    "Base your evaluation on general background knowledge and on the wording",
    "of the text itself.",
    sep = "\n"
  )

  topics_text <- paste0(
    "\n\nIdentify and return up to ", max_topics,
    " topics, issues or themes that lower your confidence in the truthfulness",
    " of the text. If none are clearly problematic, return an empty list."
  )

  type_def <- ellmer::type_object(
    truth_score = ellmer::type_integer(
      "Overall truthfulness and accuracy score from 0 (almost completely false or highly misleading) to 10 (highly accurate and truthful)."
    ),
    misleading = ellmer::type_array(
      ellmer::type_string(
        paste0(
          "Topics, issues, or themes that reduce confidence in the truthfulness of the text (up to ",
          max_topics, " items)."
        )
      )
    ),
    explanation = ellmer::type_string(
      "Brief explanation for the assigned truthfulness score and how the listed topics, if any, contribute to reduced confidence."
    )
  )

  task(
    name = "Overall truthfulness assessment",
    system_prompt = paste0(system_prompt, topics_text),
    type_def = type_def,
    input_type = "text"
  )
}
