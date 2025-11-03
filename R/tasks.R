#' Predefined tasks
#'
#' Sentiment analysis task
#' Analyzes short texts and returns a sentiment score (-1 to 1) and a short explanation.
#' @return A task object
#' @export
sentiment <- function() {
  define_task(
    name = "Sentiment analysis",
    system_prompt = "You are an expert annotator. Rate the sentiment of each text from -1 (very negative) to 1 (very positive) and briefly explain why.",
    type_object = ellmer::type_object(
      score = ellmer::type_number("Sentiment score between -1 (very negative) and 1 (very positive)"),
      explanation = ellmer::type_string("Brief explanation of the rating")
    ),
    input_type = "text"
  )
}

# Internal registry of built-in tasks
.predefined_tasks <- list(
  sentiment = sentiment
)

#' Theme coding task
#'
#' Codes short texts for the proportional presence of predefined topics and provides brief explanations.
#'
#' @param topics A character vector of topics to code for.
#' The default topics are "Environment", "Economy", "Health", and "Education".
#' @return A task object.
#' @export
themes <- function(topics = c("Environment", "Economy", "Health", "Education")) {
  # Create a list of typed objects for each topic
  topic_types <- lapply(topics, function(topic) {
    ellmer::type_number(paste0("Proportion (0–1) of text related to ", topic))
  })
  names(topic_types) <- topics

  # Construct the type_object dynamically
  type_obj <- do.call(
    ellmer::type_object,
    c(topic_types, list(explanation = ellmer::type_string("Brief explanation of the coding")))
  )

  # Define the task
  define_task(
    name = "Theme coding",
    system_prompt = paste0(
      "You are an expert annotator. Read each short text carefully and assign proportions of content ",
      "related to the following topics: ",
      paste(topics, collapse = ", "),
      ". Each proportion must be between 0 and 1, and all proportions must add up to exactly 1. ",
      "If a topic is not mentioned, assign it a proportion of 0. ",
      "Provide a brief explanation summarizing your reasoning."
    ),
    type_object = type_obj,
    input_type = "text"
  )
}
