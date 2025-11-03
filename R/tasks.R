#' The `sentiment()` task analyzes short texts and returns a sentiment score (-1 to 1) and a short explanation.
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



#' The `themes()` task assesses texts for the proportional presence of predefined topics.
#' @param topics A character vector of topics to code for.
#' The default topics are "Environment", "Economy", "Health", and "Education".
#' @return A task object.
#' @export
themes <- function(topics = c("Environment", "Economy", "Health", "Education")) {
  # Create a list of typed objects for each topic
  topic_types <- lapply(topics, function(topic) {
    ellmer::type_number(paste0("Proportion (0-1) of text related to ", topic))
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
      "You are an expert annotator. Read each short text carefully and assign proportions of content",
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


#' The `stance()` task assesses the stance of a text towards a specific topic or issue.
#' @param topic A character string specifying the topic for stance detection.
#' @return A task object
#' @export
stance <- function(topic = "the given topic") {
  define_task(
    name = "Stance detection",
    system_prompt = paste0(
      "You are an expert annotator. Read each short text carefully and determine its stance towards ",
      topic,
      " Classify the stance as Pro, Neutral, or Contra, and provide a brief explanation for your classification."
    ),
    type_object = ellmer::type_object(
      stance = ellmer::type_string("Stance towards the topic: Pro, Neutral, or Contra"),
      explanation = ellmer::type_string("Brief explanation of the classification")
    ),
    input_type = "text"
  )
}
