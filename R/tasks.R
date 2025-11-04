#' Predefined task for sentiment analysis
#' @return A task object
#' @export
sentiment <- function() {
  define_task(
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
stance <- function(topic = "the given topic") {
  define_task(
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


