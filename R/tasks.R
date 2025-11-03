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
