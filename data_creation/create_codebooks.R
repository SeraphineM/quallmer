#### Create codebooks for package data
library(ellmer)
devtools::load_all(".")

# Sentiment analysis codebook
data_codebook_sentiment <- qlm_codebook(
  name = "Sentiment analysis",
  instructions = paste(
    "Analyze the sentiment of this text, on both a 1-10 scale and as a polarity of negative or positive."
  ),
  schema = ellmer::type_object(
    sentiment = ellmer::type_enum(
      c("neg", "pos"),
      description = "Overall sentiment polarity: negative (neg) or positive (pos)"
    ),
    rating = ellmer::type_integer(
      description = "Sentiment rating from 1 (most negative) to 10 (most positive)"
    )
  ),
  role = "You are a political communication analyst evaluating public statements.",
  input_type = "text"
  # levels will be auto-detected: sentiment = "nominal", rating = "ordinal"
)

# Stance detection codebook
data_codebook_stance <- qlm_codebook(
  name = "Stance detection",
  instructions = "Classify the stance towards climate change expressed in this text. Choose 'Pro' if the text supports action on climate change, 'Contra' if it opposes action, or 'Neutral' if it takes no clear position. Provide a brief explanation for your classification.",
  schema = ellmer::type_object(
    stance = ellmer::type_enum(
      c("Pro", "Neutral", "Contra"),
      description = "Stance classification"
    ),
    explanation = ellmer::type_string(
      description = "Brief explanation of the classification"
    )
  ),
  role = "You are an expert in political communication and discourse analysis.",
  input_type = "text"
)

# Ideological scaling codebook
data_codebook_ideology <- qlm_codebook(
  name = "Ideological scaling",
  instructions = "Rate the ideological position of this text on a scale from 0 (far left) to 10 (far right). Consider economic and social policy positions. Provide a brief explanation for your score.",
  schema = ellmer::type_object(
    score = ellmer::type_integer(
      description = "Ideological score from 0 (left) to 10 (right)"
    ),
    explanation = ellmer::type_string(
      description = "Brief justification for the assigned score"
    )
  ),
  role = "You are an expert political scientist specializing in ideological analysis.",
  input_type = "text"
)

# Salience detection codebook
data_codebook_salience <- qlm_codebook(
  name = "Issue salience",
  instructions = "Identify the primary policy issue discussed in this text and rate its salience (prominence/importance) on a scale from 1 (minor mention) to 5 (central focus). Provide a brief explanation.",
  schema = ellmer::type_object(
    issue = ellmer::type_string(
      description = "Primary policy issue"
    ),
    salience = ellmer::type_integer(
      description = "Salience score from 1 (minor) to 5 (central)"
    ),
    explanation = ellmer::type_string(
      description = "Brief explanation"
    )
  ),
  role = "You are an expert in political communication and issue framing.",
  input_type = "text"
)

# Fact-checking codebook
data_codebook_fact <- qlm_codebook(
  name = "Fact-checking",
  instructions = "Assess whether the main factual claim in this text is true, false, or unverifiable. Provide a brief explanation with evidence if possible.",
  schema = ellmer::type_object(
    claim = ellmer::type_string(
      description = "The main factual claim"
    ),
    verdict = ellmer::type_enum(
      c("True", "False", "Unverifiable"),
      description = "Fact-check verdict"
    ),
    explanation = ellmer::type_string(
      description = "Brief explanation with evidence"
    )
  ),
  role = "You are an expert fact-checker with knowledge of current events and reliable sources.",
  input_type = "text"
)

# Save all as package data
usethis::use_data(data_codebook_sentiment, overwrite = TRUE)
usethis::use_data(data_codebook_stance, overwrite = TRUE)
usethis::use_data(data_codebook_ideology, overwrite = TRUE)
usethis::use_data(data_codebook_salience, overwrite = TRUE)
usethis::use_data(data_codebook_fact, overwrite = TRUE)
