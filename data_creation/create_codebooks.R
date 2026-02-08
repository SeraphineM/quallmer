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

## immigration ----

## Immigration policy codebook
## Based on: Benoit, K., Conway, D., Lauderdale, B. E., Laver, M. &
## Mikhaylov, S. (2016). Crowd-sourced text analysis: reproducible and agile
## production of political data. American Political Science Review, 110(2),
## 278-295. https://doi.org/10.1017/S0003055416000058

library(quallmer)
library(ellmer)

instructions_immigration <- paste(
  "



  If the sentence IS about immigration policy, rate the position on a
  five-point scale:
    1 = Very pro-immigration (very open and favourable position)
    2 = Somewhat pro-immigration
    3 = Neutral (neither favouring nor opposing immigration)
    4 = Somewhat anti-immigration
    5 = Very anti-immigration (very closed and negative stance)

  "
)

data_codebook_immigration <- qlm_codebook(
  name = "Immigration policy coding from Benoit et al. (2016)",
  instructions = "You are coding sentences from political texts from the 2010 UK general
  election. Your task is to judge whether each sentence deals with
  immigration policy, and if so, to rate the policy position on immigration
  expressed in the sentence.

  Most sentences will NOT relate to immigration policy.",
  schema = ellmer::type_object(
    immigration_label = ellmer::type_enum(
      c("Not immigration", "Immigration"),
      description = "Whether the sentence refers to immigration policy:
      'Immigration' if the sentence relates to any aspect of immigration policy
      (work permits, residency, asylum, citizenship, illegal immigration,
      or general statements about immigrants or immigration).
      'Not immigration' if the sentence does not relate to immigration policy.

      Immigration policy relates to all government policies, laws, regulations,
  and practices that deal with the free travel of foreign persons across the
  country's borders, especially those that intend to live, work, or seek legal
  protection (asylum) in that country. Examples include the regulation of:
  work permits for foreign nationals; residency permits for foreign nationals;
  asylum seekers and their treatment; requirements for acquiring citizenship;
  illegal immigrants and migrant workers (and their families) living or
  working illegally in the country. It also includes favourable or
  unfavourable general statements about immigrants or immigration policy."
    ),
    immigration_position = ellmer::type_integer(
      "Policy position on immigration, coded as:
      -1 = Favorable and open immigration policy.
      0 = Neutral,
      1 = Negative and closed immigration policy.

      Examples of PRO-immigration positions: positive statements about benefits
  of immigration (economic or cultural); moral obligation to welcome asylum
  seekers; policies improving conditions for asylum seekers; urging more
  work permits; making it possible for illegal immigrants to obtain legal
  status; reducing barriers to immigration.

  Examples of ANTI-immigration positions: negative statements about
  consequences of immigration (job losses, crime, cultural destruction);
  arguments about asylum seekers abusing the system; policies to deport
  asylum seekers; restricting work permits including points systems;
  deporting illegal immigrants; increasing barriers to immigration.

  Examples of NEUTRAL positions: advocating a balanced approach; statements
  about administrative capacity for handling immigration; statements that
  do not take a clear pro- or anti-immigration stance."
    )
  ),
  input_type = "text",
  levels = list(
    immigration_label = "nominal",
    immigration_position = "ordinal"
  )
)



codebook_immigration


# Save all as package data
usethis::use_data(data_codebook_sentiment, overwrite = TRUE)
usethis::use_data(data_codebook_stance, overwrite = TRUE)
usethis::use_data(data_codebook_ideology, overwrite = TRUE)
usethis::use_data(data_codebook_salience, overwrite = TRUE)
usethis::use_data(data_codebook_fact, overwrite = TRUE)
usethis::use_data(data_codebook_immigration, overwrite = TRUE)
