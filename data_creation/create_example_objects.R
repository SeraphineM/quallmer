# Script to create example objects for package examples
# These objects are saved to inst/extdata/example_objects.rds
# and used in documentation examples for downstream functions

library(quallmer)

# Check for API key
if (Sys.getenv("OPENAI_API_KEY") == "") {
  stop("OPENAI_API_KEY environment variable not set. ",
       "Please set it before running this script.")
}

# Set seed for reproducibility
set.seed(42)

# Use a small sample of texts for quick generation
texts <- c(
  "I absolutely loved this movie! Best film I've seen all year.",
  "Terrible experience. Would not recommend to anyone.",
  "It was okay, nothing special but not bad either.",
  "Amazing performance by the lead actor. Truly inspiring.",
  "Disappointing. The plot made no sense and acting was poor."
)

cat("Generating example coded object with sentiment analysis...\n")

# Generate first coded object using Anthropic (fast and reliable)
example_coded_sentiment <- qlm_code(
  texts,
  data_codebook_sentiment,
  model = "openai/gpt-4.1",
  name = "example_sentiment"
)

cat("Generated example_coded_sentiment with", nrow(example_coded_sentiment), "units\n")

# Generate a replication with different model
cat("Generating replicated coded object...\n")

example_coded_mini <- qlm_replicate(
  example_coded_sentiment,
  model = "openai/gpt-4.1-mini",
  name = "example_mini"
)

cat("Generated example_coded_mini\n")

# Create a comparison object
cat("Generating comparison object...\n")

example_comparison <- qlm_compare(
  example_coded_sentiment,
  example_coded_mini,
  by = "sentiment",
  level = "nominal"
)

cat("Generated example_comparison\n")

# Create a gold standard from the first coded object
# (in practice this would be expert-coded, but we'll use the first run as proxy)
gold_data <- as.data.frame(example_coded_sentiment)
example_gold_standard <- as_qlm_coded(
  gold_data,
  name = "gold_standard",
  is_gold = TRUE,
  codebook = list(
    name = "Sentiment Analysis",
    instructions = "Classify sentiment as positive, negative, or neutral"
  )
)

cat("Created example_gold_standard\n")

# Create a validation object
cat("Generating validation object...\n")

example_validation <- qlm_validate(
  example_coded_mini,
  gold = example_gold_standard,
  by = "sentiment",
  level = "nominal"
)

cat("Generated example_validation\n")

# Create human-coded example
human_data <- data.frame(
  .id = 1:3,
  category = c("A", "B", "A")
)
example_humancoded <- as_qlm_coded(
  human_data,
  name = "example_coder"
)

cat("Created example_humancoded\n")

# Save all objects to a single RDS file
example_objects <- list(
  example_coded_sentiment = example_coded_sentiment,
  example_coded_mini = example_coded_mini,
  example_comparison = example_comparison,
  example_validation = example_validation,
  example_gold_standard = example_gold_standard,
  example_humancoded = example_humancoded
)

output_file <- "inst/extdata/example_objects.rds"
saveRDS(example_objects, output_file)

cat("\nSuccessfully saved example objects to:", output_file, "\n")
cat("\nObjects included:\n")
cat("  - example_coded_sentiment: qlm_coded object from sentiment analysis\n")
cat("  - example_coded_mini: replicated qlm_coded object\n")
cat("  - example_comparison: qlm_comparison object\n")
cat("  - example_validation: qlm_validation object\n")
cat("  - example_gold_standard: gold standard qlm_coded object\n")
cat("  - example_humancoded: human-coded qlm_coded object\n")
cat("\nThese objects can be loaded in examples with:\n")
cat("  examples <- readRDS(system.file('extdata', 'example_objects.rds', package = 'quallmer'))\n")
