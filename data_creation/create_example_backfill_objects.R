# Script to add a coded run that came back incomplete, and its backfilled
# counterpart, to inst/extdata/example_objects.rds (#173).
#
# The workflow guide, the audit trail tutorial and the examples of
# qlm_failures() and qlm_backfill() show what an incomplete run looks like
# and what a backfill does to it, and none of them can spend an API key. So
# the two objects are coded once here, with a live model, and shipped.
#
# The failures are manufactured, but each is a real one, recorded as the
# package records it. A short request timeout stands in for the transient
# failures a run meets in practice (a rate limit, a timeout, a server error
# that outlasts ellmer's retries): ellmer records the timeout as the unit's
# .error, and a backfill pass made without the timeout recovers the unit. A
# low max_tokens produces the failure a backfill leaves alone: a response cut
# off at the limit, which re-sending the same request cannot fix. Which units
# time out, and which quoted sentence exceeds the limit, vary from run to run,
# so the run is repeated until it holds both kinds of failure and the backfill
# recovers every unit it re-codes.
#
# create_example_objects.R produces the other objects in the file and keeps
# these two; this script keeps those. Run from the package root.
#
# Model: anthropic/claude-haiku-4-5, through ellmer 0.4.2.
# Last generated: 2026-09-04, accepting the fourth run: one request timed
# out, three responses were cut off, and the pass recovered the one.

library(quallmer)
library(quanteda)

if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  stop("ANTHROPIC_API_KEY environment variable not set. ",
       "Please set it before running this script.")
}

output_file <- "inst/extdata/example_objects.rds"
example_objects <- readRDS(output_file)

# Eight short reviews, four of each polarity, from the package's own sample
# of the Large Movie Review Dataset. Short, and of similar length, so that
# every request takes about as long as every other and the timeout does not
# single out the long ones.
review_ids <- c(
  "3275_2.txt", "3150_1.txt", "3918_1.txt", "7530_1.txt",
  "4247_8.txt", "7227_7.txt", "8877_8.txt", "11413_10.txt"
)
texts <- as.character(data_corpus_LMRDsample)[review_ids]

# Quoted sentences make the length of the answer depend on the review, so
# that a low max_tokens cuts some answers off and not others
codebook <- qlm_codebook(
  name = "Sentiment with evidence",
  instructions = paste(
    "Classify the overall sentiment of the movie review, rate it, and quote",
    "the sentences that show the sentiment."
  ),
  schema = ellmer::type_object(
    sentiment = ellmer::type_enum(c("neg", "pos"), "Overall sentiment polarity"),
    rating = ellmer::type_integer(
      "Sentiment rating from 1 (most negative) to 10 (most positive)"
    ),
    evidence = ellmer::type_string(
      "Every sentence of the review that expresses an evaluation of the film, quoted verbatim"
    )
  )
)

model <- "anthropic/claude-haiku-4-5"
max_tokens <- 90L  # low enough that the longer quotations are cut off
timeout_s <- 1.5   # just inside the model's usual latency; it should catch a
                   # few requests, not all, so tune it to the day's latency
max_runs <- 40L

is_cut_off <- function(errors) {
  vapply(errors, inherits, logical(1), "quallmer_truncation_error")
}

accepted <- FALSE
for (run in seq_len(max_runs)) {
  # ellmer would retry a timed-out request; one try, so that the timeout is
  # recorded as the failure, as it is after an outage that outlasts the tries
  options(ellmer_timeout_s = timeout_s, ellmer_max_tries = 1L)
  incomplete <- suppressWarnings(suppressMessages(qlm_code(
    texts, codebook,
    model = model,
    params = ellmer::params(max_tokens = max_tokens),
    name = "example_incomplete",
    notes = paste(
      "Coded with a deliberately short request timeout and max_tokens = 90,",
      "so that the run came back incomplete"
    )
  )))
  options(ellmer_timeout_s = NULL, ellmer_max_tries = NULL)

  failures <- qlm_failures(incomplete)
  cut_off <- is_cut_off(failures$.error)
  cat(sprintf(
    "Run %d: %d failed of %d (%d cut off, %d timed out)\n",
    run, nrow(failures), nrow(incomplete), sum(cut_off), sum(!cut_off)
  ))
  # Both kinds present, and enough of the run coded for the object to read
  # as a run that mostly worked
  if (!any(cut_off) || all(cut_off) || nrow(failures) > 4L) next

  # The pass runs with the run's own model and settings and today's default
  # timeout. Its messages are kept, for the guide to quote.
  pass_messages <- character()
  backfilled <- withCallingHandlers(
    qlm_backfill(incomplete),
    message = function(m) {
      pass_messages <<- c(pass_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  pass <- qlm_meta(backfilled, "backfill", type = "object")[[1]]
  cat(sprintf(
    "  backfill: attempted %d, recovered %d\n",
    length(pass$attempted), length(pass$recovered)
  ))
  if (setequal(pass$recovered, pass$attempted)) {
    accepted <- TRUE
    break
  }
}
if (!accepted) {
  stop("No run met the conditions in ", max_runs, " attempts; ",
       "adjust timeout_s to the day's latency and try again.")
}

cat("\nAccepted run", run, "\n\n")
print(incomplete)
print(qlm_failures(incomplete))
cat("\nBackfill messages:\n")
cat(pass_messages, sep = "")
cat("\n")
print(backfilled)
print(qlm_failures(backfilled))

example_objects$example_coded_incomplete <- incomplete
example_objects$example_coded_backfilled <- backfilled
saveRDS(example_objects, output_file)

cat("\nSaved", output_file, "with objects:", paste(names(example_objects), collapse = ", "), "\n")
