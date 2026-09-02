# Live check for the DeepSeek JSON-mode coding path (#128).
#
# NOT part of the test suite: every section below makes real, billed API calls.
# Run it interactively, section by section, after
#
#   pkgload::load_all(".")
#
# Requires DEEPSEEK_API_KEY in ~/.Renviron. Section 0 costs nothing and is worth
# running first. Sections 1-8 are independent, so run whichever you care about.
#
# What each section is actually probing:
#   1  does the happy path work at all, and does it look like other providers
#   2  do names survive, does cost accounting appear when asked for
#   3  the hard case: nested arrays, where JSON-path errors earn their keep
#   4  can the validator + repair loop actually recover a bad response
#   5  what a genuine failure looks like when repair is switched off
#   6  do the downstream objects still work off a JSON-path result
#   7  the point of the whole exercise: same codebook, two providers, one answer
#   8  adversarial prompts, to try to provoke non-conformance on purpose

library(quallmer)

MODEL <- "deepseek/deepseek-v4-flash"  # or "deepseek-v4-pro"
#
# Valid names at time of writing: deepseek-v4-pro, deepseek-v4-flash,
# deepseek-v4-flash-vision-exp. A wrong one now aborts with that list rather
# than retrying nine times and warning four times; worth seeing once:
#   qlm_code(texts, data_codebook_sentiment, model = "deepseek/deepseek-v3-flash")

texts <- c(
  "I love this product, it exceeded every expectation.",
  "Terrible experience. Broke within a week and support ignored me.",
  "It's okay. Does the job, nothing special."
)


# 0. No API calls -------------------------------------------------------------
# Confirms routing and the guard rails without spending anything.

stopifnot(
  # DeepSeek rejects the schema-constrained request, so it starts in JSON mode
  identical(quallmer:::default_structured_mode(MODEL), "json"),
  identical(quallmer:::default_structured_mode("openai/gpt-4o-mini"), "auto")
)

# The exact system prompt DeepSeek will receive. Worth reading once: if the
# model misbehaves, this is the first place to look.
cat(quallmer:::json_system_prompt(data_codebook_sentiment))

# Guards, all of which should error before any request is made
try(qlm_code(texts, data_codebook_sentiment, model = MODEL, batch = TRUE))
try(qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini", max_retries = 2))
try(qlm_code(texts, data_codebook_sentiment, model = MODEL, max_retries = -1))


# 1. Basic run ----------------------------------------------------------------

coded <- qlm_code(texts, data_codebook_sentiment, model = MODEL)
coded

# Expect: sentiment is a factor with levels neg/pos, rating an integer 1-10,
# no .error column at all, and backend recorded.
str(coded$sentiment)
str(coded$rating)
"%in%"(".error", names(coded))          # should be FALSE
qlm_meta(coded, type = "object")$backend
qlm_meta(coded, type = "user")$n_invalid


# 2. Named inputs, tokens and cost --------------------------------------------

named <- c(
  rev1 = "Great service, would recommend.",
  rev2 = "Very disappointing, would not buy again.",
  rev3 = "Fine, I suppose."
)
coded_named <- qlm_code(named, data_codebook_sentiment, model = MODEL,
                        include_tokens = TRUE, include_cost = TRUE)
coded_named

coded_named$.id                          # should be rev1, rev2, rev3
sum(coded_named$cost)                    # total spend for this call
# Same request through a schema-enforcing provider produces these same four
# columns in this same order; that parity is the thing being checked.
names(coded_named)


# 3. Nested arrays ------------------------------------------------------------
# The case the flat codebooks do not exercise. A validation failure here should
# report a path like $.claims[2].salience rather than a bare mismatch.

codebook_claims <- qlm_codebook(
  name = "Claims",
  role = "You are a careful political analyst.",
  instructions = paste(
    "Identify each distinct policy claim made in the text.",
    "For each claim give the claim itself, a topic label, and a salience score",
    "from 0 to 1 reflecting how central the claim is to the passage."
  ),
  schema = ellmer::type_object(
    overall_tone = ellmer::type_enum(
      "Overall tone of the passage",
      values = c("positive", "neutral", "negative")
    ),
    claims = ellmer::type_array(
      ellmer::type_object(
        claim = ellmer::type_string("The policy claim, in your own words"),
        topic = ellmer::type_enum(
          "Policy area",
          values = c("economy", "immigration", "health", "environment", "other")
        ),
        salience = ellmer::type_number("Centrality of the claim, 0 to 1")
      ),
      description = "Policy claims made in the passage"
    )
  )
)

passages <- c(
  "We will cut taxes for working families and invest the savings from waste in the health service.",
  "Our borders must be secure, but we will always honour our obligations to genuine refugees.",
  "The climate emergency demands that we end fossil fuel subsidies this parliament."
)

coded_claims <- qlm_code(passages, codebook_claims, model = MODEL)
coded_claims
# claims should be a list-column of tibbles, one per passage
coded_claims$claims[[1]]
str(coded_claims$claims[[1]])


# 4. Does the repair loop actually recover? -----------------------------------
# A deliberately awkward enum: models like to answer with their own wording
# rather than the permitted values, which is exactly what repair is for.
# Run this a few times; occasional retries are the expected behaviour, and a
# clean result means the loop did its job silently.

codebook_awkward <- qlm_codebook(
  name = "Awkward enum",
  instructions = paste(
    "Classify the emotional register of the text.",
    "Answer with the code only."
  ),
  schema = ellmer::type_object(
    register = ellmer::type_enum(
      "Emotional register",
      values = c("ER_01_ELATED", "ER_02_CONTENT", "ER_03_FLAT",
                 "ER_04_IRRITATED", "ER_05_FURIOUS")
    ),
    confidence = ellmer::type_number("Confidence from 0 to 1")
  )
)

coded_awkward <- qlm_code(texts, codebook_awkward, model = MODEL)
coded_awkward
table(coded_awkward$register, useNA = "ifany")


# 5. What failure looks like --------------------------------------------------
# max_retries = 0 disables repair, so any non-conformance surfaces directly.
# Compare the .error messages here against a default run of section 4.

coded_noretry <- qlm_code(texts, codebook_awkward, model = MODEL, max_retries = 0)
coded_noretry

if (".error" %in% names(coded_noretry)) {
  vapply(
    coded_noretry$.error,
    function(e) if (is.null(e)) NA_character_ else conditionMessage(e),
    character(1)
  )
}
qlm_meta(coded_noretry, type = "user")$n_invalid


# 6. Downstream objects -------------------------------------------------------
# A JSON-path result must behave like any other qlm_coded object.

qlm_trail(coded)
rep1 <- qlm_replicate(coded)             # NB: reverts to max_retries = 2
qlm_compare(coded, rep1)


# 7. Cross-provider agreement -------------------------------------------------
# The real test of the fix. Needs OPENAI_API_KEY as well. If the two providers
# disagree far more than two runs of the same provider do, that is a signal
# worth chasing.

coded_openai <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini")

identical(names(coded), names(coded_openai))
identical(lapply(coded, class), lapply(coded_openai, class))
qlm_compare(coded, coded_openai)

# Within-provider baseline, for comparison
qlm_compare(coded, qlm_replicate(coded))


# 8. Trying to break it -------------------------------------------------------
# Prompts written to provoke the failure modes seen in the PopuLLM runs:
# schema keywords echoed into the answer, properties renamed, extra keys added.
# The validator should catch all of these; watch whether repair recovers them.

codebook_adversarial <- qlm_codebook(
  name = "Adversarial",
  instructions = paste(
    "Analyse the text. Explain your reasoning in detail, describe the",
    "structure of your answer, and include any additional properties you",
    "think would be informative."
  ),
  schema = ellmer::type_object(
    is_populist = ellmer::type_boolean("Whether the text is populist"),
    justification = ellmer::type_string("Why you reached that conclusion")
  )
)

long_texts <- c(
  "The corrupt elite have betrayed ordinary people for too long. We will take back control.",
  "This government's fiscal framework balances medium-term consolidation against growth."
)

wrong_texts <- c(
  "The Chinese Communist Party controls peoples' minds.",
  "This government's fiscal framework balances medium-term consolidation against growth."
)

coded_adv <- qlm_code(long_texts, codebook_adversarial, model = MODEL)
coded_adv
qlm_meta(coded_adv, type = "user")$n_invalid

# Raise retries if failures persist, to see whether they are recoverable at all
coded_adv5 <- qlm_code(long_texts, codebook_adversarial, model = MODEL, max_retries = 5)
qlm_meta(coded_adv5, type = "user")$n_invalid
