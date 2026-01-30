# Sentiment analysis codebook for movie reviews

A `qlm_codebook` object defining instructions for sentiment analysis of
movie reviews. Designed to work with
[data_corpus_LMRDsample](https://quallmer.github.io/quallmer/reference/data_corpus_LMRDsample.md)
but with an expanded polarity scale that includes a "mixed" category.

## Usage

``` r
data_codebook_sentiment
```

## Format

A `qlm_codebook` object containing:

- name:

  Task name: "Movie Review Sentiment"

- instructions:

  Coding instructions for analyzing movie review sentiment

- schema:

  Response schema with two fields: `polarity` (Enum of "neg", "mixed",
  or "pos") and `rating` (Integer from 1 to 10)

- role:

  Expert film critic persona

- input_type:

  "text"

## See also

[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md),
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md),
[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md),
[data_corpus_LMRDsample](https://quallmer.github.io/quallmer/reference/data_corpus_LMRDsample.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# View the codebook
data_codebook_sentiment

# Use with movie review corpus
coded <- qlm_code(data_corpus_LMRDsample[1:10],
                  data_codebook_sentiment,
                  model = "openai")

# Create multiple coded versions for comparison
coded1 <- qlm_code(data_corpus_LMRDsample[1:20],
                   data_codebook_sentiment,
                   model = "openai/gpt-4o-mini")
coded2 <- qlm_code(data_corpus_LMRDsample[1:20],
                   data_codebook_sentiment,
                   model = "openai/gpt-4o")

# Compare inter-rater reliability
comparison <- qlm_compare(coded1, coded2, by = "rating", level = "interval")
print(comparison)
} # }
```
