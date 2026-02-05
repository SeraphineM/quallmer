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
# View the codebook
data_codebook_sentiment
#> quallmer codebook: Sentiment analysis 
#>   Input type:   text
#>   Role:         You are a political communication analyst evaluating public ...
#>   Instructions: Analyze the sentiment of this text, on both a 1-10 scale and...
#>   Output schema:ellmer::TypeObject
#>   Levels:
#>     sentiment: nominal
#>     rating: ordinal

# \donttest{
# Use with movie review corpus (requires API key)
coded <- qlm_code(data_corpus_LMRDsample[1:10],
                  data_codebook_sentiment,
                  model = "openai")
#> Using model = "gpt-4.1".

# Create multiple coded versions for comparison
coded1 <- qlm_code(data_corpus_LMRDsample[1:20],
                   data_codebook_sentiment,
                   model = "openai/gpt-4o-mini")
#> [working] (1 + 0) -> 10 -> 9 | ■■■■■■■■■■■■■■■                   45%
#> [working] (0 + 0) -> 0 -> 20 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%
coded2 <- qlm_code(data_corpus_LMRDsample[1:20],
                   data_codebook_sentiment,
                   model = "openai/gpt-4o")

# Compare inter-rater reliability
comparison <- qlm_compare(coded1, coded2, by = "rating", level = "interval")
print(comparison)
#> 
#> ── Inter-rater reliability ──
#> 
#> Subjects: 20
#> Raters: 2
#> 
#> 
#> ── rating (interval) 
#> Percent agreement: 0.8500
#> Krippendorff's alpha: 0.9437
#> ICC: 0.9450
#> Pearson's r: 0.9439
#> 
# }
```
