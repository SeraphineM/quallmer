# Comparing and replicating coded results

In this tutorial, we will explore how to assess the reliability and
validity of LLM-coded results using the `quallmer` package. We will
cover three key functions:

- [`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md) -
  for assessing inter-rater reliability between multiple coded results
- [`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md) -
  for validating coded results against a gold standard
- [`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md) -
  for re-executing coding with different settings to test reliability

These tools help ensure that your qualitative coding is robust,
reproducible, and accurate.

## Loading packages and data

``` r
# We will use the quanteda package
# for loading a sample corpus of inaugural speeches
# If you have not yet installed the quanteda package, you can do so by:
# install.packages("quanteda")
library(quanteda)
```

    ## Package version: 4.3.1
    ## Unicode version: 15.1
    ## ICU version: 74.2

    ## Parallel computing: disabled

    ## See https://quanteda.io for tutorials and examples.

``` r
library(quallmer)
```

    ## Loading required package: ellmer

``` r
# For educational purposes,
# we will use a subset of the inaugural speeches corpus
# The ten most recent speeches in the corpus
data_corpus_inaugural <- quanteda::data_corpus_inaugural[50:60]
```

## Using a codebook for this tutorial

For this tutorial, we’ll use the built-in `data_codebook_fact` as a
quick example. This allows us to focus on the comparison and validation
functions rather than codebook design.

``` r
# View the built-in sentiment codebook
data_codebook_ideology
```

    ## quallmer codebook: Ideological scaling 
    ##   Input type:   text
    ##   Role:         You are an expert political scientist specializing in ideolo...
    ##   Instructions: Rate the ideological position of this text on a scale from 0...
    ##   Output schema:ellmer::TypeObject
    ##   Levels:
    ##     score: ordinal
    ##     explanation: nominal

**Note**: The built-in codebooks are provided as examples and starting
points. For actual research projects, you should create custom codebooks
specific to your research questions (see the “Creating codebooks”
tutorial for details).

## Initial coding run

Let’s code the speeches using our codebook with a specific model and
settings:

``` r
# Code the speeches with GPT-4o using the built-in codebook on ideology
coded1 <- qlm_code(data_corpus_inaugural,
                   codebook = data_codebook_ideology,
                   model = "openai/gpt-4o",
                   params = params(temperature = 0),
                   name = "gpt4o_run")
```

    ## [working] (0 + 0) -> 10 -> 1 | ■■■■                               9%

    ## [working] (0 + 0) -> 0 -> 11 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

``` r
# View the results
coded1
```

    ## # quallmer coded object
    ## # Run:      gpt4o_run
    ## # Codebook: Ideological scaling
    ## # Model:    openai/gpt-4o
    ## # Units:    11
    ## 
    ## # A tibble: 11 × 3
    ##    .id          score explanation                                               
    ##  * <chr>        <int> <chr>                                                     
    ##  1 1985-Reagan      8 The text emphasizes limited government, reduced taxes, an…
    ##  2 1989-Bush        7 The text reflects a center-right ideological position. It…
    ##  3 1993-Clinton     4 The text emphasizes themes of renewal, change, and respon…
    ##  4 1997-Clinton     4 The text emphasizes themes of equality, community, and op…
    ##  5 2001-Bush        6 The text reflects a centrist to moderately right-leaning …
    ##  6 2005-Bush        7 The text emphasizes a strong commitment to spreading demo…
    ##  7 2009-Obama       3 The text emphasizes unity, social responsibility, and gov…
    ##  8 2013-Obama       3 The text emphasizes equality, collective action, and soci…
    ##  9 2017-Trump       8 The text emphasizes nationalism, protectionism, and a foc…
    ## 10 2021-Biden       3 The text emphasizes unity, democracy, and addressing soci…
    ## 11 2025-Trump       8 The text emphasizes nationalism, strong border control, m…

## Replicating with different settings

The
[`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md)
function allows you to re-execute coding with different models,
parameters, or codebooks while maintaining a provenance chain. This is
useful for testing the sensitivity of your results to different
settings.

### Replicating with a different model

``` r
# Replicate the coding with openai/gpt-4o-mini
coded2 <- qlm_replicate(coded1,
                        model = "openai/gpt-4o-mini",
                        name = "mini_run")
```

    ## [working] (0 + 0) -> 6 -> 5 | ■■■■■■■■■■■■■■■                   45%

    ## [working] (0 + 0) -> 0 -> 11 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

### Replicating with different temperature

``` r
# Replicate with higher temperature for more variability
coded3 <- qlm_replicate(coded1,
                        params = params(temperature = 0.7),
                        name = "gpt4o_temp07")
```

    ## [working] (0 + 0) -> 10 -> 1 | ■■■■                               9%

    ## [working] (0 + 0) -> 0 -> 11 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

## Comparing multiple coded results

Once you have multiple coded results, you can assess inter-rater
reliability using
[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md).
This is useful when you want to check consistency across different
models, coders, or coding runs.

### Computing Krippendorff’s alpha

``` r
# Compare the first three runs to assess reliability
comparison <- qlm_compare(coded1, coded2, coded3, 
                          by = "score",
                          level = "ordinal")

# View the comparison results
comparison
```

    ## 

    ## ── Inter-rater reliability ──

    ## 

    ## Subjects: 11

    ## Raters: 3

    ## 

    ## ── score (ordinal)

    ## Percent agreement: 0.3636

    ## Krippendorff's alpha: 0.8593

    ## Kendall's W: 0.8657

    ## Spearman's rho: 0.8707

    ## 

The output shows:

- The reliability measures and their values, appropriate to ordinal
  data.
- The number of subjects (11 speeches) and raters (3 LLM coding runs).  
- The level of measurement (interval).

### Computing percent agreement with a tolerance

If we treat the data as ordinal, but relax the “tolerance” on the
agreement to +/-1 of the values to be compared, then we can get a
different definition of agreement, thus changing the score. “Percent
agreement” then rises substantially.

``` r
qlm_compare(coded1, coded2, coded3, 
            by = "score",
            level = "ordinal",
            tolerance = 1)
```

    ## 

    ## ── Inter-rater reliability ──

    ## 

    ## Subjects: 11

    ## Raters: 3

    ## 

    ## ── score (ordinal)

    ## Percent agreement: 0.8182

    ## Krippendorff's alpha: 0.8593

    ## Kendall's W: 0.8657

    ## Spearman's rho: 0.8707

    ## 

## Validating against a gold standard

When you have human-coded reference data (a gold standard), you can
assess the accuracy of LLM coding using
[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md).
This computes classification metrics like accuracy, precision, recall,
and F1-score.

### Creating a gold standard

For this example, let’s simulate having human-coded sentiment data:

``` r
# In practice, this would be your human-coded reference data
gold_standard <- data.frame(
  .id = coded1$.id,
  score = c(8, 7, 4, 7, 6, 7, 5, 6, 8, 3, 8)
)
```

### Computing validation metrics

``` r
# Validate the LLM coding against the gold standard
validation <- qlm_validate(coded1,
                           gold = gold_standard,
                           by = "score")
```

    ## ℹ Converting `gold` to <as_qlm_coded> object.
    ## ℹ Use `as_qlm_coded()` directly to provide coder names and metadata.

``` r
# View validation results
validation
```

    ## 
    ## ── quallmer validation ──
    ## 
    ## n: 11
    ## 
    ## 
    ## ── score (ordinal) 
    ## Spearman's rho: 0.8884
    ## Kendall's tau: 0.8000
    ## MAE: 0.7273

The output shows:

- Overall accuracy: proportion of correct classifications
- Precision: proportion of positive identifications that were actually
  correct
- Recall: proportion of actual positives that were identified correctly
- F1-score: harmonic mean of precision and recall
- Cohen’s kappa: agreement adjusted for chance

Of course, we can also perform validation treating this data as ordinal
or even interval:

``` r
qlm_validate(coded1, gold = gold_standard, by = "score", level = "ordinal")
```

    ## ℹ Converting `gold` to <as_qlm_coded> object.
    ## ℹ Use `as_qlm_coded()` directly to provide coder names and metadata.
    ## 
    ## 
    ## ── quallmer validation ──
    ## 
    ## 
    ## 
    ## n: 11
    ## 
    ## 
    ## 
    ## 
    ## 
    ## ── score (ordinal) 
    ## 
    ## Spearman's rho: 0.8884
    ## 
    ## Kendall's tau: 0.8000
    ## 
    ## MAE: 0.7273

``` r
qlm_validate(coded1, gold = gold_standard, by = "score", level = "interval")
```

    ## ℹ Converting `gold` to <as_qlm_coded> object.
    ## ℹ Use `as_qlm_coded()` directly to provide coder names and metadata.
    ## 
    ## 
    ## ── quallmer validation ──
    ## 
    ## 
    ## 
    ## n: 11
    ## 
    ## 
    ## 
    ## 
    ## 
    ## ── score (interval) 
    ## 
    ## Pearson's r: 0.8092
    ## 
    ## MAE: 0.7273
    ## 
    ## RMSE: 1.4142
    ## 
    ## ICC: 0.7460

## Best practices for reliability and validation

1.  **Multiple replications**: Run coding with at least 2-3 different
    models or settings to assess consistency
2.  **Consistent temperature**: Use `temperature = 0` for more
    deterministic and reliable results
3.  **Document settings**: Use the `name` parameter to track different
    runs
4.  **Gold standard size**: Aim for at least 100 examples in your gold
    standard for reliable validation metrics
5.  **Measure selection**:
    - Use Krippendorff’s alpha for nominal/ordinal data
    - Use Cohen’s/Fleiss’ kappa for categorical agreement
    - Use correlation measures for continuous data
6.  **Interpretation**:
    - α or κ \> 0.80: Almost perfect agreement
    - α or κ \> 0.60: Substantial agreement
    - α or κ \> 0.40: Moderate agreement
    - α or κ \< 0.40: Fair to poor agreement

## Summary

In this tutorial, you learned how to:

- Use
  [`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md)
  to systematically test coding across different models and settings
- Use
  [`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md)
  to assess inter-rater reliability between multiple coded results
- Use
  [`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md)
  to measure accuracy against a gold standard
- Interpret reliability and validation metrics

These tools help ensure that your qualitative coding is robust,
reproducible, and scientifically sound.
