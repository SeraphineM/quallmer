# The quallmer audit trail

The quallmer trail system creates complete audit trails following
Lincoln and Guba’s (1985) concept for establishing trustworthiness in
qualitative research. It automatically captures the full decision
history of your coding workflow, supporting the confirmability and
dependability criteria that allow others to trace the logic of your
analytical decisions.

## Lincoln and Guba’s audit trail framework

Lincoln and Guba (1985, p. 319-320) describe six categories of audit
trail materials:

1.  **Raw data:** Original texts, recordings, notes
2.  **Data reduction products:** Summaries, condensed notes
3.  **Data reconstruction products:** Themes, interpretations, findings
4.  **Process notes:** Methodological decisions, procedures
5.  **Materials relating to intentions:** Proposals, personal notes
6.  **Instrument development information:** Codebooks, protocols

## How quallmer implements the audit trail

The quallmer package operationalizes this framework for LLM-assisted
text analysis:

    qlm_code()      ->  Produces coded results (raw output)
          |
    qlm_replicate() ->  Re-runs with modifications (process notes)
          |
    qlm_trail()     ->  Documents complete workflow (audit trail)

| Audit trail component              | quallmer implementation                        |
|------------------------------------|------------------------------------------------|
| Raw data                           | Original texts stored in coded objects         |
| Data reduction products            | Coded results from each run                    |
| Data reconstruction products       | Comparisons and validations                    |
| Process notes                      | Model parameters, timestamps, decision history |
| Materials relating to intentions   | Function calls documenting intent              |
| Instrument development information | Codebook with instructions and schema          |

## Basic usage

First, let’s load the package and create some example coded objects to
demonstrate the trail system:

``` r
library(quallmer)
#> Loading required package: ellmer
```

In a real workflow, you would use
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
and
[`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md):

``` r
# Initial coding run (requires API key)
coded1 <- qlm_code(
  texts,
  data_codebook_sentiment,
  model = "openai/gpt-4o",
  name = "gpt4o_run"
)

# Replicate with different model
coded2 <- qlm_replicate(
  coded1,
  model = "openai/gpt-4o-mini",
  name = "mini_run"
)
```

For this tutorial, we’ll create mock coded objects that show what the
trail system captures:

``` r
# Create first coded object (simulating qlm_code output)
coded1 <- data.frame(
  .id = 1:5,
  text = c(
    "I love this product!",
    "Terrible experience.",
    "It's okay, nothing special.",
    "Best purchase ever!",
    "Would not recommend."
  ),
  sentiment = c("positive", "negative", "neutral", "positive", "negative")
)
class(coded1) <- c("qlm_coded", "data.frame")

attr(coded1, "run") <- list(
  name = "gpt4o_run",
  parent = NULL,
  call = quote(qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o")),
  metadata = list(
    timestamp = as.POSIXct("2024-06-15 10:30:00"),
    n_units = 5,
    quallmer_version = packageVersion("quallmer"),
    ellmer_version = packageVersion("ellmer"),
    R_version = R.version.string
  ),
  chat_args = list(name = "openai/gpt-4o", temperature = 0),
  codebook = list(
    name = "Sentiment Analysis",
    instructions = "Classify the sentiment of each text as positive, negative, or neutral."
  )
)

# Create second coded object (simulating qlm_replicate output)
coded2 <- data.frame(
  .id = 1:5,
  text = c(
    "I love this product!",
    "Terrible experience.",
    "It's okay, nothing special.",
    "Best purchase ever!",
    "Would not recommend."
  ),
  sentiment = c("positive", "negative", "neutral", "positive", "negative")
)
class(coded2) <- c("qlm_coded", "data.frame")

attr(coded2, "run") <- list(
  name = "mini_run",
  parent = "gpt4o_run",
  call = quote(qlm_replicate(coded1, model = "openai/gpt-4o-mini")),
  metadata = list(
    timestamp = as.POSIXct("2024-06-15 10:35:00"),
    n_units = 5,
    quallmer_version = packageVersion("quallmer"),
    ellmer_version = packageVersion("ellmer"),
    R_version = R.version.string
  ),
  chat_args = list(name = "openai/gpt-4o-mini", temperature = 0),
  codebook = list(
    name = "Sentiment Analysis",
    instructions = "Classify the sentiment of each text as positive, negative, or neutral."
  )
)
```

Now we can create and view the audit trail:

``` r
# Create the audit trail
trail <- qlm_trail(coded1, coded2)

# View the trail
print(trail)
#> # quallmer audit trail (2 runs)
#> 
#> 1. gpt4o_run (original)
#>    2024-06-15 10:30 | openai/gpt-4o
#>    Codebook: Sentiment Analysis
#> 
#> 2. mini_run (parent: gpt4o_run)
#>    2024-06-15 10:35 | openai/gpt-4o-mini
#>    Codebook: Sentiment Analysis
```

The trail shows:

- The chronological order of runs (parent first, then children)
- Timestamps and model information for each run
- Parent-child relationships between runs
- Codebook used for each run

## Saving the audit trail

To create a permanent record, provide a `path` argument:

``` r
# Save to a temporary directory for this example
temp_path <- file.path(tempdir(), "my_analysis")
qlm_trail(coded1, coded2, path = temp_path)
#> ✔ Trail saved to /tmp/RtmpVG0895/my_analysis.rds
#> ✔ Report saved to /tmp/RtmpVG0895/my_analysis.qmd
#> ℹ Render with `quarto::quarto_render("/tmp/RtmpVG0895/my_analysis.qmd")`
```

This creates two files:

- **my_analysis.rds**: Complete trail object with all coded data
  (reloadable with [`readRDS()`](https://rdrr.io/r/base/readRDS.html))
- **my_analysis.qmd**: Quarto document with full audit trail
  documentation

The Quarto report includes all Lincoln and Guba audit trail components:

- **Trail summary**: Overview and system information
- **Instrument development**: Codebooks with instructions and schema
- **Process notes**: Chronological record of all runs with parameters
- **Raw data and coded results**: Summary of data stored in RDS
- **Replication**: Environment setup and executable code

## Including comparisons

When you include comparison objects, the trail captures the complete
assessment workflow:

``` r
# Create a comparison object (simulating qlm_compare output)
comparison <- list(
  level = "nominal",
  subjects = 5,
  raters = 2,
  alpha_nominal = 1.0,
  kappa = 1.0,
  kappa_type = "Cohen's",
  percent_agreement = 1.0
)
class(comparison) <- "qlm_comparison"

attr(comparison, "run") <- list(
  name = "irr_comparison",
  parent = c("gpt4o_run", "mini_run"),
  metadata = list(timestamp = as.POSIXct("2024-06-15 10:40:00"))
)

# Create trail with comparison
trail_with_comparison <- qlm_trail(coded1, coded2, comparison)
print(trail_with_comparison)
#> # quallmer audit trail (3 runs)
#> 
#> 1. gpt4o_run (original)
#>    2024-06-15 10:30 | openai/gpt-4o
#>    Codebook: Sentiment Analysis
#> 
#> 2. mini_run (parent: gpt4o_run)
#>    2024-06-15 10:35 | openai/gpt-4o-mini
#>    Codebook: Sentiment Analysis
#> 
#> 3. irr_comparison (parents: gpt4o_run, mini_run)
#>    2024-06-15 10:40 | unknown
#>    Comparison: nominal level | 5 subjects | 2 raters
```

The trail now includes comparison information showing which runs were
compared and the inter-rater reliability metrics.

## Loading saved trails

``` r
# Reload a saved trail
loaded_trail <- readRDS(paste0(temp_path, ".rds"))

# Access coded data from a specific run
coded_data <- loaded_trail$runs$gpt4o_run$data
head(coded_data)
#>   .id                        text sentiment
#> 1   1        I love this product!  positive
#> 2   2        Terrible experience.  negative
#> 3   3 It's okay, nothing special.   neutral
#> 4   4         Best purchase ever!  positive
#> 5   5        Would not recommend.  negative

# View run metadata
loaded_trail$runs$gpt4o_run$metadata$timestamp
#> [1] "2024-06-15 10:30:00 UTC"
loaded_trail$runs$gpt4o_run$chat_args$name
#> [1] "openai/gpt-4o"
```

## Replication materials

The generated Quarto report serves as comprehensive replication
materials. The replication section includes:

- **Environment setup**: Package installation and API credential
  configuration
- **Loading the trail**: Code to load the RDS file
- **Replication code**: Executable R code to replicate each coding run
- **Note on reproducibility**: Guidance on LLM stochasticity

This makes it easy for other researchers to reproduce or extend your
analysis.

## Summary

The
[`qlm_trail()`](https://quallmer.github.io/quallmer/reference/qlm_trail.md)
function provides workflow traceability following Lincoln and Guba’s
(1985) audit trail concept:

1.  **Automatic tracking**: Every
    [`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
    and
    [`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md)
    captures the full decision history
2.  **Simple extraction**:
    [`qlm_trail()`](https://quallmer.github.io/quallmer/reference/qlm_trail.md)
    reconstructs your complete audit trail
3.  **Easy archiving**: Add `path` to save RDS and generate Quarto
    report with replication instructions

Always name your runs with the `name` parameter to make trails easier to
interpret.

## Reference

Lincoln, Y. S., & Guba, E. G. (1985). *Naturalistic Inquiry*. Sage.
