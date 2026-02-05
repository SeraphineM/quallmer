# Create an audit trail from quallmer objects

Creates a complete audit trail documenting your qualitative coding
workflow. Following Lincoln and Guba's (1985) concept of the audit trail
for establishing trustworthiness in qualitative research, this function
captures the full decision history of your AI-assisted coding process.

## Usage

``` r
qlm_trail(..., path = NULL)
```

## Arguments

- ...:

  One or more quallmer objects (`qlm_coded`, `qlm_comparison`, or
  `qlm_validation`). When multiple objects are provided, they will be
  used to reconstruct the complete workflow chain.

- path:

  Optional base path for saving the audit trail. When provided, creates
  `{path}.rds` (complete archive) and `{path}.qmd` (human-readable
  report). If `NULL` (default), the trail is only returned without
  saving.

## Value

A `qlm_trail` object containing:

- runs:

  List of run information with coded data, ordered from oldest to newest

- complete:

  Logical indicating whether all parent references were resolved

## Details

Lincoln and Guba (1985, pp. 319-320) describe six categories of audit
trail materials for establishing trustworthiness in qualitative
research. The quallmer package operationalizes these for LLM-assisted
text analysis:

- Raw data:

  Original texts stored in coded objects

- Data reduction products:

  Coded results from each run

- Data reconstruction products:

  Comparisons and validations

- Process notes:

  Model parameters, timestamps, decision history

- Materials relating to intentions:

  Function calls documenting intent

- Instrument development information:

  Codebook with instructions and schema

When `path` is provided, the function creates:

- `{path}.rds`: Complete trail object for R (reloadable with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html))

- `{path}.qmd`: Quarto document with full audit trail documentation

## References

Lincoln, Y. S., & Guba, E. G. (1985). *Naturalistic Inquiry*. Sage.

## See also

[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md),
[`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md),
[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md),
[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md)

## Examples

``` r
# Load example coded objects
examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))

# View audit trail from two coding runs
trail <- qlm_trail(
  examples$example_coded_sentiment,
  examples$example_coded_mini
)
print(trail)
#> # quallmer audit trail (2 runs)
#> 
#> 1. example_sentiment (original)
#>    2026-02-05 01:29 | openai/gpt-4.1
#>    Codebook: Sentiment analysis
#> 
#> 2. example_mini (parent: example_sentiment)
#>    2026-02-05 01:29 | openai/gpt-4.1-mini
#>    Codebook: Sentiment analysis

# \donttest{
# Save complete audit trail (creates .rds and .qmd files)
qlm_trail(
  examples$example_coded_sentiment,
  examples$example_coded_mini,
  path = tempfile("my_analysis")
)
#> ✔ Trail saved to /tmp/RtmpACzeKe/my_analysis1b8c4c94f03b.rds
#> ✔ Report saved to /tmp/RtmpACzeKe/my_analysis1b8c4c94f03b.qmd
#> ℹ Render with `quarto::quarto_render("/tmp/RtmpACzeKe/my_analysis1b8c4c94f03b.qmd")`
# }
```
