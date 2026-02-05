# Compare coded results for inter-rater reliability

Compares two or more data frames or `qlm_coded` objects to assess
inter-rater reliability or agreement. This function extracts a specified
variable from each object and computes reliability statistics using the
irr package.

## Usage

``` r
qlm_compare(
  ...,
  by,
  level = NULL,
  tolerance = 0,
  ci = c("none", "analytic", "bootstrap"),
  bootstrap_n = 1000
)
```

## Arguments

- ...:

  Two or more data frames, `qlm_coded`, or `as_qlm_coded` objects to
  compare. These represent different "raters" (e.g., different LLM runs,
  different models, human coders, or human vs. LLM coding). Each object
  must have a `.id` column and the variable specified in `by`. Objects
  should have the same units (matching `.id` values). Plain data frames
  are automatically converted to `as_qlm_coded` objects.

- by:

  Optional. Name of the variable(s) to compare across raters (supports
  both quoted and unquoted). If `NULL` (default), all coded variables
  are compared. Can be a single variable (`by = sentiment`), a character
  vector (`by = c("sentiment", "rating")`), or NULL to process all
  variables.

- level:

  Optional. Measurement level(s) for the variable(s). Can be:

  - `NULL` (default): Auto-detect from codebook

  - Character scalar: Use same level for all variables

  - Named list: Specify level for each variable

  Valid levels are `"nominal"`, `"ordinal"`, `"interval"`, or `"ratio"`.

- tolerance:

  Numeric. Tolerance for agreement with numeric data. Default is 0
  (exact agreement required). Used for percent agreement calculation.

- ci:

  Confidence interval method:

  `"none"`

  :   No confidence intervals (default)

  `"analytic"`

  :   Analytic CIs where available (ICC, Pearson's r)

  `"bootstrap"`

  :   Bootstrap CIs for all metrics via resampling

- bootstrap_n:

  Number of bootstrap resamples when `ci = "bootstrap"`. Default
  is 1000. Ignored when `ci` is `"none"` or `"analytic"`.

## Value

A `qlm_comparison` object (a tibble/data frame) with the following
columns:

- `variable`:

  Name of the compared variable

- `level`:

  Measurement level used

- `measure`:

  Name of the reliability metric

- `value`:

  Computed value of the metric

- `rater1`, `rater2`, ...:

  Names of the compared objects (one column per rater)

- `ci_lower`:

  Lower bound of confidence interval (only if `ci != "none"`)

- `ci_upper`:

  Upper bound of confidence interval (only if `ci != "none"`)

The object has class
`c("qlm_comparison", "tbl_df", "tbl", "data.frame")` and attributes
containing metadata (`raters`, `n`, `call`).

**Metrics computed by measurement level:**

- **Nominal:** alpha_nominal, kappa (Cohen's/Fleiss'), percent_agreement

- **Ordinal:** alpha_ordinal, kappa_weighted (2 raters only), w
  (Kendall's W), rho (Spearman's), percent_agreement

- **Interval/Ratio:** alpha_interval/alpha_ratio, icc, r (Pearson's),
  percent_agreement

**Confidence intervals:**

- `ci = "analytic"`: Provides analytic CIs for ICC and Pearson's r only

- `ci = "bootstrap"`: Provides bootstrap CIs for all metrics via
  resampling

## Details

The function merges the coded objects by their `.id` column and only
includes units that are present in all objects. Missing values in any
rater will exclude that unit from analysis.

**Measurement levels and statistics:**

- **Nominal**: For unordered categories. Computes Krippendorff's alpha,
  Cohen's/Fleiss' kappa, and percent agreement.

- **Ordinal**: For ordered categories. Computes Krippendorff's alpha
  (ordinal), weighted kappa (2 raters only), Kendall's W, Spearman's
  rho, and percent agreement.

- **Interval**: For continuous data with meaningful intervals. Computes
  Krippendorff's alpha (interval), ICC, Pearson's r, and percent
  agreement.

- **Ratio**: For continuous data with a true zero point. Computes the
  same measures as interval level, but Krippendorff's alpha uses the
  ratio-level formula which accounts for proportional differences.

Kendall's W, ICC, and percent agreement are computed using all raters
simultaneously. For 3 or more raters, Spearman's rho and Pearson's r are
computed as the mean of all pairwise correlations between raters.

## See also

[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md)
for validation of coding against gold standards,
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
for LLM coding,
[`as_qlm_coded()`](https://quallmer.github.io/quallmer/reference/as_qlm_coded.md)
for human coding.

## Examples

``` r
# Load example coded objects
examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))

# Compare two coding runs
comparison <- qlm_compare(
  examples$example_coded_sentiment,
  examples$example_coded_mini,
  by = "sentiment",
  level = "nominal"
)
print(comparison)
#> 
#> ── Inter-rater reliability ──
#> 
#> Subjects: 5
#> Raters: 2
#> 
#> 
#> ── sentiment (nominal) 
#> Percent agreement: 1.0000
#> Krippendorff's alpha: 1.0000
#> Kappa: 1.0000
#> 

# Compare specific variables with explicit levels
qlm_compare(
  examples$example_coded_sentiment,
  examples$example_coded_mini,
  by = "sentiment"
)
#> 
#> ── Inter-rater reliability ──
#> 
#> Subjects: 5
#> Raters: 2
#> 
#> 
#> ── sentiment (nominal) 
#> Percent agreement: 1.0000
#> Krippendorff's alpha: 1.0000
#> Kappa: 1.0000
#> 
```
