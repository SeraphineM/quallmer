# Validate coded results against a gold standard

Validates LLM-coded results from one or more `qlm_coded` objects against
a gold standard (typically human annotations) using appropriate metrics
based on measurement level. For nominal data, computes accuracy,
precision, recall, F1-score, and Cohen's kappa. For ordinal data,
computes accuracy and weighted kappa (linear weighting), which accounts
for the ordering and distance between categories.

## Usage

``` r
qlm_validate(
  ...,
  gold,
  by,
  level = NULL,
  average = c("macro", "micro", "weighted", "none"),
  ci = c("none", "analytic", "bootstrap"),
  bootstrap_n = 1000
)
```

## Arguments

- ...:

  One or more data frames, `qlm_coded`, or `as_qlm_coded` objects
  containing predictions to validate. Must include a `.id` column and
  the variable(s) specified in `by`. Plain data frames are automatically
  converted to `as_qlm_coded` objects. Multiple objects will be
  validated separately against the same gold standard, and results
  combined with a `rater` column to distinguish them.

- gold:

  A data frame, `qlm_coded`, or object created with
  [`as_qlm_coded()`](https://quallmer.github.io/quallmer/reference/as_qlm_coded.md)
  containing gold standard annotations. Must include a `.id` column for
  joining with objects in `...` and the variable(s) specified in `by`.
  Plain data frames are automatically converted. **Optional** when using
  objects marked with `as_qlm_coded(data, is_gold = TRUE)` - these are
  auto-detected.

- by:

  Optional. Name of the variable(s) to validate (supports both quoted
  and unquoted). If `NULL` (default), all coded variables are validated.
  Can be a single variable (`by = sentiment`), a character vector
  (`by = c("sentiment", "rating")`), or NULL to process all variables.

- level:

  Optional. Measurement level(s) for the variable(s). Can be:

  - `NULL` (default): Auto-detect from codebook

  - Character scalar: Use same level for all variables

  - Named list: Specify level for each variable

  Valid levels are `"nominal"`, `"ordinal"`, or `"interval"`.

- average:

  Character scalar. Averaging method for multiclass metrics (nominal
  level only):

  `"macro"`

  :   Unweighted mean across classes (default)

  `"micro"`

  :   Aggregate contributions globally (sum TP, FP, FN)

  `"weighted"`

  :   Weighted mean by class prevalence

  `"none"`

  :   Return per-class metrics in addition to global metrics

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

A `qlm_validation` object (a tibble/data frame) with the following
columns:

- `variable`:

  Name of the validated variable

- `level`:

  Measurement level used

- `measure`:

  Name of the validation metric

- `value`:

  Computed value of the metric

- `class`:

  For nominal data: averaging method used (e.g., "", "", "") or class
  label (when `average = "none"`). For ordinal/interval data: NA
  (averaging not applicable).

- `rater`:

  Name of the object being validated (from input names)

- `ci_lower`:

  Lower bound of confidence interval (only if `ci != "none"`)

- `ci_upper`:

  Upper bound of confidence interval (only if `ci != "none"`)

The object has class
`c("qlm_validation", "tbl_df", "tbl", "data.frame")` and attributes
containing metadata (`n`, `call`).

**Metrics computed by measurement level:**

- **Nominal:** accuracy, precision, recall, f1, kappa

- **Ordinal:** rho (Spearman's), tau (Kendall's), mae

- **Interval:** icc, r (Pearson's), mae, rmse

**Confidence intervals:**

- `ci = "analytic"`: Provides analytic CIs for ICC and Pearson's r only

- `ci = "bootstrap"`: Provides bootstrap CIs for all metrics via
  resampling

## Details

The function performs an inner join between `x` and `gold` using the
`.id` column, so only units present in both datasets are included in
validation. Missing values (NA) in either predictions or gold standard
are excluded with a warning.

**Measurement levels:**

- **Nominal**: Categories with no inherent ordering (e.g., topics,
  sentiment polarity). Metrics: accuracy, precision, recall, F1-score,
  Cohen's kappa (unweighted).

- **Ordinal**: Categories with meaningful ordering but unequal intervals
  (e.g., ratings 1-5, Likert scales). Metrics: Spearman's rho (`rho`,
  rank correlation), Kendall's tau (`tau`, rank correlation), and MAE
  (`mae`, mean absolute error). These measures account for the ordering
  of categories without assuming equal intervals.

- **Interval/Ratio**: Numeric data with equal intervals (e.g., counts,
  continuous measurements). Metrics: ICC (intraclass correlation),
  Pearson's r (linear correlation), MAE (mean absolute error), and RMSE
  (root mean squared error).

For multiclass problems with nominal data, the `average` parameter
controls how per-class metrics are aggregated:

- **Macro averaging** computes metrics for each class independently and
  takes the unweighted mean. This treats all classes equally regardless
  of size.

- **Micro averaging** aggregates all true positives, false positives,
  and false negatives globally before computing metrics. This weights
  classes by their prevalence.

- **Weighted averaging** computes metrics for each class and takes the
  mean weighted by class size.

- **No averaging** (`average = "none"`) returns global macro-averaged
  metrics plus per-class breakdown.

Note: The `average` parameter only affects precision, recall, and F1 for
nominal data. For ordinal data, these metrics are not computed.

## See also

[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md)
for inter-rater reliability between coded objects,
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
for LLM coding,
[`as_qlm_coded()`](https://quallmer.github.io/quallmer/reference/as_qlm_coded.md)
for converting human-coded data,
[`yardstick::accuracy()`](https://yardstick.tidymodels.org/reference/accuracy.html),
[`yardstick::precision()`](https://yardstick.tidymodels.org/reference/precision.html),
[`yardstick::recall()`](https://yardstick.tidymodels.org/reference/recall.html),
[`yardstick::f_meas()`](https://yardstick.tidymodels.org/reference/f_meas.html),
[`yardstick::kap()`](https://yardstick.tidymodels.org/reference/kap.html),
[`yardstick::conf_mat()`](https://yardstick.tidymodels.org/reference/conf_mat.html)

## Examples

``` r
# Load example coded objects
examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))

# Validate against gold standard (auto-detected)
validation <- qlm_validate(
  examples$example_coded_mini,
  examples$example_gold_standard,
  by = "sentiment",
  level = "nominal"
)
print(validation)
#> 
#> ── quallmer validation ──
#> 
#> n: 5
#> 
#> 
#> ── sentiment (nominal) 
#> By class:
#> <macro>:
#> accuracy: 1.0000
#> precision: 1.0000
#> recall: 1.0000
#> F1: 1.0000
#> Cohen's kappa: 1.0000
#> 

# Explicit gold parameter (backward compatible)
validation2 <- qlm_validate(
  examples$example_coded_mini,
  gold = examples$example_gold_standard,
  by = "sentiment",
  level = "nominal"
)
print(validation2)
#> 
#> ── quallmer validation ──
#> 
#> n: 5
#> 
#> 
#> ── sentiment (nominal) 
#> By class:
#> <macro>:
#> accuracy: 1.0000
#> precision: 1.0000
#> recall: 1.0000
#> F1: 1.0000
#> Cohen's kappa: 1.0000
#> 
```
