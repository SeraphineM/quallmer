# Replicate a coding task

Re-executes a coding task from a `qlm_coded` object, optionally with
modified settings. If no overrides are provided, uses identical settings
to the original coding.

## Usage

``` r
qlm_replicate(
  x,
  ...,
  codebook = NULL,
  model = NULL,
  batch = NULL,
  name = NULL,
  notes = NULL
)
```

## Arguments

- x:

  A `qlm_coded` object.

- ...:

  Optional overrides passed to
  [`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md),
  such as `temperature` or `max_tokens`.

- codebook:

  Optional replacement codebook. If `NULL` (default), uses the codebook
  from `x`.

- model:

  Optional replacement model (e.g., `"openai/gpt-4o"`). If `NULL`
  (default), uses the model from `x`.

- batch:

  Optional logical to override batch processing setting. If `NULL`
  (default), uses the batch setting from `x`. Set to `TRUE` to use batch
  processing or `FALSE` to use parallel processing, regardless of the
  original setting.

- name:

  Optional name for this run. If `NULL`, defaults to the model name (if
  changed) or `"replication_N"` where N is the replication count.

- notes:

  Optional character string with descriptive notes about this
  replication. Useful for documenting why this replication was run or
  what differs from the original. Default is `NULL`.

## Value

A `qlm_coded` object with `run$parent` set to the parent's run name.

## See also

[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
for initial coding,
[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md)
for comparing replicated results.

## Examples

``` r
# \donttest{
# First create a coded object
texts <- c("I love this!", "Terrible.", "It's okay.")
coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini", name = "run1")

# Replicate with same model
coded2 <- qlm_replicate(coded, name = "run2")

# Compare results
qlm_compare(coded, coded2, by = "sentiment")
#> 
#> ── Inter-rater reliability ──
#> 
#> Subjects: 3
#> Raters: 2
#> 
#> 
#> ── sentiment (nominal) 
#> Percent agreement: 1.0000
#> Krippendorff's alpha: 1.0000
#> Kappa: 1.0000
#> 
# }
```
