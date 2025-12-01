# Compute intercoder reliability statistics

This function computes a set of intercoder reliability statistics for
nominal coding data with multiple coders: Krippendorff's alpha
(nominal), Fleiss' kappa, mean pairwise Cohen's kappa, mean pairwise
percent agreement, share of unanimous units, and basic counts.

## Usage

``` r
agreement(data, unit_id_col, coder_cols, min_coders = 2L)
```

## Arguments

- data:

  A data frame containing the unit identifier and coder columns.

- unit_id_col:

  Character scalar. Name of the column identifying units (e.g. document
  ID, paragraph ID).

- coder_cols:

  Character vector. Names of columns containing the coders' codes (each
  column = one coder).

- min_coders:

  Integer: minimum number of non-missing coders per unit for that unit
  to be included. Default is 2.

## Value

A data frame with two columns:

- metric:

  Name of the statistic

- value:

  Estimated value

## Examples

``` r
if (FALSE) { # \dontrun{
agreement(
  data = my_df,
  unit_id_col = "doc_id",
  coder_cols  = c("coder1", "coder2", "coder3")
)
} # }
```
