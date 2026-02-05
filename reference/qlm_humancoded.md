# Convert human-coded data to qlm_coded format (deprecated)

This function is retained for backwards compatibility. New code should
use
[`as_qlm_coded()`](https://quallmer.github.io/quallmer/reference/as_qlm_coded.md)
instead, which provides the same functionality with an additional
`is_gold` parameter for marking gold standards.

## Usage

``` r
qlm_humancoded(
  x,
  name = NULL,
  codebook = NULL,
  texts = NULL,
  notes = NULL,
  metadata = list()
)
```

## Arguments

- x:

  A data frame containing human-coded data. Must include a `.id` column
  for unit identifiers and one or more coded variables.

- name:

  Character string identifying this coding run (e.g., "Coder_A",
  "expert_rater"). Default is `NULL`.

- codebook:

  Optional list containing coding instructions.

- texts:

  Optional vector of original texts or data that were coded.

- notes:

  Optional character string with descriptive notes.

- metadata:

  Optional list of metadata about the coding process.

## Value

A `qlm_humancoded` object (inherits from `qlm_coded`).

## See also

[`as_qlm_coded()`](https://quallmer.github.io/quallmer/reference/as_qlm_coded.md)
for the current recommended function.
