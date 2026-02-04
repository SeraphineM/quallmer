# Convert coded data to qlm_coded format

Converts a data frame of coded data (human-coded or from external
sources) into a `qlm_coded` object. This enables provenance tracking and
integration with
[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md),
[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md),
and
[`qlm_trail()`](https://quallmer.github.io/quallmer/reference/qlm_trail.md)
for coded data alongside LLM-coded results.

## Usage

``` r
as_qlm_coded(
  x,
  name = NULL,
  is_gold = FALSE,
  codebook = NULL,
  texts = NULL,
  metadata = list()
)
```

## Arguments

- x:

  A data frame containing coded data. Must include a `.id` column for
  unit identifiers and one or more coded variables.

- name:

  Character string identifying this coding run (e.g., "Coder_A",
  "expert_rater", "Gold_Standard"). Default is `NULL`.

- is_gold:

  Logical. If `TRUE`, marks this object as a gold standard for automatic
  detection by
  [`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md).
  When a gold standard object is passed to
  [`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md),
  the `gold =` parameter becomes optional. Default is `FALSE`.

- codebook:

  Optional list containing coding instructions. Can include:

  `name`

  :   Name of the coding scheme

  `instructions`

  :   Text describing coding instructions

  `schema`

  :   NULL (not used for human coding)

  If `NULL` (default), a minimal placeholder codebook is created.

- texts:

  Optional vector of original texts or data that were coded. Should
  correspond to the `.id` values in `data`. If provided, enables more
  complete provenance tracking.

- metadata:

  Optional list of metadata about the coding process. Can include any
  relevant information such as:

  `coder_name`

  :   Name of the human coder

  `coder_id`

  :   Identifier for the coder

  `training`

  :   Description of coder training

  `date`

  :   Date of coding

  `notes`

  :   Any additional notes

  The function automatically adds `timestamp`, `n_units`, and
  `source = "human"`.

## Value

A `qlm_coded` object (tibble with additional class and attributes) for
provenance tracking. When `is_gold = TRUE`, the object is marked as a
gold standard in its attributes.

## Details

When printed, objects created with `as_qlm_coded()` display "Source:
Human coder" instead of model information, clearly distinguishing human
from LLM coding.

### Gold Standards

Objects marked with `is_gold = TRUE` are automatically detected by
[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md),
allowing simpler syntax:

    # With is_gold = TRUE
    gold <- as_qlm_coded(gold_data, name = "Expert", is_gold = TRUE)
    qlm_validate(coded1, coded2, gold, by = "sentiment")  # gold = not needed!

    # Without is_gold (or explicit gold =)
    gold <- as_qlm_coded(gold_data, name = "Expert")
    qlm_validate(coded1, coded2, gold = gold, by = "sentiment")

## See also

[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
for LLM coding,
[`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md)
for inter-rater reliability,
[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md)
for validation against gold standards,
[`qlm_trail()`](https://quallmer.github.io/quallmer/reference/qlm_trail.md)
for provenance tracking.

## Examples

``` r
# Basic usage
human_data <- data.frame(
  .id = 1:10,
  sentiment = sample(c("pos", "neg"), 10, replace = TRUE)
)

coder_a <- as_qlm_coded(human_data, name = "Coder_A")
coder_a
#> # quallmer coded object
#> # Run:      Coder_A
#> # Source:   Human coder
#> # Units:    10
#> 
#> # A tibble: 10 × 2
#>      .id sentiment
#>  * <int> <chr>    
#>  1     1 pos      
#>  2     2 neg      
#>  3     3 pos      
#>  4     4 pos      
#>  5     5 pos      
#>  6     6 neg      
#>  7     7 neg      
#>  8     8 pos      
#>  9     9 pos      
#> 10    10 pos      

# Create a gold standard
gold <- as_qlm_coded(
  human_data,
  name = "Expert",
  is_gold = TRUE
)

# Validate with automatic gold detection
coder_b_data <- data.frame(
  .id = 1:10,
  sentiment = sample(c("pos", "neg"), 10, replace = TRUE)
)
coder_b <- as_qlm_coded(coder_b_data, name = "Coder_B")

# No need for gold = when gold object is marked
qlm_validate(coder_a, coder_b, gold, by = "sentiment", level = "nominal")
#> 
#> ── quallmer validation ──
#> 
#> n: 10
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
#> accuracy: 0.7000
#> precision: 0.6667
#> recall: 0.6905
#> F1: 0.6703
#> Cohen's kappa: 0.3478
#> 

# With complete metadata
expert <- as_qlm_coded(
  human_data,
  name = "expert_rater",
  is_gold = TRUE,
  codebook = list(
    name = "Sentiment Analysis",
    instructions = "Code overall sentiment as positive or negative"
  ),
  metadata = list(
    coder_name = "Dr. Smith",
    coder_id = "EXP001",
    training = "5 years experience",
    date = "2024-01-15"
  )
)
```
