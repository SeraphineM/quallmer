# Fact-checking codebook

A `qlm_codebook` object defining instructions for assessing the
truthfulness and accuracy of texts.

## Usage

``` r
data_codebook_fact
```

## Format

A `qlm_codebook` object containing:

- name:

  Task name: "Fact-checking"

- instructions:

  Coding instructions for truthfulness assessment

- schema:

  Response schema with three fields: `truth_score` (Integer from 0
  (false/misleading) to 10 (accurate)), `misleading_topic` (Array of
  topics that reduce confidence, up to 5), and `explanation` (Brief
  explanation of the truthfulness score)

- role:

  Expert fact-checker persona

- input_type:

  "text"

## See also

[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md),
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# View the codebook
data_codebook_fact

# Use with claims or articles
# NEEDS ACTUAL DATA
coded <- qlm_code(claims,
                  data_codebook_fact,
                  model = "openai/gpt-4o-mini")
} # }
```
