# Stance detection codebook for climate change

A `qlm_codebook` object defining instructions for detecting stance
towards climate change in texts.

## Usage

``` r
data_codebook_stance
```

## Format

A `qlm_codebook` object containing:

- name:

  Task name: "Stance detection"

- instructions:

  Coding instructions for classifying stance

- schema:

  Response schema with two fields: `stance` (String indicating "Pro",
  "Neutral", or "Contra") and `explanation` (Brief explanation of the
  classification)

- role:

  Expert annotator persona

- input_type:

  "text"

## See also

[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md),
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# View the codebook
data_codebook_stance

# Use with text data
coded <- qlm_code(tail(quanteda::data_corpus_inaugural),
                  data_codebook_stance,
                  model = "openai/gpt-4o-mini")
 coded
} # }
```
