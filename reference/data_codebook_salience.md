# Topic salience codebook

A `qlm_codebook` object defining instructions for extracting and ranking
topics discussed in texts by their salience.

## Usage

``` r
data_codebook_salience
```

## Format

A `qlm_codebook` object containing:

- name:

  Task name: "Salience (ranked topics)"

- instructions:

  Coding instructions for topic salience ranking

- schema:

  Response schema with two fields: `topics` (Array of strings listing
  topics by salience, up to 5) and `explanation` (Brief explanation of
  topic selection and ordering)

- role:

  Expert content analyst persona

- input_type:

  "text"

## See also

[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md),
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# View the codebook
data_codebook_salience

# Use with documents
coded <- qlm_code(tail(quanteda::data_corpus_inaugural),
                  data_codebook_salience,
                  model = "openai/gpt-4o-mini")
coded
} # }
```
