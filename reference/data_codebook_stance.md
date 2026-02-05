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
# View the codebook
data_codebook_stance
#> quallmer codebook: Stance detection 
#>   Input type:   text
#>   Role:         You are an expert in political communication and discourse a...
#>   Instructions: Classify the stance towards climate change expressed in this...
#>   Output schema:ellmer::TypeObject
#>   Levels:
#>     stance: nominal
#>     explanation: nominal
```
