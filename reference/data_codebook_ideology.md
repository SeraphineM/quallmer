# Ideological scaling codebook for left-right dimension

A `qlm_codebook` object defining instructions for scaling texts on a
left-right ideological dimension.

## Usage

``` r
data_codebook_ideology
```

## Format

A `qlm_codebook` object containing:

- name:

  Task name: "Ideological scaling"

- instructions:

  Coding instructions for ideological scaling

- schema:

  Response schema with two fields: `score` (Integer from 0 (left) to 10
  (right)) and `explanation` (Brief justification for the assigned
  score)

- role:

  Expert political scientist persona

- input_type:

  "text"

## See also

[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md),
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)

## Examples

``` r
# View the codebook
data_codebook_ideology
#> quallmer codebook: Ideological scaling 
#>   Input type:   text
#>   Role:         You are an expert political scientist specializing in ideolo...
#>   Instructions: Rate the ideological position of this text on a scale from 0...
#>   Output schema:ellmer::TypeObject
#>   Levels:
#>     score: ordinal
#>     explanation: nominal
```
