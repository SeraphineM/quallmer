# Trail settings specification

Simple description of an LLM configuration for use with quallmer Trails.

## Usage

``` r
trail_settings(
  provider = "openai",
  model = "gpt-4.1-mini",
  temperature = 0,
  extra = list()
)
```

## Arguments

- provider:

  Character. Backend provider identifier, e.g. "openai", "ollama",
  "azure", etc.

- model:

  Character. Model identifier, e.g. "gpt-4.1-mini", "gpt-4o-mini",
  "llama3.2:1b".

- temperature:

  Numeric scalar. Sampling temperature (default 0).

- extra:

  Named list of extra arguments merged into \`api_args\`.

## Value

An object of class `"trail_setting"`.
