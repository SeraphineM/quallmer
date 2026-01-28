# Define a qualitative codebook

Creates a codebook definition for use with
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md).
A codebook specifies what information to extract from input data,
including the instructions that guide the LLM and the structured output
schema.

## Usage

``` r
qlm_codebook(
  name,
  instructions,
  schema,
  role = NULL,
  input_type = c("text", "image"),
  levels = NULL
)
```

## Arguments

- name:

  Name of the codebook (character).

- instructions:

  Instructions to guide the model in performing the coding task.

- schema:

  Structured output definition, e.g., created by
  [`ellmer::type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
  [`ellmer::type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
  or
  [`ellmer::type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html).

- role:

  Optional role description for the model (e.g., "You are an expert
  annotator"). If provided, this will be prepended to the instructions
  when creating the system prompt.

- input_type:

  Type of input data: `"text"` (default) or `"image"`.

- levels:

  Optional named list specifying measurement levels for each variable in
  the schema. Names should match schema property names. Values should be
  one of `"nominal"`, `"ordinal"`, `"interval"`, or `"ratio"`. If `NULL`
  (default), levels are auto-detected from schema types using the
  following mapping: `type_boolean` and `type_enum` = nominal,
  `type_string` = nominal, `type_integer` = ordinal, `type_number` =
  interval.

## Value

A codebook object (a list with class `c("qlm_codebook", "task")`)
containing the codebook definition. Use with
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
to apply the codebook to data.

## Details

This function replaces
[`task()`](https://quallmer.github.io/quallmer/reference/task.md), which
is now deprecated. The returned object has dual class inheritance
(`c("qlm_codebook", "task")`) to maintain backward compatibility with
existing code using
[`annotate()`](https://quallmer.github.io/quallmer/reference/annotate.md).

## See also

[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
for applying codebooks to data,
[data_codebook_sentiment](https://quallmer.github.io/quallmer/reference/data_codebook_sentiment.md),
[data_codebook_stance](https://quallmer.github.io/quallmer/reference/data_codebook_stance.md),
[data_codebook_ideology](https://quallmer.github.io/quallmer/reference/data_codebook_ideology.md),
[data_codebook_salience](https://quallmer.github.io/quallmer/reference/data_codebook_salience.md),
[data_codebook_fact](https://quallmer.github.io/quallmer/reference/data_codebook_fact.md)
for predefined codebooks,
[`task()`](https://quallmer.github.io/quallmer/reference/task.md) for
the deprecated function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define a custom codebook
my_codebook <- qlm_codebook(
  name = "Sentiment",
  instructions = "Rate the sentiment from -1 (negative) to 1 (positive).",
  schema = type_object(
    score = type_number("Sentiment score from -1 to 1"),
    explanation = type_string("Brief explanation")
  )
)

# With a role
my_codebook <- qlm_codebook(
  name = "Sentiment",
  instructions = "Rate the sentiment from -1 (negative) to 1 (positive).",
  schema = type_object(
    score = type_number("Sentiment score from -1 to 1"),
    explanation = type_string("Brief explanation")
  ),
  role = "You are an expert sentiment analyst."
)

# With explicit measurement levels
my_codebook_levels <- qlm_codebook(
  name = "Sentiment",
  instructions = "Rate the sentiment from -1 (negative) to 1 (positive).",
  schema = type_object(
    score = type_number("Sentiment score from -1 to 1"),
    explanation = type_string("Brief explanation")
  ),
  levels = list(score = "interval", explanation = "nominal")
)

# Use with qlm_code()
texts <- c("I love this!", "This is terrible.")
coded <- qlm_code(texts, my_codebook, model = "openai/gpt-4o-mini")
coded  # Print results as tibble
} # }
```
