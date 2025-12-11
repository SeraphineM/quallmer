# Define an annotation task

A flexible task definition wrapper for ellmer. Supports any structured
output type, including \`type_object()\`, \`type_array()\`,
\`type_enum()\`, \`type_boolean()\`, and others.

## Usage

``` r
task(name, system_prompt, type_def, input_type = c("text", "image"))
```

## Arguments

- name:

  Name of the task.

- system_prompt:

  System prompt to guide the model (as required by ellmer's
  \`chat_fn\`).

- type_def:

  Structured output definition, e.g., created by
  \`ellmer::type_object()\`, \`ellmer::type_array()\`, or
  \`ellmer::type_enum()\`.

- input_type:

  Type of input data: \`"text"\` or \`"image"\`.

## Value

A task object with a \`run()\` method.
