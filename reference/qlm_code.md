# Code qualitative data with an LLM

Applies a codebook to input data using a large language model, returning
a rich object that includes the codebook, execution settings, results,
and metadata for reproducibility.

## Usage

``` r
qlm_code(x, codebook, model, ..., batch = FALSE, name = NULL)
```

## Arguments

- x:

  Input data: a character vector of texts (for text codebooks) or file
  paths to images (for image codebooks). Named vectors will use names as
  identifiers in the output; unnamed vectors will use sequential
  integers.

- codebook:

  A codebook object created with
  [`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md).
  Also accepts deprecated
  [`task()`](https://quallmer.github.io/quallmer/reference/task.md)
  objects for backward compatibility.

- model:

  Provider (and optionally model) name in the form `"provider/model"` or
  `"provider"` (which will use the default model for that provider).
  Passed to the `name` argument of
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html).
  Examples: `"openai/gpt-4o-mini"`,
  `"anthropic/claude-3-5-sonnet-20241022"`, `"ollama/llama3.2"`,
  `"openai"` (uses default OpenAI model).

- ...:

  Additional arguments passed to
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html),
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html),
  or
  [`ellmer::batch_chat_structured()`](https://ellmer.tidyverse.org/reference/batch_chat.html),
  based on argument name. Arguments recognized by
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html)
  take priority when there are overlaps. Batch-specific arguments
  (`path`, `wait`, `ignore_hash`) are only used when `batch = TRUE`.
  Arguments not recognized by any function will generate a warning.

- batch:

  Logical. If `TRUE`, uses
  [`ellmer::batch_chat_structured()`](https://ellmer.tidyverse.org/reference/batch_chat.html)
  instead of
  [`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html).
  Batch processing is more cost-effective for large jobs but may have
  longer turnaround times. Default is `FALSE`. See
  [`ellmer::batch_chat_structured()`](https://ellmer.tidyverse.org/reference/batch_chat.html)
  for details.

- name:

  Character string identifying this coding run. Default is `NULL`.

## Value

A `qlm_coded` object (a tibble with additional attributes):

- Data columns:

  The coded results with a `.id` column for identifiers.

- Attributes:

  `data`, `input_type`, and `run` (list containing name, batch, call,
  codebook, chat_args, execution_args, metadata, parent).

The object prints as a tibble and can be used directly in data
manipulation workflows. The `batch` flag in the `run` attribute
indicates whether batch processing was used. The `execution_args`
contains all non-chat execution arguments (for either parallel or batch
processing).

## Details

Arguments in `...` are dynamically routed to either
[`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html),
[`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html),
or
[`ellmer::batch_chat_structured()`](https://ellmer.tidyverse.org/reference/batch_chat.html)
based on their names.

Progress indicators and error handling are provided by the underlying
[`ellmer::parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html)
or
[`ellmer::batch_chat_structured()`](https://ellmer.tidyverse.org/reference/batch_chat.html)
function. Set `verbose = TRUE` to see progress messages during coding.
Retry logic for API failures should be configured through ellmer's
options.

When `batch = TRUE`, the function uses
[`ellmer::batch_chat_structured()`](https://ellmer.tidyverse.org/reference/batch_chat.html)
which submits jobs to the provider's batch API. This is typically more
cost-effective but has longer turnaround times. The `path` argument
specifies where batch results are cached, `wait` controls whether to
wait for completion, and `ignore_hash` can force reprocessing of cached
results.

## See also

[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md)
for creating codebooks,
[`annotate()`](https://quallmer.github.io/quallmer/reference/annotate.md)
for the deprecated function.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(24)
texts <- data_corpus_LMRDsample[sample(length(data_corpus_LMRDsample), size = 20)]

# Basic sentiment analysis
coded <- qlm_code(texts, data_codebook_sentiment, model = "openai")
coded  # Print results as tibble

# With named inputs (names become IDs in output)
texts <- c(doc1 = "Great service!", doc2 = "Very disappointing.")
coded <- qlm_code(texts, data_codebook_sentiment, model = "openai")

# Specify provider and model
coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini")

# With execution control
coded <- qlm_code(texts, data_codebook_sentiment,
                  model = "openai/gpt-4o-mini",
                  params = params(temperature = 0))

# Include token usage and cost
coded <- qlm_code(texts, data_codebook_sentiment,
                  model = "openai",
                  include_tokens = TRUE,
                  include_cost = TRUE)
coded

# Use batch processing for cost-effective large-scale coding
coded_batch <- qlm_code(texts, data_codebook_sentiment,
                        model = "openai",
                        batch = TRUE,
                        path = "batch_results.json",
                        ignore_hash = TRUE,
                        include_cost = TRUE)
coded_batch
} # }
```
