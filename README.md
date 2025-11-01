
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quallmer <a href="https://SeraphineM.github.io/quallmer/"><img src="man/figures/logo.png" align="center" height="200" alt="quallmer website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/quanteda.llm)](https://CRAN.R-project.org/package=quallmer)
[![R-CMD-check](https://SeraphineM.github.io/quallmer//actions/workflows/R-CMD-check.yaml/badge.svg)](https://SeraphineM.github.io/quallmer/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/quanteda/quanteda.llm/graph/badge.svg)](https://app.codecov.io/gh/quanteda/quanteda.llm)
[![pkgdown](https://img.shields.io/badge/pkgdown-site-blue)](https://SeraphineM.github.io/quallmer/)
<!-- badges: end -->

`quallmer` is an **accessible and easy-to-use R package for qualitative
researchers to quickly apply AI-assisted annotation to texts, images,
pdfs, and other structured data.** Using `annotate()` and predefined or
custom tasks via `define_task()`, users can generate structured,
interpretable outputs powered by large language models (LLMs). The
package includes a library of example tasks (more to come). It also
provides `agreement()`, which launches an interactive app to manually
code data, compare LLM outputs with human annotations, and assess
inter-coder reliability. `quallmer` makes AI-assisted qualitative coding
accessible without requiring deep expertise in programming or machine
learning.

# Included functions

The package provides the following core functions:

### `annotate()`

- A generic function that works with any LLM supported by
  [ellmer](https://ellmer.tidyverse.org/articles/structured-data.html).  
- Generates structured responses based on predefined or user-defined
  tasks.

### `define_task()`

- Create custom annotation tasks with structured outputs.  
- Uses `system_prompt` and `type_object()` to define how the LLM should
  interpret inputs and format outputs.  
- Tasks created with `define_task()` can be passed directly to
  `annotate()`.  
- This allows users to tailor the annotation process to their specific
  data types and makes our package extensible for future use cases.

### `agreement()`

- Launches an interactive app to manually code data and review
  LLM-generated annotations.  
- Compares AI annotations with human-coded data to evaluate agreement
  and inter-coder reliability.

# Supported LLMs

The package supports all LLMs currently available with the `ellmer`
package:

For authentication and usage of each of these LLMs, please refer to the
respective `ellmer` documentation
[here](https://ellmer.tidyverse.org/reference/index.html). **For
example,** to use the `chat_openai` models, you would need to sign up
for an API key from
[OpenAI](https://platform.openai.com/playground/prompts) which you can
save in your `.Renviron` file as `OPENAI_API_KEY`. To use the
`chat_ollama` models, first download and install
[Ollama](https://ollama.com/). Then install some models either from the
command line (e.g. with ollama pull llama3.1) or within R using the
`rollama` package. The Ollama app must be running for the models to be
used.

## Installation

You can install the development version of **quallmer** from
<https://github.com/SeraphineM/quallmer> with:

``` r
# install.packages("pak")
pak::pak("SeraphineM/quallmer")
```

## Example use

To learn more about how to use the package, please refer to the
following examples:

- [Sentiment analysis with LLMs](...)
- More to come.
