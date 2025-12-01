# quallmer

The `quallmer` package is an **easy-to-use toolbox for qualitative
researchers to quickly apply AI-assisted annotation to texts, images,
pdfs, tabular data and other structured data.**

Using
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md),
users can generate structured, interpretable outputs powered by large
language models (LLMs). The package includes a library of [predefined
tasks for common qualitative coding
needs](https://seraphinem.github.io/quallmer/articles/pkgdown/examples/overview.html),
such as sentiment analysis, thematic coding, and stance detection. It
also allows users to [create their own custom annotation tasks tailored
to their specific research questions and data
types](https://seraphinem.github.io/quallmer/articles/pkgdown/tutorials/customtask.html)
using
[`task()`](https://seraphinem.github.io/quallmer/reference/task.md). To
ensure quality and reliability of AI-generated annotations, `quallmer`
offers tools for comparing LLM outputs with human-coded data and
assessing inter-coder reliability. With
[`agreement()`](https://seraphinem.github.io/quallmer/reference/agreement.md),
users can launch an interactive app to manually code data, review AI
annotations, and evaluate intercoder reliability between coders and
agreement with LLM-generated scores.

**The `quallmer` package makes AI-assisted qualitative coding accessible
without requiring deep expertise in R, programming or machine
learning.**

# Included functions

The package provides the following core functions:

### `annotate()`

- A generic function that works with any LLM supported by
  [ellmer](https://ellmer.tidyverse.org/index.html).  
- Generates structured responses based on
  [predefined](https://seraphinem.github.io/quallmer/articles/pkgdown/examples/overview.htmll)
  or [user-defined
  tasks](https://seraphinem.github.io/quallmer/articles/pkgdown/tutorials/customtask.html).

### `task()`

- Creates custom annotation tasks tailored to specific research
  questions and data types.
- Uses `system_prompt` and various type specifications from the
  [ellmer](https://ellmer.tidyverse.org/reference/type_boolean.html)
  package to define how the LLM should interpret inputs and format
  outputs.  
- Tasks created with
  [`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
  can be passed directly to
  [`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md).  
- This allows users to tailor the annotation process to their specific
  data types and makes our package extensible for future use cases.

### `agreement_app()`

- Launches an interactive app to
  - Manually code data
  - Review and validate LLM-generated annotations
  - Compare human-coded data with LLM-generated annotations to evaluate
    agreement and inter-coder reliability with various metrics (e.g.,
    Krippendorff’s alpha, Cohen’s or Fleiss’ kappa).

### `agreement()`

- Calculates intercoder reliability scores (e.g., Krippendorff’s alpha
  and Cohen’s or Fleiss’ kappa) between multiple human coders or between
  human coders and LLM-generated annotations.
- Works similar to the Agreement App but can be used programmatically
  without launching the app.

# Supported LLMs

The package supports all LLMs currently available with the `ellmer`
package. For authentication and usage of each of these LLMs, please
refer to the respective
[ellmer](https://ellmer.tidyverse.org/reference/index.html)
documentation and see our [tutorial for setting up an openai API
key](https://seraphinem.github.io/quallmer/articles/pkgdown/tutorials/openai.html)
or [getting started with an open-source Ollama
model](https://seraphinem.github.io/quallmer/articles/pkgdown/tutorials/ollama.html).

## Installation

You can install the development version of `quallmer` from
<https://github.com/SeraphineM/quallmer> with:

``` r
# install.packages("pak")
pak::pak("SeraphineM/quallmer")
```

## Example use and tutorials

To learn more about how to use the package, please refer to our
[step-by-step
tutorials](https://seraphinem.github.io/quallmer/articles/getting-started.html)
and the illustrations on [how to use the predefined
tasks](https://seraphinem.github.io/quallmer/articles/pkgdown/examples/overview.html).
