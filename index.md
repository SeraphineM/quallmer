# quallmer

The **quallmer** package is an **easy-to-use toolbox for qualitative
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
ensure quality and reliability of AI-generated annotations, **quallmer**
offers tools for comparing LLM outputs with human-coded data and
assessing inter-coder reliability. With `validation_app()`, users can
launch an interactive app to manually code data, review AI annotations,
and evaluate agreement between human and AI codings. The package also
includes a `trail_` functionality that enables systematic comparisons
across multiple LLM runs (“trails”) with different settings to ensure
reproducibility and reliability of AI-assisted coding.

**The quallmer package makes AI-assisted qualitative coding accessible
without requiring deep expertise in R, programming or machine
learning.**

# Core functions

The package provides the following core functions:

### `annotate()`

- A generic function that works with any LLM supported by
  [ellmer](https://ellmer.tidyverse.org/index.html).  
- Generates structured responses based on
  [predefined](https://seraphinem.github.io/quallmer/articles/pkgdown/examples/overview.html)
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

### `validate_app()`

- Launches an interactive app to
  - Manually code data
  - Review and validate LLM-generated annotations
  - Compare human-coded data with LLM-generated annotations to evaluate
    inter-coder reliability (e.g., Krippendorff’s alpha, Cohen’s or
    Fleiss’ kappa) or, if a gold standard is available, accuracy (e.g.,
    accuracy, precision, recall, F1-score).

### `validate()`

- Works similar to our validation app but can be used programmatically
  without launching the app if data has been already manually coded by
  humans.
- Has two modes:
  1.  **No gold standard available**: Compare LLM-generated annotations
      with multiple human coders to assess inter-coder reliability
      (e.g., Krippendorff’s alpha, Cohen’s or Fleiss’ kappa).
  2.  **Gold standard available**: Compare LLM-generated annotations
      against a human-coded gold standard to assess accuracy (e.g.,
      accuracy, precision, recall, F1-score).

# The quallmer trail

Apart from the core functions above, the **quallmer** package also
provides a set of functions to ensure reproducibility and reliability of
LLM-generated annotations through **systematic comparisons across
multiple LLM runs with different settings.** This “trail” functionality
adds a reproducibility layer on top of
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
with the following workflow:

![](reference/figures/paw.png)

1.  **Define trail settings**  
    Describe the LLM trail, i.e., how LLMs should be called (e.g.,
    model, temperature).

    [`trail_settings()`](https://seraphinem.github.io/quallmer/reference/trail_settings.md)  
    ↓

2.  **Record single LLM trails for reproducibility**  
    Record all information needed for reproducing LLM runs on a given
    task with a specific setting.

    `trail_record(data, text_col, task, setting, id_col)`

    ↓

3.  **Run multiple trails with different settings and assess
    sensitivity**  
    Run the *same* task and data across multiple settings (e.g.,
    different LLMs, different temperatures) and compute agreement across
    trails to illustrate reliability and replication sensitivity of LLM
    annotations.

    `trail_compare(data, text_col, task, settings = list(...), id_col, lablel_col)`

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

You can install the development version of **quallmer** from
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
