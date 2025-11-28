# Example: Ideology detection

The
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function with a predefined
[`task_ideology()`](https://seraphinem.github.io/quallmer/reference/task_ideology.md)
allows you to perform ideological scaling (0-10) on texts regarding a
specified ideological dimension. In this example, we will demonstrate
how to use the
[`task_ideology()`](https://seraphinem.github.io/quallmer/reference/task_ideology.md)
for ideology detection on a sample corpus of innaugural speeches from
U.S. presidents. We will use the dimension “inclusive–exclusive” as an
example. To refine the task, we will also provide a definition of the
dimension (optional).

### Loading packages and data

``` r
# We will use the quanteda package 
# for loading a sample corpus of innaugural speeches
# If you have not yet installed the quanteda package, you can do so by:
# install.packages("quanteda")
library(quanteda)
```

    ## Package version: 4.3.1
    ## Unicode version: 15.1
    ## ICU version: 74.2

    ## Parallel computing: disabled

    ## See https://quanteda.io for tutorials and examples.

``` r
library(quallmer)
```

    ## Loading required package: ellmer

``` r
# For educational purposes, 
# we will use a subset of the inaugural speeches corpus
# The three most recent speeches in the corpus
data_corpus_inaugural <- quanteda::data_corpus_inaugural[57:60]
```

### Using `annotate()` for ideological scaling of texts

``` r
# Define ideological dimension
dimension <- "inclusive–exclusive"
# Provide definition for the dimension
definition <- "Inclusive language emphasizes equal rights, diversity, pluralism, 
and protection of minorities, whereas exclusive language emphasizes exclusion 
of groups, national homogeneity, and restricting rights."
# Apply predefined ideology task with task_ideology() in the annotate() function
result <- annotate(data_corpus_inaugural, task = task_ideology(dimension, definition), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0, seed = 42))
```

    ## Running task 'Ideological scaling' using model: gpt-4o

    ## Warning: 4 requests errored.

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:kableExtra':
    ## 
    ##     group_rows

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

| id         | score | explanation |
|:-----------|------:|:------------|
| 2013-Obama |    NA | NA          |
| 2017-Trump |    NA | NA          |
| 2021-Biden |    NA | NA          |
| 2025-Trump |    NA | NA          |

### Adjusting the ideology scaling task

You can customize the ideological scaling task by defining your own task
with [`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
(for a more detailed explanation, [see our “Defining custom tasks”
tutorial](https://seraphinem.github.io/quallmer/articles/pkgdown/tutorials/customtask.html)).
For example, you might like to change the scale from 0-10 to -5 to +5.

``` r
custom_ideology <- task(
    name = "Ideological scaling",
    system_prompt = paste0(
      "You are an expert political scientist performing ideological text scaling.",
      "Task:",
      "- Read each short text carefully.",
      "- Place the text on a -5 to +5 scale for the following ideological dimension: ",
      dimension, 
      definition
    ),
    type_def = ellmer::type_object(
      score       = ellmer::type_integer("Ideological position on the specified dimension (0–10, where -5 = first pole, +5 = second pole)"),
      explanation = ellmer::type_string("Brief justification for the assigned score, referring to specific elements in the text")
    ),
    input_type = "text"
  )
# Apply the custom task
custom_result <- annotate(data_corpus_inaugural, task = custom_ideology, 
                          chat_fn = chat_openai, model = "gpt-4o",
                          api_args = list(temperature = 0, seed = 42))
```

    ## Running task 'Ideological scaling' using model: gpt-4o

    ## Warning: 4 requests errored.

| id         | score | explanation |
|:-----------|------:|:------------|
| 2013-Obama |    NA | NA          |
| 2017-Trump |    NA | NA          |
| 2021-Biden |    NA | NA          |
| 2025-Trump |    NA | NA          |

In this example, we demonstrated how to use the
[`task_ideology()`](https://seraphinem.github.io/quallmer/reference/task_ideology.md)
for scaling texts regarding their ideological position on a specified
dimension. We also showed how to customize the task using the
[`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
function for more tailored annotation needs, e.g., changing the scale
from 0-10 to -5 to +5. Now you can apply these techniques to your own
text data for ideological analysis!
