# Example: Fact checking of claims

The
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function with a predefined
[`task_fact()`](https://seraphinem.github.io/quallmer/reference/task_fact.md)
can be used to fact-check claims made in texts. In this example, we will
demonstrate how to apply this task to a sample corpus of innaugural
speeches from US presidents.

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

### Using `annotate()` for fact checking of claims in texts

``` r
# Apply predefined fact checking task with task_fact() in the annotate() function
result <- annotate(data_corpus_inaugural, task = task_fact(), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0, seed = 42))
```

    ## Running task 'Overall truthfulness assessment' using model: gpt-4o

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

| id         | truth_score | misleading | explanation |
|:-----------|------------:|:-----------|:------------|
| 2013-Obama |          NA |            | NA          |
| 2017-Trump |          NA |            | NA          |
| 2021-Biden |          NA |            | NA          |
| 2025-Trump |          NA |            | NA          |

### Using `annotate()` for fact checking with a specific number of claims to check

``` r
# Apply predefined fact checking task with task_fact() in the annotate() function
result_claims <- annotate(data_corpus_inaugural, task = task_fact(max_topics = 3), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0, seed = 42))
```

    ## Running task 'Overall truthfulness assessment' using model: gpt-4o

    ## Warning: 4 requests errored.

| id         | truth_score | misleading | explanation |
|:-----------|------------:|:-----------|:------------|
| 2013-Obama |          NA |            | NA          |
| 2017-Trump |          NA |            | NA          |
| 2021-Biden |          NA |            | NA          |
| 2025-Trump |          NA |            | NA          |
