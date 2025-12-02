# The quallmer trail

In this tutorial, we will walk through the `quallmer` trail
functionality step-by-step. The trail functions allow users to
systematically compare LLM-generated annotations across multiple runs
with different settings, ensuring reproducibility and reliability of the
results.

## Loading packages and data and defining a task

We will start by loading the necessary packages and a sample dataset. We
will then define a custom task that we want the LLMs to perform.

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
# turn corpus into data frame
data_corpus_inaugural <- quanteda::convert(data_corpus_inaugural, to = "data.frame")
```

### Defining a custom prompt

This is very similar to defining a custom prompt for the
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function. Here, we define a prompt that instructs the LLM to score
documents based on their alignment with the political left.

``` r
prompt <- "Score the following document on a scale of how much it aligns
with the political left. The political left is defined as groups which
advocate for social equality, government intervention in the economy,
and progressive policies. Use the following metrics:
SCORING METRIC:
3 : extremely left
2 : very left
1 : slightly left
0 : not at all left"
```

### Defining the structure of the response with define_task()

The [`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
function allows us to specify the expected structure of the LLM’s
response. It has the following important arguments which users need to
specify:

- `name`: A descriptive name for the task.
- `system_prompt`: The prompt that guides the LLM on how to perform the
  task.
- `type_def`: Defines the expected structure of the response using
  [ellmers type
  specifications](https://ellmer.tidyverse.org/reference/type_boolean.html)
  such as
  [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
  [`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
  etc.

For more information on how to use ellmer’s type specifications, please
refer to the [ellmer documentation on type
specifications](https://ellmer.tidyverse.org/reference/type_boolean.html).

``` r
# Define the custom task using task()
ideology_scores <- task(
  name = "Score Political Left Alignment",
  system_prompt = prompt,
  type_def = type_object(
    score = type_number("Score"),
    explanation = type_string("Explanation")
  ),
  input_type = "text"
)
```

## Define different trail settings

After having defined our task, we can now set up different trail
settings to compare how different LLM configurations affect the results.
For this, we can use the
[`trail_setting()`](https://seraphinem.github.io/quallmer/reference/trail_setting.md)
function. In this example, we will create four settings with different
models and different temperature values to see how they affect the LLM’s
responses.

``` r
setting_gptmini0 <- trail_setting(
provider = "openai",
model = "gpt-4.1-mini",
temperature = 0
)

setting_gptmini7 <- trail_setting(
provider = "openai",
model = "gpt-4.1-mini",
temperature = 0.7
)

setting_gpt400 <- trail_setting(
provider = "openai",
model = "gpt-4o",
temperature = 0
)

setting_gpt407 <- trail_setting(
provider = "openai",
model = "gpt-4o",
temperature = 0.7
)
```

## Run a single trail with a specific setting

We can use the
[`trail_record()`](https://seraphinem.github.io/quallmer/reference/trail_record.md)
function to run a single trail with a specific setting. This is useful
for ensuring reproducibility of results with a given configuration. The
example below shows how to run a trail using the `setting_T0` defined
earlier. The result of this function is a data frame containing the
LLM-generated annotations for each document in the dataset as well as
the associated metadata such as the setting used, the task, the data,
timestamp, etc. You can save this output for future reference to ensure
that you can reproduce the results later. You can also share this output
with others to allow them to verify your findings.

``` r
rec_T0 <- trail_record(
data = data_corpus_inaugural,
text_col = "text",
task = ideology_scores,
setting = setting_gptmini0,
id_col = "doc_id"
)
```

    ## Running task 'Score Political Left Alignment' using model: gpt-4.1-mini

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

``` r
# Display the recorded trail's settings
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
meta_df <- tibble(
  field = names(rec_T0$meta),
  value = vapply(rec_T0$meta, function(x) {
    if (is.list(x)) {
      # pretty-print lists
      paste(capture.output(str(x, max.level = 1)), collapse = "<br>")
    } else if (length(x) > 1) {
      paste(x, collapse = ", ")
    } else {
      as.character(x)
    }
  }, FUN.VALUE = character(1))
)

meta_df %>%
  kable("html", escape = FALSE, col.names = c("Field", "Value")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE)
```

| Field        | Value                                                  |
|:-------------|:-------------------------------------------------------|
| timestamp    | 2025-12-02 00:42:03.678869                             |
| n_rows       | 4                                                      |
| provider     | openai                                                 |
| model        | gpt-4.1-mini                                           |
| temperature  | 0                                                      |
| cache_dir    | trail_cache                                            |
| cache_path   | trail_cache/trail_5df823d002ab66df499c11f5e037a179.rds |
| id_col       | doc_id                                                 |
| text_col     | text                                                   |
| task_class   | task                                                   |
| quallmer_ver | 0.1.0                                                  |
| ellmer_ver   | 0.4.0                                                  |
| R_ver        | 4.5.2                                                  |

## Run multiple trails with different settings

This step involves running the same task and data across multiple
settings using the
[`trail_compare()`](https://seraphinem.github.io/quallmer/reference/trail_compare.md)
function. This allows us to see how different configurations impact the
LLM’s outputs.

``` r
left_trails <- trail_compare(
data = data_corpus_inaugural,
text_col = "text",
task = ideology_scores,
settings = list(
  T0 = setting_gptmini0,
  T07 = setting_gptmini7,
  T40 = setting_gpt400,
  T407 = setting_gpt407
),
id_col = "doc_id"
)
```

    ## Running task 'Score Political Left Alignment' using model: gpt-4.1-mini

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

    ## Running task 'Score Political Left Alignment' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

    ## Running task 'Score Political Left Alignment' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

## Build the output-by-trail matrix

This step combines the outputs from the different trails into a matrix
format, where each row represents a document and each column represents
the output from a specific trail.

``` r
trail_mat <- trail_matrix(
x = left_trails,
id_col = "doc_id",
label_col = "score"
)

# Display the trail matrix 
trail_mat
```

    ## # A tibble: 4 × 5
    ##   doc_id        T0   T07   T40  T407
    ##   <chr>      <dbl> <dbl> <dbl> <dbl>
    ## 1 2013-Obama     2     2     2     2
    ## 2 2017-Trump     0     0     0     0
    ## 3 2021-Biden     1     1     2     2
    ## 4 2025-Trump     0     0     0     0

## Compute agreement across trails

This step assesses the stability and reliability of the LLM-generated
annotations across the different trails using intercoder reliability
metrics.

``` r
trail_icr <- trail_agreement(
x = left_trails,
id_col = "doc_id",
label_col = "score"
)

trail_icr
```

    ##                            metric  value
    ## 1                  units_included 4.0000
    ## 2                          coders 4.0000
    ## 3                      categories 3.0000
    ## 4         percent_unanimous_units 0.7500
    ## 5 mean_pairwise_percent_agreement 0.8333
    ## 6      mean_pairwise_cohens_kappa 0.7333
    ## 7             kripp_alpha_nominal 0.7251
    ## 8                    fleiss_kappa 0.7193

This output shows the intercoder reliability metrics across the
different trails, indicating how consistent the LLM-generated
annotations are across various settings. Higher values suggest greater
reliability and stability of the annotations.

Overall, the trail functionality in the `quallmer` package provides a
systematic way to assess the reproducibility and reliability of
LLM-generated annotations by comparing results across multiple runs with
different configurations. This is particularly useful for researchers
who want to ensure that their findings are robust and not overly
dependent on specific LLM settings. It also helps with the decision of
which model and settings to use for a given annotation task.
