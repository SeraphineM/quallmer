# Defining custom tasks

In this tutorial, we will explore how to create custom annotation tasks
using the `quallmer` package. Custom tasks allow you to tailor the LLM’s
output to your specific research questions and data types using the
[`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
function, providing greater flexibility and control over the annotation
process.

In the following example, we will demonstrate how to define a custom
task for scoring documents based on their alignment with political left
ideologies. For this, we formulate a prompt that asks the LLM to score
documents on a scale of political left alignment. We then define the
expected response structure using the
[`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
function. Finally, we will use the
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function to apply this custom task to a sample corpus of inaugural
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

### Defining a custom prompt

Defining prompts is a crucial step in creating custom tasks. The prompt
guides the LLM on how to interpret the input data and what kind of
output to generate. In this example, we will create a prompt that
instructs the LLM to score documents based on their alignment with
political left ideologies. Prompts can be much longer and more complex
depending on the task at hand. Prompts should be clear and specific to
ensure that the LLM understands the task requirements.

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

### Applying the custom task to the corpus

This step is similar to applying predefined tasks using the
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function. Here, we will use the
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function to apply our custom task to the sample corpus of inaugural
speeches. We will specify the LLM to use (in this case, openai’s gpt-4o
model) and any additional API arguments as needed. For example, we set
the temperature to 0 for more deterministic outputs, improving
consistency in scoring across multiple runs and therefore increasing
reliability.

``` r
# Apply the custom task to the inaugural speeches corpus
result <- annotate(data_corpus_inaugural, task = ideology_scores,
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0))
```

    ## Running task 'Score Political Left Alignment' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 1 -> 3 | ■■■■■■■■■■■■■■■■■■■■■■■           75%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

| id         | score | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|:-----------|------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2013-Obama |     2 | The document aligns very well with the political left, emphasizing social equality, government intervention, and progressive policies. It advocates for collective action, economic equality, climate change response, and social justice, all of which are key tenets of leftist ideology. However, it also acknowledges skepticism of central authority and the importance of personal responsibility, which slightly moderates its alignment.                                                   |
| 2017-Trump |     0 | The document emphasizes nationalism, protectionism, and a focus on American interests, which are not typically aligned with the political left. It lacks advocacy for social equality, government intervention in the economy, or progressive policies, which are key aspects of leftist ideology. The rhetoric is more populist and nationalist, focusing on transferring power to the people and prioritizing American interests.                                                                |
| 2021-Biden |     2 | The document aligns very well with the political left, emphasizing themes of social equality, racial justice, and government intervention in addressing economic challenges. It calls for unity, healing, and progressive change, such as rebuilding the middle class and securing healthcare for all. However, it also appeals to broader American values and unity, which tempers its alignment slightly.                                                                                        |
| 2025-Trump |     0 | The document primarily emphasizes nationalism, border security, military strength, and economic independence, which are typically associated with right-wing ideologies. It criticizes government intervention in areas like education and public health, opposes progressive policies like the Green New Deal, and promotes traditional values. These elements do not align with the political left’s focus on social equality, government intervention in the economy, and progressive policies. |

Now you have successfully created and applied a custom annotation task
using the `quallmer` package! You can further modify the prompt and
response structure to suit your specific research needs.
