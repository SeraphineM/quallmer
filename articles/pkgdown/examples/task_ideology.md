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
                   api_args = list(temperature = 0))
```

    ## Running task 'Ideological scaling' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

| id         | score | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:-----------|------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2013-Obama |     1 | The text emphasizes inclusivity, highlighting themes of equality, diversity, and collective action. It references historical struggles for civil rights and calls for equal treatment of women, LGBTQ+ individuals, and immigrants. Phrases like ‘all men are created equal,’ ‘diversity and openness,’ and ‘our journey is not complete until’ underscore a commitment to inclusivity and equal rights for all citizens.                                                                            |
| 2017-Trump |     7 | The text emphasizes national homogeneity and prioritizes American interests with phrases like “America first” and “protect our borders.” It focuses on American workers and industries, suggesting an exclusive stance. However, it also includes some inclusive elements, such as unity across racial lines and a call for solidarity, which prevents a higher exclusive score.                                                                                                                     |
| 2021-Biden |     0 | The text emphasizes unity, inclusion, and addressing systemic issues like racial justice and political extremism. It calls for coming together as a nation, respecting diversity, and healing divisions, which aligns with inclusive language.                                                                                                                                                                                                                                                       |
| 2025-Trump |     8 | The text emphasizes national sovereignty, exclusion of certain groups, and a focus on national homogeneity. Phrases like “put America first,” “halt illegal entry,” and “returning millions of criminal aliens” indicate an exclusive stance. The mention of “only two genders” and ending “socially engineered race and gender” policies further supports this position. While there are mentions of unity and diversity, the overall tone and specific policies suggest a more exclusive ideology. |

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
                          api_args = list(temperature = 0))
```

    ## Running task 'Ideological scaling' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

| id         | score | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|:-----------|------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2013-Obama |     5 | The text strongly emphasizes inclusivity, highlighting themes of equality, diversity, and collective action. It advocates for equal rights across gender, sexual orientation, and socioeconomic status, and stresses the importance of welcoming immigrants and supporting marginalized groups. The language consistently promotes unity and shared responsibility, aligning with inclusive values.                                                                        |
| 2017-Trump |    -2 | The text emphasizes national homogeneity and prioritizes American interests with phrases like “America first” and “protect our borders.” It highlights exclusionary themes by focusing on protecting American jobs and industries from foreign influence. While there are inclusive elements, such as unity and shared national identity, the overall tone leans towards exclusivity by emphasizing national over global or diverse interests.                             |
| 2021-Biden |     5 | The text emphasizes unity, inclusion, and the protection of democracy, highlighting the importance of racial justice, confronting white supremacy, and embracing diversity. It calls for healing, respect, and dignity for all, advocating for a more inclusive society.                                                                                                                                                                                                   |
| 2025-Trump |    -3 | The text emphasizes national sovereignty, border control, and a strong focus on American exceptionalism, which aligns with exclusive language. It includes plans to halt illegal entry and return ‘criminal aliens,’ and emphasizes a colorblind, merit-based society, which can be seen as minimizing diversity and inclusion efforts. The rhetoric around national unity and pride is framed in a way that prioritizes national homogeneity and strength over pluralism. |

In this example, we demonstrated how to use the
[`task_ideology()`](https://seraphinem.github.io/quallmer/reference/task_ideology.md)
for scaling texts regarding their ideological position on a specified
dimension. We also showed how to customize the task using the
[`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
function for more tailored annotation needs, e.g., changing the scale
from 0-10 to -5 to +5. Now you can apply these techniques to your own
text data for ideological analysis!
