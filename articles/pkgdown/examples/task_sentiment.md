# Example: Sentiment analysis

The
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function with a predefined `sentiment()` task allows you to rate the
sentiment of texts using an LLM. The predefined `sentiment()` object
structures the response with a numeric sentiment score from -1 (very
negative) to 1 (very positive) and a brief explanation.

### Loading packages and data

``` r
library(quallmer)
```

    ## Loading required package: ellmer

``` r
#Example texts
texts <- c(
"This is wonderful!",
"I really dislike this approach.",
"The results are somewhat disappointing.",
"Absolutely fantastic work!"
)
```

### Using `annotate()` for predefined sentiment analysis of texts

``` r
# Apply predefined sentiment task with task_sentiment() in the annotate() function
result <- annotate(texts, task = task_sentiment(), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0))
```

    ## Running task 'Sentiment analysis' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

|  id | score | explanation                                                                                                             |
|----:|------:|:------------------------------------------------------------------------------------------------------------------------|
|   1 |   0.9 | The sentiment is very positive, as indicated by the enthusiastic expression ‘wonderful.’                                |
|   2 |  -0.8 | The word ‘dislike’ indicates a strong negative sentiment towards the approach.                                          |
|   3 |  -0.5 | The word ‘disappointing’ indicates a negative sentiment, though ‘somewhat’ softens it slightly.                         |
|   4 |   0.9 | The phrase is highly positive, using words like ‘absolutely’ and ‘fantastic’ to express strong approval and admiration. |

### Adjusting the sentiment task

You can customize the sentiment analysis task by defining your own task
with [`task()`](https://seraphinem.github.io/quallmer/reference/task.md)
(for a more detailed explanation, [see our “Defining custom tasks”
tutorial](https://seraphinem.github.io/quallmer/articles/pkgdown/tutorials/customtask.html)).

For example, you might want to include an additional field for
confidence level.

``` r
custom_sentiment <- task(
  name = "Custom sentiment analysis",
  system_prompt = "You are an expert annotator. Rate the sentiment of each text from -1 (very negative) to 1 (very positive), briefly explain why, and provide a confidence level from 0 to 1.",
  type_def = ellmer::type_object(
    score = ellmer::type_number("Sentiment score between -1 (very negative) and 1 (very positive)"),
    explanation = ellmer::type_string("Brief explanation of the rating"),
    confidence = ellmer::type_number("Confidence level from 0 to 1")
  ),
  input_type = "text"
)
# Apply the custom sentiment task
custom_result <- annotate(texts, task = custom_sentiment, 
                          chat_fn = chat_openai, model = "gpt-4o",
                          api_args = list(temperature = 0))
```

    ## Running task 'Custom sentiment analysis' using model: gpt-4o

|  id | score | explanation                                                                                            | confidence |
|----:|------:|:-------------------------------------------------------------------------------------------------------|-----------:|
|   1 |   0.9 | The word ‘wonderful’ conveys a very positive sentiment.                                                |       0.95 |
|   2 |  -0.7 | The word ‘dislike’ indicates a negative sentiment towards the approach.                                |       0.90 |
|   3 |  -0.5 | The word ‘disappointing’ indicates a negative sentiment, though ‘somewhat’ softens it slightly.        |       0.90 |
|   4 |   0.9 | The phrase “Absolutely fantastic work!” is highly positive, expressing strong approval and admiration. |       0.95 |

Or, you might want to change the scoring scale to a 5-point Likert
scale.

``` r
likert_sentiment <- task(
  name = "Likert scale sentiment analysis",
  system_prompt = "You are an expert annotator. Rate the sentiment of each text on a scale from 1 (very negative) to 5 (very positive) and briefly explain why.",
  type_def = ellmer::type_object(
    score = ellmer::type_number("Sentiment score between 1 (very negative) and 5 (very positive)"),
    explanation = ellmer::type_string("Brief explanation of the rating")
  ),
  input_type = "text"
)
# Apply the Likert scale sentiment task
likert_result <- annotate(texts, task = likert_sentiment, 
                          chat_fn = chat_openai, model = "gpt-4o",
                          api_args = list(temperature = 0))
```

    ## Running task 'Likert scale sentiment analysis' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

|  id | score | explanation                                                                                                      |
|----:|------:|:-----------------------------------------------------------------------------------------------------------------|
|   1 |     5 | The word ‘wonderful’ conveys a very positive sentiment.                                                          |
|   2 |     2 | The sentiment is negative due to the use of the word ‘dislike,’ indicating dissatisfaction or disapproval.       |
|   3 |     2 | The word ‘disappointing’ indicates a negative sentiment, though ‘somewhat’ suggests it’s not extremely negative. |
|   4 |     5 | The phrase “Absolutely fantastic work!” is highly positive, expressing strong approval and admiration.           |

In this way, you can easily adapt the sentiment analysis task to fit
your specific research needs!
