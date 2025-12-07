# Example: Salience of topics

The
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function with a predefined
[`task_salience()`](https://seraphinem.github.io/quallmer/reference/task_salience.md)
can be used to identify and rank the salience of topics discussed in
texts. In this example, we will demonstrate how to apply this task to a
sample corpus of innaugural speeches from US presidents.

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

### Using `annotate()` for salience of ANY topics discussed in texts

``` r
# Apply predefined salience task with task_salience() in the annotate() function
result <- annotate(data_corpus_inaugural, task = task_salience(), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0))
```

    ## Running task 'Salience (ranked topics)' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

[TABLE]

### Using `annotate()` for salience of a SPECIFIED LIST of topics discussed in texts

``` r
# Define a list of topics to focus on
topics <- c("economy", "health", "education", "environment", "foreign policy")
# Apply predefined salience task with task_salience() in the annotate() function
result <- annotate(data_corpus_inaugural, task = task_salience(topics), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0))
```

    ## Running task 'Salience (ranked topics)' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

[TABLE]

### Adjusting the task_salience() so it also returns the stance for each topic

``` r
# Customizing the task to include the stance for each topic
custom_task <- task(
  name = "Salience and stance of topics",
  system_prompt = paste(
    "You are an expert analysing the content of texts.",
    "",
    "Task:",
    "- Read the text carefully.",
    "- Identify and rank the salience of the following topics: economy, health, education, environment, foreign policy.",
    "- For each topic mentioned, assign a stance as one of the following:",
    "  pro, neutral, or contra.",
    "- Append the stance directly after each topic name in the form 'topic: stance'.",
    "- Return all topic:stance entries in descending order of salience.",
    "- Separate entries with commas when presenting them in a list.",
    "",
    "Do not infer information that is not in the text.",
    "Base all evaluations solely on the language and arguments in the document.",
    "",
    "Output:",
    "- `topic_stance`: a ranked list of topic labels with stance labels appended (e.g., 'economy: pro', 'health: contra').",
    "- `explanation`: a brief justification explaining why the topics were ordered and how stance was determined.",
    sep = "\n"
  ),
  type_def = ellmer::type_object(
    topic_stance = ellmer::type_array(
      ellmer::type_string("Topic and stance label combined (e.g., 'economy: pro'), ranked by salience.")
    ),
    explanation = ellmer::type_string(
      "Brief justification for the salience ordering and stance classification."
    )
  ),
  input_type = "text"
)

# Apply the customized task in the annotate() function
custom_result <- annotate(data_corpus_inaugural, task = custom_task, 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0))
```

    ## Running task 'Salience and stance of topics' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

| id         | topic_stance                                                                                       | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:-----------|:---------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2013-Obama | economy: pro , environment: pro , foreign policy: pro, health: pro , education: pro                | The text emphasizes the importance of a strong economy, highlighting the need for infrastructure, fair competition, and a rising middle class, indicating a pro stance on the economy. The environment is addressed with a commitment to tackling climate change and leading in sustainable energy, showing a pro stance. Foreign policy is discussed in terms of maintaining alliances and promoting peace, indicating a pro stance. Health is mentioned in the context of reducing healthcare costs and supporting social safety nets, showing a pro stance. Education is highlighted as essential for future success, with a focus on reforming schools and training teachers, indicating a pro stance.                     |
| 2017-Trump | economy: pro , foreign policy: neutral, education: contra , environment: neutral , health: neutral | The text emphasizes economic issues, focusing on job creation, rebuilding infrastructure, and prioritizing American workers, indicating a pro stance on the economy. Foreign policy is addressed with a focus on ‘America first’ and forming alliances, suggesting a neutral stance as it balances protectionism with international cooperation. Education is mentioned negatively, highlighting a failing system, which suggests a contra stance. The environment is not directly addressed, but infrastructure plans imply a neutral stance. Health is briefly mentioned in terms of eradicating disease, suggesting a neutral stance.                                                                                       |
| 2021-Biden | democracy: pro , health: pro , unity: pro , foreign policy: pro, environment: pro , economy: pro   | The speech primarily focuses on the theme of democracy, emphasizing its triumph and the need to protect it, making ‘democracy: pro’ the most salient topic. Health is addressed through the discussion of the pandemic and the need to overcome it, leading to ‘health: pro’. Unity is a central theme, repeatedly mentioned as essential for progress, resulting in ‘unity: pro’. Foreign policy is touched upon with a commitment to re-engage with the world, hence ‘foreign policy: pro’. The environment is mentioned in the context of a ‘cry for survival,’ leading to ‘environment: pro’. Economic issues are addressed through references to job losses and rebuilding the middle class, resulting in ‘economy: pro’. |
| 2025-Trump | foreign policy: pro, economy: pro , environment: contra, health: contra , education: contra        | The text emphasizes foreign policy with a strong stance on national sovereignty, border security, and international respect, making it the most salient topic. The economy is also prominent, with a focus on reducing inflation, energy independence, and manufacturing, all framed positively. The environment is addressed negatively, with opposition to the Green New Deal and electric vehicle mandates. Health is mentioned in a critical context regarding the public health system’s inefficiency. Education is criticized for promoting negative views of the country, indicating a contra stance.                                                                                                                   |

In this example, we demonstrated how to use the
[`task_salience()`](https://seraphinem.github.io/quallmer/reference/task_salience.md)
for identifying and ranking topics discussed in texts, both with and
without a predefined list of topics. Additionally, we showed how to
customize the task to include stance classification for each topic. This
showcases the flexibility of the
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function and the `task` framework in `quallmer` for various text
analysis tasks.
