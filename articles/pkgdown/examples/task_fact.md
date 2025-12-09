# Example: Fact checking of claims

The
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function with a predefined
[`task_fact()`](https://seraphinem.github.io/quallmer/reference/task_fact.md)
can be used to fact-check claims made in texts. In this example, we will
demonstrate how to apply this task to a sample corpus of innaugural
speeches from US presidents. The fact-checking process involves
evaluating the truthfulness of claims made in the speeches and providing
explanations for each claim. The outcome is a **truthfulness score from
0 to 10, where 0 indicates completely false claims and 10 indicates
highest confidence in the truthfulness of the claims.**

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
                   api_args = list(temperature = 0))
```

    ## Running task 'Fact-checking' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 2 -> 2 | ■■■■■■■■■■■■■■■■                  50%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

| id         | truth_score | misleading_topic                                                                                                                 | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|:-----------|------------:|:---------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2013-Obama |           9 |                                                                                                                                  | The text is a speech that reflects on American values, history, and aspirations. It accurately references historical events and foundational principles of the United States, such as the Declaration of Independence and the Constitution. The speech is largely aspirational and rhetorical, focusing on ideals rather than specific factual claims, which contributes to its high truthfulness score. There are no significant misleading topics or inaccuracies present.                                                                                                                                                                                                                                                                   |
| 2017-Trump |           6 | Transfer of power to the people, American carnage , Redistribution of wealth , Protectionism benefits , Eradication of terrorism | The speech contains several claims that are more rhetorical than factual. The idea of transferring power from Washington to the people is a common political theme but lacks specific mechanisms or evidence of actual power redistribution. The term ‘American carnage’ is a dramatic portrayal that may not accurately reflect the overall state of the nation. Claims about wealth being ‘ripped’ from the middle class and redistributed globally are oversimplified and lack nuance. The benefits of protectionism are debated among economists, and the promise to eradicate terrorism is an ambitious goal that is difficult to achieve fully. These elements contribute to a lower confidence in the overall truthfulness of the text. |
| 2021-Biden |           9 |                                                                                                                                  | The text is a speech that emphasizes themes of unity, democracy, and hope. It accurately reflects historical events and current challenges, such as the COVID-19 pandemic and political divisions. The speech is largely aspirational and rhetorical, with no significant factual inaccuracies or misleading claims. The overall truthfulness score is high, as the content aligns with known facts and historical context.                                                                                                                                                                                                                                                                                                                    |
| 2025-Trump |           3 | Historical inaccuracies, Policy claims , Election results , Panama Canal , Energy policies                                       | The text contains numerous factual inaccuracies and misleading claims. For instance, the assertion about the Panama Canal being ‘taken back’ is not feasible under current international law. The claim of winning the popular vote by millions lacks evidence, as does the assertion of a ‘mandate’ from the election. The statement about the U.S. having the largest oil and gas reserves is misleading without context. Additionally, the historical references and policy proposals, such as changing the name of the Gulf of Mexico, are not grounded in current political or legal realities. These issues significantly reduce the truthfulness score.                                                                                 |

### Using `annotate()` for fact checking with a specific number of claims to check

``` r
# Apply predefined fact checking task with task_fact() in the annotate() function
result_claims <- annotate(data_corpus_inaugural, task = task_fact(max_topics = 3), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0))
```

    ## Running task 'Fact-checking' using model: gpt-4o

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 1 -> 3 | ■■■■■■■■■■■■■■■■■■■■■■■           75%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

[TABLE]

In this example, we demonstrated how to use the
[`annotate()`](https://seraphinem.github.io/quallmer/reference/annotate.md)
function with the
[`task_fact()`](https://seraphinem.github.io/quallmer/reference/task_fact.md)
to fact-check claims in a corpus of innaugural speeches. The results
include a truth score, identified misleading topics, and explanations
for each claim evaluated. The amount of claims to check can be adjusted
using the `max_topics` parameter in the
[`task_fact()`](https://seraphinem.github.io/quallmer/reference/task_fact.md)
function. Now you can apply this approach to your own texts for
fact-checking purposes!
