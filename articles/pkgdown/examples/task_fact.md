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
                   api_args = list(temperature = 0),
                   params = list(seed = 42))
```

    ## Running task 'Fact-checking' using model: gpt-4o

    ## Warning: Ignoring unsupported parameters: "seed"
    ## Ignoring unsupported parameters: "seed"
    ## Ignoring unsupported parameters: "seed"
    ## Ignoring unsupported parameters: "seed"

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

    ## [working] (0 + 0) -> 0 -> 4 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

| id         | truth_score | misleading_topic                                                                                                             | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:-----------|------------:|:-----------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 2013-Obama |           9 |                                                                                                                              | The text is a ceremonial speech, likely an inaugural address, that emphasizes American values, historical references, and aspirations. It contains general statements about democracy, equality, and national goals, which are largely accurate and consistent with historical and cultural narratives. The speech does not make specific factual claims that are easily verifiable or refutable, thus maintaining a high level of truthfulness. The themes are broad and aspirational, reducing the likelihood of misleading content.                                                                                                                                      |
| 2017-Trump |           5 | Transfer of Power to the People, American Carnage , America First Policy , Protectionism Benefits , Eradication of Terrorism | The speech contains several broad and ambitious claims that are difficult to substantiate or measure. The idea of transferring power from Washington to the people is a common political rhetoric but lacks specific mechanisms or evidence of implementation. The depiction of ‘American carnage’ is a dramatic portrayal that may not accurately reflect the overall state of the nation. The ‘America First’ policy and protectionism are complex issues with mixed economic outcomes, not universally beneficial as suggested. The claim to eradicate terrorism is overly ambitious and not easily achievable. These elements contribute to a lower truthfulness score. |
| 2021-Biden |           9 |                                                                                                                              | The text is a speech that emphasizes themes of unity, democracy, and hope. It accurately reflects historical events and current challenges, such as the COVID-19 pandemic and political divisions. The speech is largely aspirational and rhetorical, with no specific factual inaccuracies or misleading claims. The overall message aligns with known facts and historical context, resulting in a high truthfulness score.                                                                                                                                                                                                                                               |
| 2025-Trump |           3 | Historical Inaccuracies , Policy Claims , Election Results , Panama Canal Ownership , Energy and Environmental Policies      | The text contains numerous factual inaccuracies and misleading claims. For instance, the mention of an assassination attempt lacks verification, and the claim about the Panama Canal being operated by China is false. The speech also includes exaggerated or unverified claims about election results and policy impacts. These issues significantly reduce the overall truthfulness of the text.                                                                                                                                                                                                                                                                        |

### Using `annotate()` for fact checking with a specific number of claims to check

``` r
# Apply predefined fact checking task with task_fact() in the annotate() function
result_claims <- annotate(data_corpus_inaugural, task = task_fact(max_topics = 3), 
                   chat_fn = chat_openai, model = "gpt-4o",
                   api_args = list(temperature = 0),
                   params = list(seed = 42))
```

    ## Running task 'Fact-checking' using model: gpt-4o

    ## Warning: Ignoring unsupported parameters: "seed"
    ## Ignoring unsupported parameters: "seed"
    ## Ignoring unsupported parameters: "seed"
    ## Ignoring unsupported parameters: "seed"

    ## [working] (0 + 0) -> 3 -> 1 | ■■■■■■■■■                         25%

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
