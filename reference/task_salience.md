# Predefined task for salience of topics discussed (ranked topics)

Ranked list of topics mentioned in a text, ordered by salience.

## Usage

``` r
task_salience(topics = NULL, max_topics = 5)
```

## Arguments

- topics:

  Optional character vector of predefined topic labels (e.g.,
  c("economy", "health", "education", "environment")). If supplied, the
  model should only classify and rank among these topics. If NULL, the
  model may infer topic labels directly from the text.

- max_topics:

  Integer: maximum number of topics to return when topics are inferred
  from the text. Default is 5.

## Value

A task object
