# Predefined task for overall truthfulness assessment

Assigns an overall truthfulness score to a text and lists topics that
reduce confidence in its accuracy.

## Usage

``` r
task_fact(max_topics = 5)
```

## Arguments

- max_topics:

  Integer: maximum number of topics or issues to list as reducing
  confidence in the truthfulness of the text. Default is 5.

## Value

A task object
