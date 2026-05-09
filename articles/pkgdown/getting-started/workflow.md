# The quallmer workflow

This tutorial provides a quick overview of the quallmer workflow for
LLM-assisted qualitative coding. We’ll use US presidential inaugural
addresses to demonstrate the core functions.

## The workflow at a glance

1.  **Define** your codebook with
    [`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md)
2.  **Code** your data with
    [`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
3.  **Replicate** with different settings using
    [`qlm_replicate()`](https://quallmer.github.io/quallmer/reference/qlm_replicate.md)
4.  **Compare** results with
    [`qlm_compare()`](https://quallmer.github.io/quallmer/reference/qlm_compare.md)
5.  **Document** everything with
    [`qlm_trail()`](https://quallmer.github.io/quallmer/reference/qlm_trail.md)

## Sample data

We’ll use the inaugural addresses from the quanteda package:

``` r
library(quallmer)
library(quanteda)

# Get last 5 inaugural addresses
texts <- tail(data_corpus_inaugural, 5)
texts
```

## Step 1: Define a codebook

A codebook specifies what the LLM should extract from your texts:

``` r
my_codebook <- qlm_codebook(
  name = "Tone analysis",
  instructions = "Classify the overall tone of this political speech.",
  schema = type_object(
    tone = type_enum(
      values = c("optimistic", "cautious", "urgent"),
      description = "The dominant emotional tone"
    ),
    confidence = type_integer("Confidence in classification from 1-5")
  )
)

my_codebook
```

> **Tip:** Need to split texts into smaller units first? Use
> [`qlm_segment()`](https://quallmer.github.io/quallmer/reference/qlm_segment.md)
> to segment texts into thematic or conceptual units (e.g.,
> quasi-sentences, aspects, or topics) before coding. For an example,
> see here: [Text
> segmentation](https://quallmer.github.io/quallmer/articles/pkgdown/examples/example_segmentation.html).

## Step 2: Code your data

Apply the codebook to your texts using an LLM:

``` r
coded <- qlm_code(
  texts,
  my_codebook,
  model = "openai/gpt-4o-mini",
  name = "gpt4o_mini"
)

coded
```

## Step 3: Replicate with a different model

Test reliability by coding again with a different model:

``` r
coded2 <- qlm_replicate(
  coded,
  model = "openai/gpt-4o",
  name = "gpt4o"
)

coded2
```

## Step 4: Compare results

Assess inter-rater reliability between the two coding runs:

``` r
comparison <- qlm_compare(coded, coded2)
comparison
```

If you have gold standard human coding, you can also validate against it
with
[`qlm_validate()`](https://quallmer.github.io/quallmer/reference/qlm_validate.md).

## Step 5: Create an audit trail

Document your complete workflow, including models, parameters, and
results and a Quarto report with replication instructions:

``` r
# View the trail
trail <- qlm_trail(coded, coded2, comparison)
trail

# Save trail and generate report
qlm_trail(coded, coded2, comparison, path = "my_analysis")
# Creates: my_analysis.rds, my_analysis.qmd
```

This workflow provides a structured approach to leveraging LLMs for
qualitative coding with transparency as well as full traceability and
the ability to replicate and validate your analyses.
