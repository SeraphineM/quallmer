# Creating codebooks

In this tutorial, we will learn how to create custom codebooks for
qualitative coding tasks using the `quallmer` package. Codebooks are
essential for guiding large language models (LLMs) in generating
structured and meaningful outputs based on your research questions.

We’ll demonstrate two approaches:

1.  **Learning from built-in examples**: Inspect the built-in example
    `data_codebook_sentiment` to understand codebook structure

2.  **Creating custom codebooks**: Build a domain-specific codebook for
    political ideology scoring

The built-in codebooks serve as educational templates showing basic
principles of instruction writing, schema design, and role definition.
For actual research, you can create more detailed custom codebooks using
[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md)
that match your specific needs.

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
# The ten most recent speeches in the corpus
data_corpus_inaugural <- quanteda::data_corpus_inaugural[50:60]
```

### Learning from built-in codebooks examples

Before creating a custom codebook, let’s inspect the built-in
`data_codebook_sentiment` to understand the structure:

``` r
# View the codebook
data_codebook_sentiment
```

    ## quallmer codebook: Sentiment analysis 
    ##   Input type:   text
    ##   Role:         You are a political communication analyst evaluating public ...
    ##   Instructions: Analyze the sentiment of this text, on both a 1-10 scale and...
    ##   Output schema:ellmer::TypeObject

``` r
# Inspect the role
data_codebook_sentiment$role
```

    ## [1] "You are a political communication analyst evaluating public statements."

``` r
# Inspect the instructions
data_codebook_sentiment$instructions
```

    ## [1] "Analyze the sentiment of this text, on both a 1-10 scale and as a polarity of negative or positive."

This shows us:

- The input type is text
- The role defines the model as a political communication analyst
- The instructions guide the model to analyze sentiment on a 1-10 scale
  and classify polarity
- The schema specifies the expected output structure with fields for
  polarity and rating

Now let’s create a custom codebook for our specific research question.

### Defining custom instructions

Defining instructions is a crucial step in creating custom codebooks.
The instructions guide the LLM on how to interpret the input data and
what kind of output to generate. In this example, we will create
instructions that tell the LLM to score documents based on their
alignment with political left ideologies. Instructions can be much
longer and more complex depending on the task at hand. Instructions
should be clear and specific to ensure that the LLM understands the task
requirements.

``` r
instructions <- "Score the following document on a scale of how much it aligns
with the political left. The political left is defined as groups which
advocate for social equality, government intervention in the economy,
and progressive policies. Use the following metrics:
SCORING METRIC:
3 : extremely left
2 : very left
1 : slightly left
0 : not at all left"
```

### Defining the codebook with qlm_codebook()

The
[`qlm_codebook()`](https://quallmer.github.io/quallmer/reference/qlm_codebook.md)
function allows us to specify the expected structure of the LLM’s
response. It has the following important arguments which users need to
specify:

- `name`: A descriptive name for the codebook.
- `instructions`: The instructions that guide the LLM on how to perform
  the coding task.
- `schema`: Defines the expected structure of the response using
  [ellmer’s type
  specifications](https://ellmer.tidyverse.org/reference/type_boolean.html)
  such as
  [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
  [`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
  etc.
- `role`: (Optional) A role description for the model (e.g., “You are an
  expert political scientist”).
- `input_type`: The type of input data (`"text"` or `"image"`).

For more information on how to use ellmer’s type specifications, please
refer to the [ellmer documentation on type
specifications](https://ellmer.tidyverse.org/reference/type_boolean.html).

``` r
# Define the custom codebook using qlm_codebook()
ideology_codebook <- qlm_codebook(
  name = "Score Political Left Alignment",
  instructions = instructions,
  schema = type_object(
    score = type_number("Score"),
    explanation = type_string("Explanation")
  ),
  role = "You are an expert political scientist analyzing political texts.",
  input_type = "text"
)
```

### Applying the custom codebook to the corpus

We use the
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
function to apply our custom codebook to the sample corpus of inaugural
speeches. We specify the model to use via `model` (in this case,
`"openai/gpt-4o"`) and any additional parameters as needed. For example,
we set the temperature to 0 for more deterministic outputs, improving
consistency in scoring across multiple runs and therefore increasing
reliability.

The
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)
function returns a `qlm_coded` object, which is a tibble containing the
coded results along with metadata stored as attributes. The object
prints as a tibble and can be used directly in data manipulation
workflows.

``` r
# Apply the custom codebook to the inaugural speeches corpus
coded <- qlm_code(data_corpus_inaugural,
                  codebook = ideology_codebook,
                  model = "openai/gpt-4o",
                  params = params(temperature = 0))
```

    ## [working] (0 + 0) -> 10 -> 1 | ■■■■                               9%

    ## [working] (0 + 0) -> 0 -> 11 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%

``` r
# View the results
coded
```

    ## # quallmer coded object
    ## # Run:      original
    ## # Codebook: Score Political Left Alignment
    ## # Model:    openai/gpt-4o
    ## # Units:    11
    ## 
    ## # A tibble: 11 × 3
    ##    .id          score explanation                                               
    ##  * <chr>        <dbl> <chr>                                                     
    ##  1 1985-Reagan      0 "The document emphasizes reducing government intervention…
    ##  2 1989-Bush        0 "The document emphasizes themes such as free markets, lim…
    ##  3 1993-Clinton     1 "The document contains elements that align slightly with …
    ##  4 1997-Clinton     1 "The document contains elements that align slightly with …
    ##  5 2001-Bush        1 "The document contains elements that align slightly with …
    ##  6 2005-Bush        0 "The document emphasizes themes of freedom, democracy, an…
    ##  7 2009-Obama       2 "The speech aligns very well with the political left due …
    ##  8 2013-Obama       2 "The document aligns very well with the political left, e…
    ##  9 2017-Trump       0 "The document emphasizes nationalism, protectionism, and …
    ## 10 2021-Biden       2 "The document aligns very well with the political left, a…
    ## 11 2025-Trump       0 "The document emphasizes nationalism, border security, mi…

| .id          | score | explanation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:-------------|------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1985-Reagan  |     0 | The document emphasizes reducing government intervention, lowering taxes, and promoting free enterprise, which align with conservative principles rather than the political left. It advocates for limiting the size and power of the federal government, which contrasts with the left’s preference for government intervention in the economy and social equality. The focus on individual freedom, reducing dependency, and a strong national defense further aligns with right-leaning ideologies.                                                      |
| 1989-Bush    |     0 | The document emphasizes themes such as free markets, limited government intervention, and individual responsibility, which align more with conservative or right-leaning ideologies. It critiques reliance on public money to solve social issues and promotes private initiative and community involvement. These elements do not align with the political left’s focus on government intervention and social equality.                                                                                                                                    |
| 1993-Clinton |     1 | The document contains elements that align slightly with the political left, such as advocating for investment in people, addressing economic inequality, and reforming politics to reduce the influence of power and privilege. However, it also emphasizes personal responsibility, competition, and a strong national defense, which are not typically left-leaning priorities. The overall tone is more centrist, with a focus on unity and renewal rather than explicitly progressive policies.                                                         |
| 1997-Clinton |     1 | The document contains elements that align slightly with the political left, such as advocating for civil rights, social equality, and educational opportunities for all. However, it also emphasizes personal responsibility, a smaller government, and free enterprise, which are not typically associated with the political left. The balance between these elements suggests a slightly left alignment.                                                                                                                                                 |
| 2001-Bush    |     1 | The document contains elements that align slightly with the political left, such as advocating for social justice, addressing poverty, and reforming social programs like Social Security and Medicare. However, it also emphasizes personal responsibility, private character, and reducing taxes, which are more centrist or right-leaning positions. The focus on national defense and a strong military presence further distances it from a strong left alignment. Overall, the speech reflects a blend of values, resulting in a slightly left score. |
| 2005-Bush    |     0 | The document emphasizes themes of freedom, democracy, and individual responsibility, which are more aligned with conservative and neoconservative ideologies. It focuses on spreading democracy globally, national security, and individual economic independence, rather than advocating for social equality or government intervention in the economy, which are key aspects of the political left.                                                                                                                                                       |
| 2009-Obama   |     2 | The speech aligns very well with the political left due to its emphasis on social equality, government intervention, and progressive policies. It advocates for addressing economic inequality, reforming healthcare, investing in infrastructure, and transitioning to renewable energy. The call for international cooperation and addressing climate change also reflects left-leaning values. However, it balances these with appeals to traditional American values and bipartisan unity, which slightly moderates its alignment.                      |
| 2013-Obama   |     2 | The document aligns very well with the political left, emphasizing social equality, government intervention, and progressive policies. It advocates for collective action, economic equality, environmental responsibility, and social justice, all of which are key tenets of leftist ideology. However, it also acknowledges the importance of personal responsibility and skepticism of central authority, which tempers its alignment slightly.                                                                                                         |
| 2017-Trump   |     0 | The document emphasizes nationalism, protectionism, and a focus on “America first” policies, which are not typically aligned with the political left. It lacks advocacy for social equality, government intervention in the economy, or progressive policies, which are key characteristics of the political left. Instead, it focuses on reducing government power, returning power to the people, and prioritizing national interests, aligning more with right-wing populism.                                                                            |
| 2021-Biden   |     2 | The document aligns very well with the political left, as it emphasizes themes of social equality, racial justice, and government intervention in addressing economic challenges. It calls for unity to tackle systemic racism, climate change, and economic inequality, which are key concerns of the left. However, it also stresses unity and bipartisanship, which slightly moderates its alignment.                                                                                                                                                    |
| 2025-Trump   |     0 | The document emphasizes nationalism, border security, military strength, and economic independence, which are typically associated with right-wing ideologies. It criticizes government intervention in areas like education and public health, opposes progressive policies like the Green New Deal, and promotes traditional values. These elements align more with conservative or right-wing positions rather than the political left.                                                                                                                  |

## Summary

Now you have successfully learned how to: 1. Inspect built-in codebooks
to understand best practices 2. Create custom codebooks from scratch for
specific research questions 3. Apply codebooks to data using
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md)

The flexibility of custom codebooks allows you to adapt quallmer to any
qualitative coding task in your research!
