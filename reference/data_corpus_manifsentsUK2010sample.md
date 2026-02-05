# Sample of UK manifesto sentences 2010 crowd-annotated for immigration

A corpus of sentences sampled from from publicly available party
manifestos from the United Kingdom from the 2010 election. Each sentence
has been rated in terms of its classification as pertaining to
immigration or not and then on a scale of favorability or not toward
open immigration policy (as the mean score of crowd coders on a scale of
-1 (favours open immigration policy), 0 (neutral), or 1
(anti-immigration).

The sentences were sampled from the corpus used in [Benoit et al.
(2016)](https://doi.org/10.1017/S0003055416000058), which contains more
information on the crowd-sourced annotation approach.

## Usage

``` r
data_corpus_manifsentsUK2010sample
```

## Format

A [corpus](https://quanteda.io/reference/corpus.html) object. The corpus
consists of 155 sentences randomly sampled from the party manifestos,
with an attempt to balance the sentencs according to their
categorisation as pertaining to immigration or not, as well as by party.
The corpus contains the following document-level variables:

- party:

  factor; abbreviation of the party that wrote the manifesto.

- partyname:

  factor; party that wrote the manifesto.

- year:

  integer; 4-digit year of the election.

- crowd_immigration_label:

  Factor indicating whether the majority of crowd workers labelled a
  sentence as referring to immigration or not. The variable has missing
  values (`NA`) for all non-annotated manifestos.

- crowd_immigration_mean:

  numeric; the direction of statements coded as "Immigration" based on
  the aggregated crowd codings. The variable is the mean of the scores
  assigned by workers who coded a sentence and who allocated the
  sentence to the "Immigration" category. The variable ranges from -1
  (Favorable and open immigration policy) to +1 ("Negative and closed
  immigration policy").

- crowd_immigration_n:

  integer; the number of coders who contributed to the mean score
  `crowd_immigration_mean`.

## References

Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S.
(2016). [Crowd-sourced Text Analysis: Reproducible and Agile Production
of Political Data](https://doi.org/10.1017/S0003055416000058). *American
Political Science Review*, 100,(2), 278–295.

## Examples

``` r
# Inspect the corpus
summary(data_corpus_manifsentsUK2010sample)
#>    Length    Class1    Class2      Mode 
#>       155    corpus character character 
```
