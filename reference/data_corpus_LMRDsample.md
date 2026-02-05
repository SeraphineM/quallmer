# Sample from Large Movie Review Dataset (Maas et al. 2011)

A sample of 100 positive and 100 negative reviews from the Maas et al.
(2011) dataset for sentiment classification. The original dataset
contains 50,000 highly polar movie reviews.

## Usage

``` r
data_corpus_LMRDsample
```

## Format

The corpus docvars consist of:

- docnumber:

  serial (within set and polarity) document number

- rating:

  user-assigned movie rating on a 1-10 point integer scale

- polarity:

  either `neg` or `pos` to indicate whether the movie review was
  negative or positive. See Maas et al (2011) for the cut-off values
  that governed this assignment.

## Source

<http://ai.stanford.edu/~amaas/data/sentiment/>

## References

Andrew L. Maas, Raymond E. Daly, Peter T. Pham, Dan Huang, Andrew Y. Ng,
and Christopher Potts. (2011). "[Learning Word Vectors for Sentiment
Analysis](http://ai.stanford.edu/~amaas/papers/wvSent_acl2011.pdf)". The
49th Annual Meeting of the Association for Computational Linguistics
(ACL 2011).

## See also

[data_codebook_sentiment](https://quallmer.github.io/quallmer/reference/data_codebook_sentiment.md)
for an example codebook and usage with this corpus

## Examples

``` r
# Inspect the corpus
summary(data_corpus_LMRDsample)
#>    Length    Class1    Class2      Mode 
#>       200    corpus character character 

# Sample a few reviews
head(data_corpus_LMRDsample, 3)
#> Corpus consisting of 3 documents.
#> 
#> Docvars: docnumber, rating, polarity
#> 
#> 1035_3.txt: A frustrating documentary. Louis Kahn's son, who saw his father onl...
#> 3540_3.txt: I truly was disappointed by this film which I had high hopes for. I...
#> [ ... and 1 more document ]
```
