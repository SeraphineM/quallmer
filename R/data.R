#' Sample of UK manifesto sentences 2010 crowd-annotated for immigration
#'
#' @description A corpus of sentences sampled from from publicly available party
#'   manifestos from the United Kingdom from the 2010 election.  Each sentence
#'   has been rated in terms of its classification as pertaining to immigration
#'   or not and then on a scale of favorability or not toward open immigration
#'   policy (as the mean score of crowd coders on a scale of -1 (favours open
#'   immigration policy), 0 (neutral), or 1 (anti-immigration).
#'
#' @description The sentences were sampled from the corpus used in [Benoit et al.
#'   (2016)](https://doi.org/10.1017/S0003055416000058), which contains more
#'   information on the crowd-sourced annotation  approach.
#' @format A [corpus][quanteda::corpus] object.
#'   The corpus consists of 155 sentences randomly sampled from the party
#'   manifestos, with an attempt to balance the sentencs according to their
#'   categorisation as pertaining to immigration or not, as well as by party.
#'   The corpus contains the following document-level variables: \describe{
#'   \item{party}{factor; abbreviation of the party that wrote the manifesto.}
#'   \item{partyname}{factor; party that wrote the manifesto.}
#'   \item{year}{integer; 4-digit year of the election.}
#'   \item{crowd_immigration_label}{Factor indicating whether the majority of
#'   crowd workers labelled a sentence as referring to immigration or not. The
#'   variable has missing values (`NA`) for all non-annotated manifestos.}
#'   \item{crowd_immigration_mean}{numeric; the direction
#'   of statements coded as "Immigration" based on the aggregated crowd codings.
#'   The variable is the mean of the scores assigned by workers who coded a
#'   sentence and who allocated the sentence to the "Immigration" category. The
#'   variable ranges from -1 (Favorable and open immigration policy) to +1
#'   ("Negative and closed immigration policy").}
#'   \item{crowd_immigration_n}{integer; the number of coders who
#'   contributed to the
#'   mean score `crowd_immigration_mean`.}
#'   }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016).
#'   [Crowd-sourced Text Analysis:
#'   Reproducible and Agile Production of Political Data](https://doi.org/10.1017/S0003055416000058).
#'   *American Political Science Review*, 100,(2), 278--295.
#' @keywords data
#' @examples
#' # Inspect the corpus
#' summary(data_corpus_manifsentsUK2010sample)
"data_corpus_manifsentsUK2010sample"

#' Sample from Large Movie Review Dataset (Maas et al. 2011)
#'
#' A sample of 100 positive and 100 negative reviews from the Maas et al. (2011)
#' dataset for sentiment classification.  The original dataset contains 50,000
#' highly polar movie reviews.
#' @format The corpus docvars consist of:
#'   \describe{
#'   \item{docnumber}{serial (within set and polarity) document number}
#'   \item{rating}{user-assigned movie rating on a 1-10 point integer scale}
#'   \item{polarity}{either `neg` or `pos` to indicate whether the
#'     movie review was negative or positive.  See Maas et al (2011) for the
#'     cut-off values that governed this assignment.}
#'   }
#' @references Andrew L. Maas, Raymond E. Daly, Peter T. Pham, Dan Huang, Andrew
#'   Y. Ng, and Christopher Potts. (2011). "[Learning Word Vectors for Sentiment
#'   Analysis](http://ai.stanford.edu/~amaas/papers/wvSent_acl2011.pdf)". The
#'   49th Annual Meeting of the Association for Computational Linguistics (ACL
#'   2011).
#' @source <http://ai.stanford.edu/~amaas/data/sentiment/>
#' @seealso [data_codebook_sentiment] for an example codebook and usage with this corpus
#' @keywords data
#' @examples
#' # Inspect the corpus
#' summary(data_corpus_LMRDsample)
#'
#' # Sample a few reviews
#' head(data_corpus_LMRDsample, 3)
"data_corpus_LMRDsample"


#' Sentiment analysis codebook for movie reviews
#'
#' A `qlm_codebook` object defining instructions for sentiment analysis of movie
#' reviews. Designed to work with [data_corpus_LMRDsample] but with an expanded
#' polarity scale that includes a "mixed" category.
#'
#' @format A `qlm_codebook` object containing:
#'   \describe{
#'     \item{name}{Task name: "Movie Review Sentiment"}
#'     \item{instructions}{Coding instructions for analyzing movie review sentiment}
#'     \item{schema}{Response schema with two fields: `polarity` (Enum of "neg", "mixed", or "pos") and `rating` (Integer from 1 to 10)}
#'     \item{role}{Expert film critic persona}
#'     \item{input_type}{"text"}
#'   }
#'
#' @seealso [qlm_codebook()], [qlm_code()], [qlm_compare()], [data_corpus_LMRDsample]
#' @keywords data
#' @examples
#' # View the codebook
#' data_codebook_sentiment
#'
#' \donttest{
#' # Use with movie review corpus (requires API key)
#' coded <- qlm_code(data_corpus_LMRDsample[1:10],
#'                   data_codebook_sentiment,
#'                   model = "openai")
#'
#' # Create multiple coded versions for comparison
#' coded1 <- qlm_code(data_corpus_LMRDsample[1:20],
#'                    data_codebook_sentiment,
#'                    model = "openai/gpt-4o-mini")
#' coded2 <- qlm_code(data_corpus_LMRDsample[1:20],
#'                    data_codebook_sentiment,
#'                    model = "openai/gpt-4o")
#'
#' # Compare inter-rater reliability
#' comparison <- qlm_compare(coded1, coded2, by = "rating", level = "interval")
#' print(comparison)
#' }
"data_codebook_sentiment"


#' Speaker-level ideology scores from Maerz & Schneider (2020)
#'
#' A dataset containing dictionary-based liberal-illiberal rhetoric scores
#' for 40 heads of government, from Maerz & Schneider (2020).
#'
#' @format A data frame with 40 rows and 5 variables:
#'   \describe{
#'     \item{speaker}{Character. Name of the head of government.}
#'     \item{regime}{Character. Regime type: "Democracy" or "Autocracy".}
#'     \item{country}{Character. Country name.}
#'     \item{n_speeches}{Integer. Number of speeches analyzed.}
#'     \item{score}{Numeric. Liberal-illiberal rhetoric score (negative = illiberal, positive = liberal).}
#'   }
#'
#' @references
#' Maerz, S. F., & Schneider, C. Q. (2020). Comparing public communication in
#' democracies and autocracies: Automated text analyses of speeches by heads
#' of government. *Quality & Quantity*, 54, 517-545.
#' \doi{10.1007/s11135-019-00885-7}
#'
#' @source Replication data available at \url{https://dataverse.harvard.edu/dataverse/sfm}
#' @seealso [data_speeches_ms2020_sample]
#' @keywords data
#' @examples
#' # View the data
#' head(data_speakers_ms2020)
#'
#' # Plot ideology by regime type
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(data_speakers_ms2020, aes(x = reorder(speaker, score), y = score, color = regime)) +
#'     geom_point() +
#'     coord_flip() +
#'     labs(x = NULL, y = "Illiberal - Liberal")
#' }
"data_speakers_ms2020"


#' Sample of political speeches from Maerz & Schneider (2020)
#'
#' A sample of 100 speeches from the Maerz & Schneider (2020) corpus,
#' balanced across speakers and regime types. This sample is included in
#' the package for demos and testing. The full corpus of 4,740 speeches
#' is available in the package's pkgdown examples folder.
#'
#' @format A data frame with 100 rows and 8 variables:
#'   \describe{
#'     \item{.id}{Integer. Unique identifier (from full corpus).}
#'     \item{speaker}{Character. Name of the head of government.}
#'     \item{country}{Character. Country name.}
#'     \item{regime}{Character. Regime type: "Democracy" or "Autocracy".}
#'     \item{score}{Numeric. Original dictionary-based liberal-illiberal score.}
#'     \item{date}{Date. Date of the speech.}
#'     \item{title}{Character. Title of the speech.}
#'     \item{text}{Character. Full text of the speech.}
#'   }
#'
#' @references
#' Maerz, S. F., & Schneider, C. Q. (2020). Comparing public communication in
#' democracies and autocracies: Automated text analyses of speeches by heads
#' of government. *Quality & Quantity*, 54, 517-545.
#' \doi{10.1007/s11135-019-00885-7}
#'
#' @source Replication data available at \url{https://dataverse.harvard.edu/dataverse/sfm}
#' @seealso [data_speakers_ms2020]
#' @keywords data
#' @examples
#' # View the sample
#' head(data_speeches_ms2020_sample)
#'
#' # Speakers in the sample
#' table(data_speeches_ms2020_sample$regime)
"data_speeches_ms2020_sample"
