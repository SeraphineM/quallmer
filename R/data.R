#' Sample of UK manifesto sentences 2010 crowd-annotated for immigration
#'
#' @description A corpus of sentences sampled from from publicly available party
#'   manifestos from the United Kingdom from the 2010 election.  Each sentence
#'   has been rated in terms of its classification as pertaining to immigration
#'   or not and then on a scale of favorability or not toward open immigration
#'   policy (as the mean score of crowd coders on a scale of -1 (favours open
#'   immigration policy), 0 (neutral), or 1 (anti-immigration).
#'
#' @description The sentences were sampled from the corpus used in Benoit et al.
#'   (2016) \doi{10.1017/S0003055416000058}, which contains more
#'   information on the crowd-sourced annotation  approach.
#' @format A [corpus][quanteda::corpus] object.
#'   The corpus consists of 155 sentences randomly sampled from the party
#'   manifestos, with an attempt to balance the sentencs according to their
#'   categorisation as pertaining to immigration or not, as well as by party.
#'   The corpus contains the following document-level variables: \describe{
#'   \item{party}{factor; abbreviation of the party that wrote the manifesto.}
#'   \item{partyname}{factor; party that wrote the manifesto.}
#'   \item{year}{integer; 4-digit year of the election.}
#'   \item{immigration_label}{Factor indicating whether the majority of
#'   crowd workers labelled a sentence as referring to immigration or not. The
#'   variable has missing values (`NA`) for all non-annotated manifestos.}
#'   \item{immigration_mean}{numeric; the direction
#'   of statements coded as "Immigration" based on the aggregated crowd codings.
#'   The variable is the mean of the scores assigned by workers who coded a
#'   sentence and who allocated the sentence to the "Immigration" category. The
#'   variable ranges from -1 (Favorable and open immigration policy) to +1
#'   ("Negative and closed immigration policy").}
#'   \item{immigration_n}{integer; the number of coders who
#'   contributed to the mean score `immigration_mean`.}
#'   \item{immigration_position}{integer; a thresholded version of `immigration_mean`
#'   coded as -1 (pro-immigration, mean < -0.5), 0 (neutral, -0.5 <= mean <= 0.5),
#'   or 1 (anti-immigration, mean > 0.5). Set to `NA` for non-immigration sentences.}
#'   }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016).
#'   Crowd-sourced Text Analysis:
#'   Reproducible and Agile Production of Political Data.
#'   \emph{American Political Science Review}, 100,(2), 278--295.
#'   \doi{10.1017/S0003055416000058}
#' @keywords data
#' @examples
#' if (requireNamespace("quanteda", quietly = TRUE)) {
#'   # Inspect the corpus
#'   summary(data_corpus_manifsentsUK2010sample)
#' }
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
#' if (requireNamespace("quanteda", quietly = TRUE)) {
#'   # Inspect the corpus
#'   summary(data_corpus_LMRDsample)
#'
#'   # Sample a few reviews
#'   head(data_corpus_LMRDsample, 3)
#' }
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


#' Immigration policy codebook based on Benoit et al. (2016)
#'
#' A `qlm_codebook` object defining instructions for annotating whether a text
#' pertains to immigration policy and, if so, the stance toward immigration
#' openness. This codebook replicates the crowd-sourced annotation task from
#' Benoit et al. (2016) and is designed to work with
#' [data_corpus_manifsentsUK2010sample].
#'
#' @format A `qlm_codebook` object containing:
#'   \describe{
#'     \item{name}{Task name: "Immigration policy coding from Benoit et al. (2016)"}
#'     \item{instructions}{Coding instructions for identifying whether sentences
#'       from UK 2010 election manifestos pertain to immigration policy, and if so,
#'       rating the policy position expressed}
#'     \item{schema}{Response schema with two fields: `llm_immigration_label`
#'       (Enum: "Not immigration" or "Immigration" indicating whether the sentence
#'       relates to immigration policy), and `llm_immigration_position` (Integer
#'       from -1 to 1, where -1 = pro-immigration, 0 = neutral, and 1 =
#'       anti-immigration)}
#'     \item{input_type}{"text"}
#'     \item{levels}{Named character vector: llm_immigration_label = "nominal",
#'       llm_immigration_position = "ordinal"}
#'   }
#'
#' @references
#' Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov, S. (2016).
#' Crowd-sourced Text Analysis:
#' Reproducible and Agile Production of Political Data.
#' \emph{American Political Science Review}, 110(2), 278--295.
#' \doi{10.1017/S0003055416000058}
#'
#' @seealso [qlm_codebook()], [qlm_code()], [data_corpus_manifsentsUK2010sample]
#' @keywords data
#' @examples
#' # View the codebook
#' data_codebook_immigration
#'
#' \dontrun{
#' # Use with UK manifesto sentences (requires API key)
#' if (requireNamespace("quanteda", quietly = TRUE)) {
#'   coded <- qlm_code(data_corpus_manifsentsUK2010sample,
#'                     data_codebook_immigration,
#'                     model = "openai/gpt-4o-mini")
#'
#'   # Compare with crowd-sourced annotations
#'   crowd <- as_qlm_coded(
#'     data.frame(
#'       .id = docnames(data_corpus_manifsentsUK2010sample),
#'       docvars(data_corpus_manifsentsUK2010sample)
#'     ),
#'     is_gold = TRUE
#'   )
#'
#'   qlm_validate(coded, gold = crowd)
#'
#' }
#' }
"data_codebook_immigration"


#' Manifesto Project example manifestos and gold-standard segmentation
#'
#' @description Two datasets derived from Appendix 2 of Klingemann et al.
#'   (2006), which provides worked examples of the Manifesto Project
#'   quasi-sentence coding scheme.
#'
#' `data_corpus_MPexamples` is a two-document corpus containing the full source
#' texts of the Liberal-SDP Alliance 1983 UK election manifesto and the New
#' Zealand National Party 1972 election manifesto, reconstructed by joining
#' the quasi-sentences from the gold-standard annotation.
#'
#' `data_corpus_MPexamplesseg` is the corresponding gold-standard segmented
#' corpus, produced by converting the Manifesto Project's human-coded
#' quasi-sentences via [as_qlm_coded()] with `qlm_segment = TRUE`. It is marked
#' as a gold standard (`is_gold = TRUE`) and can be passed directly to
#' [qlm_compare()] alongside output from [qlm_segment()] to compute
#' Krippendorff's alpha for unitizing.
#'
#' @name data_corpus_MPexamples
#'
#' @format
#' **`data_corpus_MPexamples`**: A [corpus][quanteda::corpus] with 2 documents
#' and the following document-level variables:
#' \describe{
#'   \item{country}{Character. Country of origin: `"UK"` or `"NZ"`.}
#'   \item{party}{Character. Party name: `"Liberal-SDP Alliance"` or
#'     `"National Party"`.}
#'   \item{year}{Integer. Election year: `1983` or `1972`.}
#' }
#'
#' **`data_corpus_MPexamplesseg`**: A segmented [corpus][quanteda::corpus] with
#' 178 quasi-sentences (107 Liberal-SDP, 71 NZ National Party) and the
#' following document-level variables:
#' \describe{
#'   \item{docid}{Character. Source document identifier (`"Liberal_SDP_1983"`
#'     or `"NZ_NP_1972"`).}
#'   \item{segid}{Integer. Quasi-sentence index within the source document.}
#'   \item{char_start}{Integer. Start character position in the source text.}
#'   \item{char_end}{Integer. End character position in the source text.}
#'   \item{manifesto}{Character. Manifesto Project manifesto label
#'     (`"Liberal-SDP 1983"` or `"NP 1972"`).}
#'   \item{country}{Character. Country of origin: `"UK"` or `"NZ"`.}
#'   \item{per}{Integer. Manifesto Project policy category code.}
#' }
#'
#' @references
#' Klingemann, H. D., Volkens, A., Bara, J., Budge, I., & McDonald, M. D.
#' (2006). \emph{Mapping Policy Preferences II: Estimates for Parties,
#' Electors, and Governments in Eastern Europe, European Union, and OECD
#' 1990--2003}. Oxford University Press.
#'
#' @seealso [qlm_segment()], [as_qlm_coded()], [qlm_compare()]
#' @keywords data
#' @examples
#' if (requireNamespace("quanteda", quietly = TRUE)) {
#'   # Inspect the source texts
#'   summary(data_corpus_MPexamples)
#'
#'   # Subset to one manifesto
#'   quanteda::corpus_subset(data_corpus_MPexamples, country == "NZ")
#'
#'   # Gold-standard segmentation for the NZ manifesto
#'   quanteda::corpus_subset(data_corpus_MPexamplesseg,
#'                           quanteda::docvars(data_corpus_MPexamplesseg,
#'                                            "docid") == "NZ_NP_1972")
#' }
"data_corpus_MPexamples"

#' @rdname data_corpus_MPexamples
"data_corpus_MPexamplesseg"

#' Sample corpus of political speeches from Maerz & Schneider (2020)
#'
#' A corpus of 100 speeches from the Maerz & Schneider (2020) corpus,
#' balanced across regime types (50 autocracies, 50 democracies). This sample
#' is included in the package for demos and testing. The full corpus of 4,740
#' speeches is available in the package's pkgdown examples folder.
#'
#' @format A [corpus][quanteda::corpus] object.
#'   The corpus consists of 100 speeches randomly sampled from 40 heads of
#'   government across 27 countries, balanced by regime type. The corpus
#'   contains the following document-level variables:
#'   \describe{
#'     \item{speaker}{Character. Name of the head of government.}
#'     \item{country}{Character. Country name.}
#'     \item{regime}{Factor. Regime type: "Democracy" or "Autocracy".}
#'     \item{score}{Numeric. Original dictionary-based liberal-illiberal score.}
#'     \item{date}{Date. Date of the speech.}
#'     \item{title}{Character. Title of the speech.}
#'   }
#'
#' @references
#' Maerz, S. F., & Schneider, C. Q. (2020). Comparing public communication in
#' democracies and autocracies: Automated text analyses of speeches by heads
#' of government. *Quality & Quantity*, 54, 517-545.
#' \doi{10.1007/s11135-019-00885-7}
#'
#' @keywords data
#' @examples
#' if (requireNamespace("quanteda", quietly = TRUE)) {
#'   # Inspect the corpus
#'   summary(data_corpus_ms2020sample, n = 10)
#'
#'   # Regime distribution
#'   table(data_corpus_ms2020sample$regime)
#'
#'   # View a sample speech
#'   cat(data_corpus_ms2020sample[1])
#' }
"data_corpus_ms2020sample"
