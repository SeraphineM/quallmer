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
#' \dontrun{
#' library(quanteda)
#'
#' immigration_instructions <- "This task involves reading sentences from
#' political texts from the 2010 UK general election, and judging whether
#' these statements deal with immigration policy. Each sentence may or may
#' not be related to immigration policy.
#'
#' First, you will read a short section from a party manifesto. For the
#' sentence highlighted in red, enter your best judgment about whether it
#' refers to some aspect of immigration policy, or not. Most sentences will
#' not relate to immigration policy - it is your job to find and rate those
#' that do. If the sentence does not refer to immigration policy, you should
#' select 'Not immigration policy' and proceed directly to the next sentence.
#' If the sentence does refer to immigration policy, you should indicate this
#' by checking this option.
#'
#' If you indicate that the sentence is immigration policy, you will be asked
#' to give your best judgment of the policy position on immigration being
#' expressed in the sentence. This will range from a very open and favourable
#' position on immigration, to a very closed and negative stance on
#' immigration. These are coded on a five-point scale, with a neutral position
#' (neither favouring nor opposing) immigration lying in the middle."
#'
#' immigration_description <- "What is 'immigration policy'?
#'
#' Immigration policy relates to all government policies, laws, regulations,
#' and practices that deal with the free travel of foreign persons across the
#' country's borders, especially those that intend to live, work, or seek
#' legal protection (asylum) in that country. Examples of specific policies
#' that pertain to immigration include the regulation of: work permits for
#' foreign nationals; residency permits for foreign nationals; asylum seekers
#' and their treatment; requirements for acquiring citizenship; illegal
#' immigrants and migrant workers (and their families) living or working
#' illegally in the country.
#'
#' It also includes favorable or unfavorable general statements about
#' immigrants or immigration policy, such as statements indicating that
#' immigration has been good for a country, or that immigrants have forced
#' local people out of jobs, etc."
#'
#' immigration_scale <- "Pro-immigration policies (a value of -1)
#'
#' Examples of 'pro' immigration positions include: Positive statements about
#' the benefits of immigration, such as economic or cultural benefits;
#' Statements about the moral obligation to welcome asylum seekers; Policies
#' that would improve conditions for asylum seekers and their families; Urging
#' an increase the number of work permits for foreign nationals; Making it
#' possible for illegal immigrants to obtain a legal status or even
#' citizenship; Reducing barriers to immigration generally.
#'
#' Anti-immigration policies (a value of 1)
#'
#' Examples of 'anti' immigration positions include: Negative statements about
#' consequences of immigration, such as job losses, increased crime, or
#' destruction of national culture; Arguments about asylum seekers abusing the
#' system; Policies to deport asylum seekers and their families; Urging
#' restrictions on the number of work permits for foreign nationals, including
#' points systems; Deporting illegal immigrants and their families; Increasing
#' barriers to immigration generally.
#'
#' Neutral immigration policy statements (a value of 0)
#'
#' Examples of neutral statements about immigration policy: Advocating a
#' balanced approach to the problem; Statements about administrative capacity
#' for handling immigration or asylum seekers; Statements that do not take a
#' pro- or anti-immigration stance generally, despite making some statement
#' about immigration."
#'
#' # define a codebook
#' codebook_immigration <- qlm_codebook(
#'   name = "Immigration policy",
#'   instructions = immigration_instructions,
#'   schema = type_object(
#'     immigration_label = type_enum(c("Immigration", "Not immmigration")),
#'     immigration_scale = type_integer(immigration_scale)
#'   )
#' )
#'
#' result <- qlm_code(data_corpus_manifsentsUK2010sample,
#'                    codebook_immigration,
#'                    model = "openai/gpt-4o")
#' gold <- data.frame(.id = docnames(data_corpus_manifsentsUK2010sample),
#'                    immigration_label = data_corpus_manifsentsUK2010sample$crowd_immigration_label,
#'                    immigration_scale = data_corpus_manifsentsUK2010sample$crowd_immigration_mean)
#' qlm_validate(result, gold, by = "immigration_label")
#' qlm_validate(result, gold, by = "immigration_scale", level = "interval")
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
#' @keywords data
#' @examples
#' \dontrun{
#' library(quanteda)
#'
#' # define a sentiment codebook
#' codebook_posneg <- qlm_codebook(
#'   name = "Sentiment analysis of movie reviews",
#'   instructions = "You will rate the sentiment from movie reviews.",
#'   schema = type_object(
#'     polarity = type_enum(c("pos", "neg"),
#'     description = "Sentiment label (pos = positive, neg = negative")
#'   )
#' )
#'
#' set.seed(10001)
#' test_corpus <- data_corpus_LMRDsample %>%
#'   corpus_sample(size = 10, by = polarity)
#'
#' result <- qlm_code(test_corpus, codebook_posneg, model = "openai/gpt-4o-mini")
#'
#' # Create gold standard from corpus metadata
#' gold <- data.frame(.id = result$.id, polarity = test_corpus$polarity)
#'
#' # Validate against human annotations
#' qlm_validate(result, gold, by = "polarity")
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
#' \dontrun{
#' # View the codebook
#' data_codebook_sentiment
#'
#' # Use with movie review corpus
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


#' Stance detection codebook for climate change
#'
#' A `qlm_codebook` object defining instructions for detecting stance towards
#' climate change in texts.
#'
#' @format A `qlm_codebook` object containing:
#'   \describe{
#'     \item{name}{Task name: "Stance detection"}
#'     \item{instructions}{Coding instructions for classifying stance}
#'     \item{schema}{Response schema with two fields: `stance` (String indicating "Pro", "Neutral", or "Contra") and `explanation` (Brief explanation of the classification)}
#'     \item{role}{Expert annotator persona}
#'     \item{input_type}{"text"}
#'   }
#'
#' @seealso [qlm_codebook()], [qlm_code()]
#' @keywords data
#' @examples
#' \dontrun{
#' # View the codebook
#' data_codebook_stance
#'
#' # Use with text data
#' coded <- qlm_code(tail(quanteda::data_corpus_inaugural),
#'                   data_codebook_stance,
#'                   model = "openai/gpt-4o-mini")
#'  coded
#' }
"data_codebook_stance"


#' Ideological scaling codebook for left-right dimension
#'
#' A `qlm_codebook` object defining instructions for scaling texts on a
#' left-right ideological dimension.
#'
#' @format A `qlm_codebook` object containing:
#'   \describe{
#'     \item{name}{Task name: "Ideological scaling"}
#'     \item{instructions}{Coding instructions for ideological scaling}
#'     \item{schema}{Response schema with two fields: `score` (Integer from 0 (left) to 10 (right)) and `explanation` (Brief justification for the assigned score)}
#'     \item{role}{Expert political scientist persona}
#'     \item{input_type}{"text"}
#'   }
#'
#' @seealso [qlm_codebook()], [qlm_code()]
#' @keywords data
#' @examples
#' \dontrun{
#' # View the codebook
#' data_codebook_ideology
#'
#' # Use with political texts
#' coded <- qlm_code(tail(quanteda::data_corpus_inaugural),
#'                   data_codebook_ideology,
#'                   model = "openai/gpt-4o-mini")
#' coded
#' }
"data_codebook_ideology"


#' Topic salience codebook
#'
#' A `qlm_codebook` object defining instructions for extracting and ranking
#' topics discussed in texts by their salience.
#'
#' @format A `qlm_codebook` object containing:
#'   \describe{
#'     \item{name}{Task name: "Salience (ranked topics)"}
#'     \item{instructions}{Coding instructions for topic salience ranking}
#'     \item{schema}{Response schema with two fields: `topics` (Array of strings listing topics by salience, up to 5) and `explanation` (Brief explanation of topic selection and ordering)}
#'     \item{role}{Expert content analyst persona}
#'     \item{input_type}{"text"}
#'   }
#'
#' @seealso [qlm_codebook()], [qlm_code()]
#' @keywords data
#' @examples
#' \dontrun{
#' # View the codebook
#' data_codebook_salience
#'
#' # Use with documents
#' coded <- qlm_code(tail(quanteda::data_corpus_inaugural),
#'                   data_codebook_salience,
#'                   model = "openai/gpt-4o-mini")
#' coded
#' }
"data_codebook_salience"


#' Fact-checking codebook
#'
#' A `qlm_codebook` object defining instructions for assessing the truthfulness
#' and accuracy of texts.
#'
#' @format A `qlm_codebook` object containing:
#'   \describe{
#'     \item{name}{Task name: "Fact-checking"}
#'     \item{instructions}{Coding instructions for truthfulness assessment}
#'     \item{schema}{Response schema with three fields: `truth_score` (Integer from 0 (false/misleading) to 10 (accurate)), `misleading_topic` (Array of topics that reduce confidence, up to 5), and `explanation` (Brief explanation of the truthfulness score)}
#'     \item{role}{Expert fact-checker persona}
#'     \item{input_type}{"text"}
#'   }
#'
#' @seealso [qlm_codebook()], [qlm_code()]
#' @keywords data
#' @examples
#' \dontrun{
#' # View the codebook
#' data_codebook_fact
#'
#' # Use with claims or articles
#' # NEEDS ACTUAL DATA
#' coded <- qlm_code(claims,
#'                   data_codebook_fact,
#'                   model = "openai/gpt-4o-mini")
#' }
"data_codebook_fact"
