#### corpus creation

## create the movie review corpus

load("data_corpus_LMRD.rda")

set.seed(1001)
data_corpus_LMRDsample <- corpus_sample(data_corpus_LMRD, size = 100, by = polarity)
docnames(data_corpus_LMRDsample) <- data_corpus_LMRDsample |>
  docnames() |>
  basename()
data_corpus_LMRDsample$set <- NULL

usethis::use_data(data_corpus_LMRDsample, overwrite = TRUE)

## create the immigration sentence corpus

load("data_creation/data_corpus_manifestosentsUK.rda")

set.seed(1001)
data_corpus_manifsentsUK2010sample <- data_corpus_manifestosentsUK |>
  corpus_subset(year == 2010) |>
  corpus_sample(size = 10, by = interaction(party, crowd_immigration_label, drop = TRUE), replace = TRUE)

# remove duplicates
data_corpus_manifsentsUK2010sample <- corpus_subset(data_corpus_manifsentsUK2010sample,
                                                  !duplicated(data_corpus_manifsentsUK2010sample))
# tidy up docnames
docnames(data_corpus_manifsentsUK2010sample) <-
  sub("\\.[0-9]+$", "", docnames(data_corpus_manifsentsUK2010sample))

library(quanteda.tidy)
data_corpus_manifsentsUK2010sample <- data_corpus_manifsentsUK2010sample %>%
  select(contains("party"), year, contains("immigration"))

usethis::use_data(data_corpus_manifsentsUK2010sample, overwrite = TRUE)

