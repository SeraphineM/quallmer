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

data_corpus_manifsentsUK2010sample <- data_corpus_manifsentsUK2010sample %>%
  rename(immigration_label = crowd_immigration_label,
         immigration_mean = crowd_immigration_mean,
         immigration_n = crowd_immigration_n) %>%
  mutate(immigration_position = as.integer(as.character(cut(
    data_corpus_manifsentsUK2010sample$crowd_immigration_mean,
    breaks = c(-Inf, -.5, .5, Inf),
    labels = c(-1, 0, 1)))))

usethis::use_data(data_corpus_manifsentsUK2010sample, overwrite = TRUE)

