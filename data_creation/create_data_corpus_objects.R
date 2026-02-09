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

## create the political speeches corpus (Maerz & Schneider 2020)

# Load the full speeches dataset
data_speeches_ms2020 <- readRDS("vignettes/pkgdown/examples/data/data_speeches_ms2020.rds")

# Create a balanced sample of 100 speeches
set.seed(1002)
data_speeches_sample <- data_speeches_ms2020 |>
  dplyr::group_by(regime) |>
  dplyr::slice_sample(n = 50) |>
  dplyr::ungroup()

# Fix malformed 0x92 byte (Windows-1252 smart quote) by replacing with apostrophe
data_speeches_sample <- data_speeches_sample |>
  dplyr::mutate(dplyr::across(where(is.character), ~gsub("\x92", "'", .x, useBytes = TRUE)))

# Convert date and regime to appropriate types
data_speeches_sample <- data_speeches_sample |>
  dplyr::mutate(
    date = as.Date(date),
    regime = factor(regime)
  )

# Convert to quanteda corpus (quanteda handles text normalization)
data_corpus_ms2020sample <- quanteda::corpus(
  data_speeches_sample,
  docid_field = ".id",
  text_field = "text"
)

usethis::use_data(data_corpus_ms2020sample, overwrite = TRUE)

