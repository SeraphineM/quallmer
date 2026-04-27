## Create data_corpus_MPexamples and data_corpus_MPexamplesseg
##
## Source: Manifesto Project sample manifestos (quasisentences.rda)
## Two documents: Liberal-SDP Alliance 1983 (UK) and National Party 1972 (NZ)

library(quanteda)
devtools::load_all()

load("vignettes/pkgdown/examples/data/quasi-sentences/quasisentences.rda")

# ---- Liberal-SDP 1983 source text ----------------------------------------
# Reconstruct from quasi-sentences. Two texts have CMP category codes embedded
# at split boundaries (e.g. "between 305 our two parties"); remove them.
lib <- quasisentences[quasisentences$manifesto == "Liberal-SDP 1983", ]
lib_texts_clean <- gsub(" [0-9]{3} ", " ", lib$text)
lib_source <- paste(lib_texts_clean, collapse = " ")

# ---- NP 1972 source text --------------------------------------------------
nz_source <- paste(
  readLines("vignettes/pkgdown/examples/data/quasi-sentences/NZ_NP_1972.txt"),
  collapse = "\n"
)

# ---- data_corpus_MPexamples -----------------------------------------------
source_texts <- c(Liberal_SDP_1983 = lib_source, NZ_NP_1972 = nz_source)

data_corpus_MPexamples <- quanteda::corpus(
  source_texts,
  docvars = data.frame(
    country = c("UK", "NZ"),
    party   = c("Liberal-SDP Alliance", "National Party"),
    year    = c(1983L, 1972L)
  )
)
class(data_corpus_MPexamples) <- c("qlm_corpus", class(data_corpus_MPexamples))

usethis::use_data(data_corpus_MPexamples, overwrite = TRUE)

# ---- data_corpus_MPexamplesseg --------------------------------------------
# Gold-standard segmentation from the Manifesto Project, converted to a
# segmented corpus via as_qlm_coded(qlm_segment = TRUE) for use with
# qlm_compare() unitizing alpha.

qs <- quasisentences
qs$text[qs$manifesto == "Liberal-SDP 1983"] <- lib_texts_clean
qs$docid <- ifelse(
  qs$manifesto == "Liberal-SDP 1983", "Liberal_SDP_1983", "NZ_NP_1972"
)
qs$sequence <- NULL

data_corpus_MPexamplesseg <- as_qlm_coded(
  qs,
  qlm_segment = TRUE,
  source_text  = source_texts,
  name         = "Manifesto Project",
  is_gold      = TRUE
)

usethis::use_data(data_corpus_MPexamplesseg, overwrite = TRUE)
