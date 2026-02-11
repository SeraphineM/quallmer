# Tests for as_qlm_coded() S3 generic and methods

# Test data.frame method -------------------------------------------------------

test_that("as_qlm_coded.data.frame creates object with correct structure", {
  data <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  result <- as_qlm_coded(data, name = "test_coder")

  # Check class structure (dual inheritance)
  expect_true(inherits(result, "qlm_humancoded"))
  expect_true(inherits(result, "qlm_coded"))
  expect_true(inherits(result, "tbl_df"))

  # Check structure
  expect_equal(nrow(result), 10)
  expect_true(".id" %in% names(result))
  expect_true("category" %in% names(result))

  # Check attributes
  meta_attr <- attr(result, "meta")
  expect_equal(meta_attr$user$name, "test_coder")
  expect_equal(meta_attr$object$source, "human")
  expect_equal(meta_attr$object$n_units, 10)
})

test_that("as_qlm_coded.data.frame validates inputs", {
  # Should error with non-data.frame
  expect_error(
    as_qlm_coded(list(a = 1)),
    "must be a data frame"
  )

  # Should error if .id column missing
  data_no_id <- data.frame(category = rep(c("A", "B"), 5))
  expect_error(
    as_qlm_coded(data_no_id),
    "must contain.*\\.id.*column"
  )
})

test_that("as_qlm_coded.data.frame accepts custom codebook", {
  data <- data.frame(.id = 1:10, sentiment = rep(c("pos", "neg"), 5))

  codebook <- list(
    name = "Sentiment Coding",
    instructions = "Code as positive or negative"
  )

  result <- as_qlm_coded(data, codebook = codebook)

  codebook_attr <- attr(result, "codebook")
  expect_equal(codebook_attr$name, "Sentiment Coding")
  expect_equal(codebook_attr$instructions, "Code as positive or negative")
})

test_that("as_qlm_coded.data.frame handles is_gold parameter", {
  data <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  # Test without is_gold
  result_not_gold <- as_qlm_coded(data, name = "test")
  expect_false(attr(result_not_gold, "meta")$object$is_gold)

  # Test with is_gold
  result_gold <- as_qlm_coded(data, name = "gold", is_gold = TRUE)
  expect_true(attr(result_gold, "meta")$object$is_gold)
})

test_that("as_qlm_coded.data.frame accepts texts parameter", {
  data <- data.frame(.id = 1:5, sentiment = rep(c("pos", "neg"), length.out = 5))
  texts <- c("text1", "text2", "text3", "text4", "text5")

  result <- as_qlm_coded(data, texts = texts)

  expect_equal(attr(result, "data"), texts)
})

test_that("as_qlm_coded.data.frame accepts metadata parameter", {
  data <- data.frame(.id = 1:5, sentiment = rep(c("pos", "neg"), length.out = 5))

  result <- as_qlm_coded(
    data,
    name = "expert",
    metadata = list(coder_name = "Dr. Smith", experience = "5 years")
  )

  meta_attr <- attr(result, "meta")
  expect_equal(meta_attr$user$coder_name, "Dr. Smith")
  expect_equal(meta_attr$user$experience, "5 years")
  # Should still have automatic metadata
  expect_equal(meta_attr$object$source, "human")
  expect_true(!is.null(meta_attr$system$timestamp))
})

test_that("as_qlm_coded.data.frame accepts custom id column", {
  data <- data.frame(
    doc_id = c("A", "B", "C"),
    sentiment = c("pos", "neg", "pos")
  )

  result <- as_qlm_coded(data, id = "doc_id", name = "test")

  # Check .id was created from doc_id
  expect_true(".id" %in% names(result))
  expect_equal(result$.id, c("A", "B", "C"))
  # Original id column should no longer exist as doc_id
  expect_false("doc_id" %in% names(result))
  # Other columns should still be present
  expect_true("sentiment" %in% names(result))
})

test_that("as_qlm_coded.data.frame errors with invalid id column", {
  data <- data.frame(
    doc_id = 1:5,
    sentiment = rep(c("pos", "neg"), length.out = 5)
  )

  expect_error(
    as_qlm_coded(data, id = "nonexistent"),
    "must contain.*nonexistent.*column"
  )
})

test_that("as_qlm_coded.data.frame supports NSE for id parameter", {
  data <- data.frame(
    doc_id = c("A", "B", "C"),
    sentiment = c("pos", "neg", "pos")
  )

  # Test with bare name (NSE)
  result_nse <- as_qlm_coded(data, id = doc_id, name = "test_nse")

  # Check .id was created from doc_id
  expect_true(".id" %in% names(result_nse))
  expect_equal(result_nse$.id, c("A", "B", "C"))

  # Test with quoted string (should work the same)
  result_quoted <- as_qlm_coded(data, id = "doc_id", name = "test_quoted")

  # Should produce identical results
  expect_equal(result_nse$.id, result_quoted$.id)
})

test_that("as_qlm_coded.data.frame defaults to .id when id is NULL", {
  data <- data.frame(
    .id = c("X", "Y", "Z"),
    sentiment = c("pos", "neg", "pos")
  )

  # Test with NULL (should use .id)
  result <- as_qlm_coded(data, name = "test")

  expect_equal(result$.id, c("X", "Y", "Z"))
})


# Test corpus method -----------------------------------------------------------

test_that("as_qlm_coded.corpus works with default id (names)", {
  # Load sample corpus
  data("data_corpus_manifsentsUK2010sample")

  result <- as_qlm_coded(data_corpus_manifsentsUK2010sample, name = "crowd")

  # Check class structure
  expect_true(inherits(result, "qlm_humancoded"))
  expect_true(inherits(result, "qlm_coded"))

  # Check .id matches document names
  expect_equal(result$.id, names(data_corpus_manifsentsUK2010sample))

  # Check that docvars are included (excluding internal quanteda vars)
  expect_true("party" %in% names(result))
  expect_true("partyname" %in% names(result))
  expect_true("year" %in% names(result))

  # Internal quanteda vars should NOT be included
  expect_false("docname_" %in% names(result))
  expect_false("docid_" %in% names(result))
  expect_false("segid_" %in% names(result))

  # Check texts are preserved
  expect_equal(attr(result, "data"), as.character(data_corpus_manifsentsUK2010sample))
})

test_that("as_qlm_coded.corpus works with custom id from docvar", {
  # Load sample corpus
  data("data_corpus_manifsentsUK2010sample")

  result <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    id = "party",
    name = "crowd"
  )

  # Check .id uses the party column
  docvars <- attr(data_corpus_manifsentsUK2010sample, "docvars")
  expect_equal(result$.id, docvars$party)

  # Check that party is NOT duplicated in other columns
  # (it should only appear as .id)
  other_cols <- setdiff(names(result), ".id")
  expect_false("party" %in% other_cols)

  # Other docvars should still be present
  expect_true("partyname" %in% names(result))
  expect_true("year" %in% names(result))
})

test_that("as_qlm_coded.corpus handles is_gold parameter", {
  data("data_corpus_manifsentsUK2010sample")

  result <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    is_gold = TRUE
  )

  meta_attr <- attr(result, "meta")
  expect_true(meta_attr$object$is_gold)
})

test_that("as_qlm_coded.corpus preserves corpus text by default", {
  data("data_corpus_manifsentsUK2010sample")

  result <- as_qlm_coded(data_corpus_manifsentsUK2010sample)

  expect_equal(attr(result, "data"), as.character(data_corpus_manifsentsUK2010sample))
})

test_that("as_qlm_coded.corpus allows custom texts parameter", {
  data("data_corpus_manifsentsUK2010sample")

  custom_texts <- rep("custom text", length(data_corpus_manifsentsUK2010sample))

  result <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    texts = custom_texts
  )

  expect_equal(attr(result, "data"), custom_texts)
})

test_that("as_qlm_coded.corpus errors with invalid id parameter", {
  data("data_corpus_manifsentsUK2010sample")

  expect_error(
    as_qlm_coded(data_corpus_manifsentsUK2010sample, id = "nonexistent"),
    "not found in corpus"
  )
})

test_that("as_qlm_coded.corpus errors when no user docvars", {
  # Create a minimal corpus with only internal docvars
  corpus_no_user_vars <- structure(
    c("text1", "text2"),
    names = c("doc1", "doc2"),
    class = "corpus",
    docvars = data.frame(
      docname_ = c("doc1", "doc2"),
      docid_ = 1:2,
      segid_ = c(1L, 1L)
    ),
    meta = list()
  )

  expect_error(
    as_qlm_coded(corpus_no_user_vars),
    "no user-defined document variables"
  )
})

test_that("as_qlm_coded.corpus errors when no docvars at all", {
  # Create a corpus with no docvars
  corpus_no_docvars <- structure(
    c("text1", "text2"),
    names = c("doc1", "doc2"),
    class = "corpus",
    docvars = NULL,
    meta = list()
  )

  expect_error(
    as_qlm_coded(corpus_no_docvars),
    "no document variables"
  )
})

test_that("as_qlm_coded.corpus errors when only id docvar exists", {
  # Create corpus with only one docvar
  corpus_single_var <- structure(
    c("text1", "text2"),
    names = c("doc1", "doc2"),
    class = "corpus",
    docvars = data.frame(
      docname_ = c("doc1", "doc2"),
      party = c("A", "B")
    ),
    meta = list()
  )

  expect_error(
    as_qlm_coded(corpus_single_var, id = "party"),
    "No coded variables remain"
  )
})

test_that("as_qlm_coded.corpus errors when corpus has no names and id is NULL", {
  # Create corpus without document names
  corpus_no_names <- structure(
    c("text1", "text2"),
    class = "corpus",
    docvars = data.frame(
      category = c("A", "B")
    ),
    meta = list()
  )

  expect_error(
    as_qlm_coded(corpus_no_names),
    "no document names"
  )
})

test_that("as_qlm_coded.corpus supports NSE for id parameter", {
  data("data_corpus_manifsentsUK2010sample")

  # Test with bare name (NSE)
  result_nse <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    id = party,
    name = "crowd_nse"
  )

  # Check that party docvar was used as .id
  docvars <- attr(data_corpus_manifsentsUK2010sample, "docvars")
  expect_equal(result_nse$.id, docvars$party)
  expect_false("party" %in% names(result_nse))

  # Test with quoted string (should work the same)
  result_quoted <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    id = "party",
    name = "crowd_quoted"
  )

  # Should produce identical results
  expect_equal(result_nse$.id, result_quoted$.id)
  expect_equal(names(result_nse), names(result_quoted))
})

test_that("as_qlm_coded.corpus defaults to document names when id is NULL", {
  data("data_corpus_manifsentsUK2010sample")

  # Test with NULL (should use document names)
  result <- as_qlm_coded(data_corpus_manifsentsUK2010sample, name = "crowd")

  expect_equal(result$.id, names(data_corpus_manifsentsUK2010sample))
  # All user docvars should be included
  expect_true("party" %in% names(result))
})


# Integration tests ------------------------------------------------------------

test_that("corpus-based qlm_coded works with qlm_validate", {
  data("data_corpus_manifsentsUK2010sample")

  # Create gold standard from corpus
  gold <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    is_gold = TRUE
  )

  # Create comparison data
  comparison <- data.frame(
    .id = names(data_corpus_manifsentsUK2010sample),
    party = attr(data_corpus_manifsentsUK2010sample, "docvars")$party
  )
  comp_coded <- as_qlm_coded(comparison, name = "comparison")

  # Should work with qlm_validate (gold detected automatically)
  # We're just checking it doesn't error - actual validation logic tested elsewhere
  expect_no_error({
    result <- qlm_validate(comp_coded, gold = gold, by = "party", level = "nominal")
  })
})

test_that("corpus method produces identical results to manual data.frame conversion", {
  data("data_corpus_manifsentsUK2010sample")

  # Method 1: Using corpus directly
  result_corpus <- as_qlm_coded(
    data_corpus_manifsentsUK2010sample,
    name = "test",
    is_gold = TRUE
  )

  # Method 2: Manual conversion (old way)
  docvars <- attr(data_corpus_manifsentsUK2010sample, "docvars")
  internal_vars <- c("docname_", "docid_", "segid_")
  user_vars <- setdiff(names(docvars), internal_vars)

  manual_df <- data.frame(
    .id = names(data_corpus_manifsentsUK2010sample),
    docvars[, user_vars],
    stringsAsFactors = FALSE
  )

  result_manual <- as_qlm_coded(
    manual_df,
    name = "test",
    is_gold = TRUE,
    texts = as.character(data_corpus_manifsentsUK2010sample)
  )

  # Should have identical data content
  expect_equal(nrow(result_corpus), nrow(result_manual))
  expect_equal(names(result_corpus), names(result_manual))
  expect_equal(result_corpus$.id, result_manual$.id)

  # Should have identical metadata (except call)
  expect_equal(
    attr(result_corpus, "meta")$object$is_gold,
    attr(result_manual, "meta")$object$is_gold
  )
  expect_equal(
    attr(result_corpus, "data"),
    attr(result_manual, "data")
  )
})


# Backward compatibility -------------------------------------------------------

test_that("existing qlm_humancoded calls still work", {
  # qlm_humancoded is just an alias that calls as_qlm_coded
  data <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  result <- qlm_humancoded(data, name = "test_coder")

  expect_true(inherits(result, "qlm_humancoded"))
  expect_true(inherits(result, "qlm_coded"))
})
