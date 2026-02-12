test_that("qlm_corpus print method works without quanteda", {
  # Skip if quanteda is loaded (we need a clean environment for this test)
  skip_if(isNamespaceLoaded("quanteda"), "quanteda is already loaded")

  # Load the data
  data("data_corpus_LMRDsample", package = "quallmer")

  # Verify corpus has qlm_corpus class
  expect_s3_class(data_corpus_LMRDsample, "qlm_corpus")
  expect_s3_class(data_corpus_LMRDsample, "corpus")

  # Test that our print method produces expected output
  output <- capture.output(print(data_corpus_LMRDsample[1:2]))

  expect_true(any(grepl("Corpus consisting of 2 document", output)))
  expect_true(any(grepl("Docvars:", output)))
  expect_true(any(grepl("1035_3.txt:", output)))
  expect_true(any(grepl("3540_3.txt:", output)))

  # Should NOT show the raw character vector attributes
  expect_false(any(grepl("attr\\(", output)))
  expect_false(any(grepl("docvars.*data.frame", output)))
})

test_that("qlm_corpus subset method preserves structure", {
  # Skip if quanteda is loaded
  skip_if(isNamespaceLoaded("quanteda"), "quanteda is already loaded")

  # Load the data
  data("data_corpus_LMRDsample", package = "quallmer")

  # Test subsetting
  subset_corpus <- data_corpus_LMRDsample[1:5]

  expect_s3_class(subset_corpus, "qlm_corpus")
  expect_s3_class(subset_corpus, "corpus")
  expect_equal(length(subset_corpus), 5)

  # Check docvars are preserved
  docvars_original <- attr(data_corpus_LMRDsample, "docvars")
  docvars_subset <- attr(subset_corpus, "docvars")

  expect_equal(nrow(docvars_subset), 5)
  expect_equal(docvars_subset, docvars_original[1:5, , drop = FALSE])

  # Check meta attribute is preserved
  expect_equal(attr(subset_corpus, "meta"), attr(data_corpus_LMRDsample, "meta"))
})

test_that("qlm_corpus methods delegate to quanteda when loaded", {
  # This test requires quanteda to be available
  skip_if_not_installed("quanteda")

  # Load the data
  data("data_corpus_LMRDsample", package = "quallmer")

  # Verify corpus has qlm_corpus class
  expect_s3_class(data_corpus_LMRDsample, "qlm_corpus")

  # Load quanteda
  suppressPackageStartupMessages(library(quanteda))

  # Our print method should detect quanteda and delegate to it
  output <- capture.output(print(data_corpus_LMRDsample[1:2]))

  # quanteda's output includes specific formatting
  expect_true(any(grepl("documents and.*docvars", output)) ||
              any(grepl("Corpus consisting of.*document", output)))

  # Unload quanteda for other tests
  try(unloadNamespace("quanteda"), silent = TRUE)
})

test_that("as_qlm_corpus wraps corpus objects correctly", {
  skip_if_not_installed("quanteda")

  # Create a simple corpus
  test_corpus <- quanteda::corpus(c("Text 1", "Text 2"))

  # Verify it doesn't have qlm_corpus class initially
  expect_false(inherits(test_corpus, "qlm_corpus"))

  # Apply wrapper
  wrapped_corpus <- quallmer:::as_qlm_corpus(test_corpus)

  # Verify it now has qlm_corpus class
  expect_s3_class(wrapped_corpus, "qlm_corpus")
  expect_s3_class(wrapped_corpus, "corpus")

  # Verify idempotence - applying twice doesn't duplicate
  double_wrapped <- quallmer:::as_qlm_corpus(wrapped_corpus)
  expect_equal(sum(class(double_wrapped) == "qlm_corpus"), 1)
})

test_that("as_qlm_corpus errors on non-corpus input", {
  # Test with various non-corpus inputs
  expect_error(
    quallmer:::as_qlm_corpus("not a corpus"),
    class = "rlang_error"
  )

  expect_error(
    quallmer:::as_qlm_corpus(data.frame(text = c("a", "b"))),
    class = "rlang_error"
  )

  expect_error(
    quallmer:::as_qlm_corpus(list(a = 1, b = 2)),
    class = "rlang_error"
  )
})

test_that("as_qlm_coded.corpus applies lazy coercion", {
  skip_if_not_installed("quanteda")

  # Create a plain corpus without qlm_corpus class
  test_corpus <- quanteda::corpus(
    c("Text 1", "Text 2"),
    docvars = data.frame(category = c("A", "B"))
  )

  # Verify it doesn't have qlm_corpus class initially
  expect_false(inherits(test_corpus, "qlm_corpus"))

  # Convert to qlm_coded - should apply wrapper internally
  coded <- as_qlm_coded(test_corpus)

  # The original corpus object passed to as_qlm_coded shouldn't be modified
  expect_false(inherits(test_corpus, "qlm_corpus"))

  # But the result should have extracted data correctly
  expect_s3_class(coded, "qlm_coded")
  expect_true(".id" %in% names(coded))
  expect_true("category" %in% names(coded))
})

test_that("subsetting with quanteda loaded preserves qlm_corpus class", {
  skip_if_not_installed("quanteda")

  # Load the data
  data("data_corpus_LMRDsample", package = "quallmer")

  # Verify it has qlm_corpus class
  expect_s3_class(data_corpus_LMRDsample, "qlm_corpus")

  # Load quanteda
  suppressPackageStartupMessages(library(quanteda))

  # Subset the corpus
  subset_corpus <- data_corpus_LMRDsample[1:3]

  # Should still have qlm_corpus class after subsetting
  expect_s3_class(subset_corpus, "qlm_corpus")
  expect_s3_class(subset_corpus, "corpus")
  expect_equal(length(subset_corpus), 3)

  # Unload quanteda for other tests
  try(unloadNamespace("quanteda"), silent = TRUE)
})

test_that("qlm_corpus print output format is correct", {
  skip_if(isNamespaceLoaded("quanteda"), "quanteda is already loaded")

  # Load the data
  data("data_corpus_LMRDsample", package = "quallmer")

  # Test with full corpus
  output_full <- capture.output(print(data_corpus_LMRDsample))

  expect_true(any(grepl("Corpus consisting of 200 documents", output_full)))
  expect_true(any(grepl("\\[ \\.\\.\\..*more document", output_full)))

  # Test with single document
  output_single <- capture.output(print(data_corpus_LMRDsample[1]))

  expect_true(any(grepl("Corpus consisting of 1 document\\.", output_single)))
  expect_false(any(grepl("documents", output_single)))  # Should be singular
})

test_that("qlm_corpus with minimal docvars prints correctly", {
  skip_if(isNamespaceLoaded("quanteda"), "quanteda is already loaded")

  # Load the data
  data("data_corpus_LMRDsample", package = "quallmer")

  # Test that corpus with only internal docvars doesn't show Docvars line
  # We can simulate this by manipulating an existing corpus's docvars
  test_corpus <- data_corpus_LMRDsample[1:2]

  # Save original docvars
  orig_docvars <- attr(test_corpus, "docvars")

  # Set docvars to only have internal columns
  attr(test_corpus, "docvars") <- orig_docvars[, c("docname_", "docid_", "segid_"), drop = FALSE]

  output <- capture.output(print(test_corpus))

  expect_true(any(grepl("Corpus consisting of 2 documents", output)))
  # Should not show Docvars line when only internal columns exist
  expect_false(any(grepl("^Docvars:", output)))

  # Restore original docvars
  attr(test_corpus, "docvars") <- orig_docvars
})