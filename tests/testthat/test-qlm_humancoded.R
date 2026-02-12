test_that("qlm_humancoded creates object with correct structure", {
  data <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  result <- qlm_humancoded(data, name = "test_coder")

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

test_that("qlm_humancoded validates inputs", {
  # Should error with non-data.frame
  expect_error(
    qlm_humancoded(list(a = 1)),
    "must be a data frame"
  )

  # Should error if .id column missing
  data_no_id <- data.frame(category = rep(c("A", "B"), 5))
  expect_error(
    qlm_humancoded(data_no_id),
    "must contain.*\\.id.*column"
  )
})

test_that("qlm_humancoded accepts custom codebook", {
  data <- data.frame(.id = 1:10, sentiment = rep(c("pos", "neg"), 5))

  codebook <- list(
    name = "Sentiment Coding",
    instructions = "Code as positive or negative"
  )

  result <- qlm_humancoded(data, codebook = codebook)

  codebook_attr <- attr(result, "codebook")
  expect_equal(codebook_attr$name, "Sentiment Coding")
  expect_equal(codebook_attr$instructions, "Code as positive or negative")
  expect_null(codebook_attr$schema)  # Always NULL for human coding
})

test_that("qlm_humancoded accepts custom metadata", {
  data <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  metadata <- list(
    coder_name = "Alice",
    coder_id = "A001",
    training = "2 hours"
  )

  result <- qlm_humancoded(data, metadata = metadata)

  meta_attr <- attr(result, "meta")
  expect_equal(meta_attr$user$coder_name, "Alice")
  expect_equal(meta_attr$user$coder_id, "A001")
  expect_equal(meta_attr$user$training, "2 hours")
  expect_equal(meta_attr$object$source, "human")  # Always added
})

test_that("qlm_humancoded works with qlm_compare", {
  skip_if_not_installed("irr")

  data1 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))
  data2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  human1 <- qlm_humancoded(data1, name = "Coder_A")
  human2 <- qlm_humancoded(data2, name = "Coder_B")

  comparison <- qlm_compare(human1, human2, by = category, level = "nominal")

  expect_true(inherits(comparison, "qlm_comparison"))
  expect_true(is.data.frame(comparison))

  # Extract percent agreement from data frame
  pa_row <- comparison[comparison$measure == "percent_agreement", ]
  expect_equal(pa_row$value, 1.0)
})

test_that("qlm_humancoded works with qlm_validate", {
  skip_if_not_installed("yardstick")

  data1 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))
  data2 <- data.frame(.id = 1:10, category = rep(c("A", "B"), 5))

  human1 <- qlm_humancoded(data1, name = "Coder_A")
  human2 <- qlm_humancoded(data2, name = "Coder_B")

  # qlm_humancoded objects don't have codebook, so level must be explicit
  validation <- qlm_validate(human1, gold = human2, by = category, level = "nominal")

  expect_true(inherits(validation, "qlm_validation"))
  expect_true(is.data.frame(validation))

  # Extract accuracy from data frame
  acc_row <- validation[validation$measure == "accuracy", ]
  expect_equal(acc_row$value, 1.0)
})

test_that("print.qlm_coded distinguishes human vs LLM coding", {
  data <- data.frame(.id = 1:5, category = c("A", "B", "A", "B", "A"))

  human <- qlm_humancoded(data, name = "test_coder")

  output <- capture.output(print(human))

  expect_true(any(grepl("Source.*Human coder", output)))
  expect_false(any(grepl("Model:", output)))
})


test_that("as_qlm_coded and qlm_humancoded store notes in metadata", {
  data <- data.frame(.id = 1:5, category = c("A", "B", "A", "B", "A"))

  # Test as_qlm_coded with notes
  coded_with_notes <- as_qlm_coded(
    data,
    name = "test_coder",
    notes = "Pilot coding for training purposes"
  )

  meta_attr <- attr(coded_with_notes, "meta")
  expect_equal(meta_attr$user$notes, "Pilot coding for training purposes")

  # Test print output includes notes
  output <- capture.output(print(coded_with_notes))
  expect_true(any(grepl("Notes:.*Pilot coding for training purposes", output)))

  # Test qlm_humancoded with notes
  human_with_notes <- qlm_humancoded(
    data,
    name = "test_coder",
    notes = "Final coding after training"
  )

  meta_attr2 <- attr(human_with_notes, "meta")
  expect_equal(meta_attr2$user$notes, "Final coding after training")

  output2 <- capture.output(print(human_with_notes))
  expect_true(any(grepl("Notes:.*Final coding after training", output2)))
})
