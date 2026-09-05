test_that("qlm_codebook creates a valid codebook object", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Score"),
    explanation = ellmer::type_string("Explanation")
  )

  codebook <- qlm_codebook(
    name = "Test Codebook",
    instructions = "Rate the test.",
    schema = type_obj
  )

  expect_true(is.list(codebook))
  expect_equal(codebook$name, "Test Codebook")
  expect_equal(codebook$instructions, "Rate the test.")
  expect_equal(codebook$schema, type_obj)
  expect_null(codebook$role)
  expect_equal(codebook$input_type, "text")
})


test_that("qlm_codebook has dual class inheritance", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(
    score = ellmer::type_number("Score")
  )

  codebook <- qlm_codebook(
    name = "Test",
    instructions = "Test prompt",
    schema = type_obj
  )

  # Should have both classes
  expect_true(inherits(codebook, "qlm_codebook"))
  expect_true(inherits(codebook, "task"))

  # Class order matters for method dispatch
  expect_equal(class(codebook), c("qlm_codebook", "task"))
})


test_that("qlm_codebook validates input_type", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Valid input types
  cb_text <- qlm_codebook("Test", "Prompt", type_obj, input_type = "text")
  expect_equal(cb_text$input_type, "text")

  cb_image <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")
  expect_equal(cb_image$input_type, "image")

  cb_audio <- qlm_codebook("Test", "Prompt", type_obj, input_type = "audio")
  expect_equal(cb_audio$input_type, "audio")

  # Invalid input type should error
  expect_error(
    qlm_codebook("Test", "Prompt", type_obj, input_type = "invalid"),
    "'arg' should be one of"
  )
})


test_that("qlm_codebook stores image_file_resize on image codebooks only (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # The default is resolved and stored, so every image codebook says what
  # resolution it codes at
  cb_image <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")
  expect_identical(cb_image$image_file_resize, "high")

  cb_low <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                         image_file_resize = "low")
  expect_identical(cb_low$image_file_resize, "low")

  # A text codebook carries no value at all
  cb_text <- qlm_codebook("Test", "Prompt", type_obj)
  expect_false("image_file_resize" %in% names(cb_text))
  expect_null(cb_text$image_file_resize)

  # and refuses one rather than storing it unused
  expect_error(
    qlm_codebook("Test", "Prompt", type_obj, image_file_resize = "high"),
    "applies to image codebooks only"
  )
})


test_that("qlm_codebook validates image_file_resize (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  make <- function(resize) {
    qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                 image_file_resize = resize)
  }

  for (keyword in c("high", "low", "none")) {
    expect_identical(make(keyword)$image_file_resize, keyword)
  }
  # magick geometry strings, as ellmer accepts
  for (geometry in c("1024x1024>", "50%", "800x", "x600!", "1024x1024",
                     "300x200+10+10")) {
    expect_identical(make(geometry)$image_file_resize, geometry)
  }

  for (bad in list("medium", "", NA_character_, 512, c("low", "high"),
                   "big>", TRUE)) {
    expect_error(make(bad), "must be one of")
  }
})


test_that("codebooks saved before image_file_resize existed read as \"low\" (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # An image codebook from before the field existed was coded at ellmer's
  # default, so that is what it is read as, not the current default
  old_image <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")
  old_image$image_file_resize <- NULL
  expect_identical(as_qlm_codebook(old_image)$image_file_resize, "low")

  # A value that is present is kept
  cb_high <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")
  expect_identical(as_qlm_codebook(cb_high)$image_file_resize, "high")

  # A text codebook gains nothing
  cb_text <- qlm_codebook("Test", "Prompt", type_obj)
  expect_null(as_qlm_codebook(cb_text)$image_file_resize)

  # task() objects predate the field too
  old_task <- suppressWarnings(
    task("Test", "Prompt", type_obj, input_type = "image")
  )
  expect_identical(as_qlm_codebook(old_task)$image_file_resize, "low")
  old_text_task <- suppressWarnings(task("Test", "Prompt", type_obj))
  expect_null(as_qlm_codebook(old_text_task)$image_file_resize)
})


test_that("print.qlm_codebook shows the image resize setting (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  cb_image <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                           image_file_resize = "1024x1024>")
  output <- capture.output(print(cb_image))
  expect_true(any(grepl("Image resize: 1024x1024>", output, fixed = TRUE)))

  cb_text <- qlm_codebook("Test", "Prompt", type_obj)
  output <- capture.output(print(cb_text))
  expect_false(any(grepl("Image resize", output)))
})


test_that("as_qlm_codebook converts task objects", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Suppress deprecation warning for this test
  withr::local_options(lifecycle_verbosity = "quiet")

  # Create old-style task object
  old_task <- task(
    name = "Old Task",
    system_prompt = "Old prompt",
    type_def = type_obj
  )

  # Convert to qlm_codebook
  converted <- as_qlm_codebook(old_task)

  # Should now have both classes
  expect_true(inherits(converted, "qlm_codebook"))
  expect_true(inherits(converted, "task"))

  # Should preserve content
  expect_equal(converted$name, "Old Task")
  expect_equal(converted$system_prompt, "Old prompt")
  expect_equal(converted$type_def, type_obj)
})


test_that("as_qlm_codebook is idempotent for qlm_codebook objects", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Converting a qlm_codebook should return it unchanged
  converted <- as_qlm_codebook(codebook)

  expect_identical(converted, codebook)
})


test_that("print.qlm_codebook works", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  codebook <- qlm_codebook(
    name = "Test Codebook",
    instructions = "This is a test prompt for printing",
    schema = type_obj,
    role = "You are an expert annotator."
  )

  # Capture print output
  output <- capture.output(print(codebook))

  expect_true(any(grepl("quallmer codebook", output)))
  expect_true(any(grepl("Test Codebook", output)))
  expect_true(any(grepl("Input type", output)))
  expect_true(any(grepl("Role", output)))
  expect_true(any(grepl("Instructions", output)))
})


test_that("qlm_codebook role parameter works correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Without role
  cb_no_role <- qlm_codebook(
    name = "Test",
    instructions = "Rate the text.",
    schema = type_obj
  )

  expect_null(cb_no_role$role)
  expect_equal(cb_no_role$instructions, "Rate the text.")

  # With role
  cb_with_role <- qlm_codebook(
    name = "Test",
    instructions = "Rate the text.",
    schema = type_obj,
    role = "You are an expert."
  )

  expect_equal(cb_with_role$role, "You are an expert.")
  expect_equal(cb_with_role$instructions, "Rate the text.")
  # Note: system_prompt is constructed in qlm_code(), not stored in codebook
})


test_that("predefined codebooks are qlm_codebook objects", {
  skip_if_not_installed("ellmer")

  # Predefined codebook should be qlm_codebook object
  expect_true(inherits(data_codebook_sentiment, "qlm_codebook"))
})

test_that("qlm_codebook levels accept variables nested inside a type_array", {
  # The case from #131: one document yields many rated items, so the schema
  # is an array of per-item objects and the variables that carry levels sit
  # one array deep.
  schema <- type_object(
    assessments = type_array(
      description = "One entry per item.",
      items = type_object(
        item_id = type_string("Identifier."),
        importance = type_integer("0 = absent, 1 = mild, 2 = high."),
        position = type_integer("1-7.")
      )
    )
  )

  cb <- qlm_codebook(
    name = "nested_levels",
    instructions = "Score each item.",
    schema = schema,
    levels = list(importance = "ordinal", position = "ordinal")
  )

  expect_equal(cb$levels, list(importance = "ordinal", position = "ordinal"))
  expect_equal(qlm_levels(cb), list(importance = "ordinal", position = "ordinal"))

  # The array itself and a mix of depths are both declarable
  cb2 <- qlm_codebook(
    name = "mixed",
    instructions = "Score each item.",
    schema = schema,
    levels = list(assessments = "nominal", item_id = "nominal", importance = "ordinal")
  )
  expect_equal(names(cb2$levels), c("assessments", "item_id", "importance"))
})

test_that("qlm_codebook levels accept variables inside a nested type_object", {
  schema <- type_object(
    summary = type_string("Summary."),
    scores = type_object(
      tone = type_number("Tone from -1 to 1."),
      confidence = type_integer("1-5.")
    )
  )

  cb <- qlm_codebook(
    name = "nested_object",
    instructions = "Rate.",
    schema = schema,
    levels = list(summary = "nominal", tone = "interval", confidence = "ordinal")
  )
  expect_equal(cb$levels[["tone"]], "interval")
})

test_that("qlm_codebook levels resolve through arrays nested in arrays", {
  schema <- type_object(
    documents = type_array(
      items = type_object(
        sentences = type_array(
          items = type_object(polarity = type_integer("-1, 0, 1."))
        )
      )
    )
  )

  cb <- qlm_codebook(
    name = "deep",
    instructions = "Rate.",
    schema = schema,
    levels = list(polarity = "ordinal")
  )
  expect_equal(cb$levels[["polarity"]], "ordinal")
})

test_that("qlm_codebook levels still reject names absent at every depth", {
  schema <- type_object(
    assessments = type_array(
      items = type_object(importance = type_integer("0-2."))
    )
  )

  expect_error(
    qlm_codebook(
      name = "x", instructions = "x", schema = schema,
      levels = list(importance = "ordinal", salience = "ordinal")
    ),
    "not found in schema.*salience"
  )

  # The listing of available names now includes nested ones
  expect_error(
    qlm_codebook(
      name = "x", instructions = "x", schema = schema,
      levels = list(salience = "ordinal")
    ),
    "importance"
  )
})

test_that("qlm_codebook levels refuse a name that occurs at more than one depth", {
  schema <- type_object(
    score = type_number("Overall score."),
    items = type_array(
      items = type_object(
        score = type_integer("Per-item score."),
        label = type_string("Label.")
      )
    )
  )

  expect_error(
    qlm_codebook(
      name = "x", instructions = "x", schema = schema,
      levels = list(score = "interval")
    ),
    "more than once in the schema"
  )

  # Declaring only the unambiguous names is fine
  cb <- qlm_codebook(
    name = "x", instructions = "x", schema = schema,
    levels = list(label = "nominal")
  )
  expect_equal(cb$levels, list(label = "nominal"))
})

test_that("qlm_codebook levels are checked when the schema root is a type_array", {
  schema <- type_array(
    items = type_object(score = type_integer("1-5."), label = type_string("Label."))
  )

  # Previously the check was skipped for any root that was not a type_object
  expect_error(
    qlm_codebook("x", "x", schema, levels = list(typo = "ordinal")),
    "not found in schema.*typo"
  )

  cb <- qlm_codebook("x", "x", schema, levels = list(score = "ordinal"))
  expect_equal(cb$levels, list(score = "ordinal"))

  # A root with no named properties at all says so rather than listing nothing
  expect_error(
    qlm_codebook("x", "x", type_array(items = type_string()),
                 levels = list(score = "ordinal")),
    "declares no named properties"
  )
})

test_that("nested levels reach qlm_compare through an unnested, re-wrapped table", {
  # End to end for #131: the codebook declares levels for variables one array
  # deep; the coded output is unnested to one row per document-item, given an
  # identifier unique per item, and re-wrapped with the codebook. qlm_compare()
  # must then pick up "ordinal" for `importance` without a `level` argument.
  cb <- qlm_codebook(
    name = "nested_levels",
    instructions = "Score each item.",
    schema = type_object(
      assessments = type_array(
        items = type_object(
          item_id = type_string("Identifier."),
          importance = type_integer("0-2.")
        )
      )
    ),
    levels = list(importance = "ordinal")
  )

  # Two documents; d1 has two items, d2 has one. The unit id is document-item.
  unnested_a <- data.frame(
    unit = c("d1.econ", "d1.env", "d2.econ"),
    importance = c(2L, 1L, 0L)
  )
  unnested_b <- data.frame(
    unit = c("d1.econ", "d1.env", "d2.econ"),
    importance = c(2L, 0L, 0L)
  )

  coder_a <- as_qlm_coded(unnested_a, id = "unit", name = "A", codebook = cb)
  coder_b <- as_qlm_coded(unnested_b, id = "unit", name = "B", codebook = cb)

  res <- as.data.frame(qlm_compare(coder_a, coder_b, by = "importance"))

  expect_true(all(res$level == "ordinal"))
  expect_equal(res$value[res$measure == "percent_agreement"], 2 / 3)
})
