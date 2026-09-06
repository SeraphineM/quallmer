# One validator for both coding paths: what a model returns is checked
# against the codebook schema before ellmer's converter can coerce a wrong
# type to NA, drop an extra property, or collapse a missing array and a
# valid empty one into the same cell (#140).

test_schema <- function() {
  ellmer::type_object(
    score = ellmer::type_number("Score"),
    lab = ellmer::type_enum(values = c("pos", "neg"))
  )
}

# validate_against_type() -----------------------------------------------------

test_that("validate_against_type accepts a conforming object", {
  schema <- test_schema()
  value <- list(score = 1, lab = "pos")

  expect_equal(validate_against_type(value, schema, "$"), value)
})

test_that("validate_against_type reports missing, wrong-type and bad-enum values", {
  schema <- test_schema()

  expect_error(
    validate_against_type(list(lab = "pos"), schema, "$"),
    "\\$\\.score is required but missing",
  )
  expect_error(
    validate_against_type(list(score = "high", lab = "pos"), schema, "$"),
    "\\$\\.score must be a number"
  )
  expect_error(
    validate_against_type(list(score = 1, lab = "maybe"), schema, "$"),
    "\\$\\.lab must be one of: pos, neg"
  )
  expect_error(
    validate_against_type(list(score = 1, lab = "pos", extra = 2), schema, "$"),
    "\\$ has unexpected property: extra"
  )
  expect_error(
    validate_against_type(list(score = NULL, lab = "pos"), schema, "$"),
    "\\$\\.score is required and cannot be null"
  )
})

test_that("validate_against_type does not coerce scalars", {
  type <- ellmer::type_object(
    n = ellmer::type_number("N"),
    i = ellmer::type_integer("I"),
    b = ellmer::type_boolean("B"),
    s = ellmer::type_string("S")
  )
  ok <- list(n = 2.5, i = 3, b = TRUE, s = "x")
  expect_equal(validate_against_type(ok, type, "$"), ok)

  # A number sent as a string is what a schema-ignoring endpoint produces
  expect_error(
    validate_against_type(modifyList(ok, list(n = "2.5")), type, "$"),
    "\\$\\.n must be a number"
  )
  expect_error(
    validate_against_type(modifyList(ok, list(i = "3")), type, "$"),
    "\\$\\.i must be a integer"
  )
  expect_error(
    validate_against_type(modifyList(ok, list(b = "true")), type, "$"),
    "\\$\\.b must be a boolean"
  )
  expect_error(
    validate_against_type(modifyList(ok, list(b = 1)), type, "$"),
    "\\$\\.b must be a boolean"
  )
  expect_error(
    validate_against_type(modifyList(ok, list(s = 1)), type, "$"),
    "\\$\\.s must be a string"
  )
  # A vector is not a scalar, whatever its type
  expect_error(
    validate_against_type(modifyList(ok, list(s = c("a", "b"))), type, "$"),
    "\\$\\.s must be a string"
  )
})

test_that("validate_against_type reports the JSON path of a nested failure", {
  type <- ellmer::type_object(
    claims = ellmer::type_array(
      ellmer::type_object(salience = ellmer::type_number("Salience"))
    )
  )
  value <- list(claims = list(
    list(salience = 1),
    list(salience = 2),
    list(salience = "high")
  ))

  expect_error(
    validate_against_type(value, type, "$"),
    "\\$\\.claims\\[3\\]\\.salience must be a number"
  )
})

test_that("validate_against_type distinguishes arrays from objects", {
  type <- ellmer::type_object(xs = ellmer::type_array(ellmer::type_string()))

  expect_error(
    validate_against_type(list(xs = list(a = "one")), type, "$"),
    "\\$\\.xs must be a JSON array"
  )
  expect_equal(
    validate_against_type(list(xs = list("one", "two")), type, "$"),
    list(xs = list("one", "two"))
  )
  # An empty array is legal
  expect_equal(validate_against_type(list(xs = list()), type, "$"), list(xs = list()))
})

test_that("validate_against_type keeps a missing required array, an empty one and an object apart", {
  # The three cases the issue opens with: after conversion, all three would
  # be an empty list-column cell
  type <- ellmer::type_object(labels = ellmer::type_array(ellmer::type_string()))
  parse <- function(text) jsonlite::fromJSON(text, simplifyVector = FALSE)

  expect_equal(
    validate_against_type(parse('{"labels": []}'), type, "$"),
    list(labels = list())
  )
  expect_error(
    validate_against_type(parse("{}"), type, "$"),
    "\\$\\.labels is required but missing"
  )
  expect_error(
    validate_against_type(parse('{"labels": {}}'), type, "$"),
    "\\$\\.labels must be a JSON array"
  )
  expect_error(
    validate_against_type(parse('{"labels": null}'), type, "$"),
    "\\$\\.labels is required and cannot be null"
  )
})

test_that("validate_against_type accepts an empty object when nothing is required", {
  type <- ellmer::type_object(a = ellmer::type_string("A", required = FALSE))
  # jsonlite parses "{}" to a *named* empty list, not an unnamed one
  value <- jsonlite::fromJSON("{}", simplifyVector = FALSE)

  expect_named(value)
  expect_equal(validate_against_type(value, type, "$"), list(a = NULL))
})

test_that("validate_against_type keeps an optional property sent as null", {
  # Regression: `output[[property]] <- NULL` deleted the property, so an
  # explicit null vanished from the object while an absent one stayed
  type <- ellmer::type_object(
    a = ellmer::type_string("A", required = FALSE),
    b = ellmer::type_string("B")
  )
  absent <- validate_against_type(list(b = "x"), type, "$")
  null <- validate_against_type(list(a = NULL, b = "x"), type, "$")

  expect_identical(names(absent), c("a", "b"))
  expect_identical(names(null), c("a", "b"))
  expect_identical(absent, null)
  expect_null(null$a)
})

test_that("validate_against_type keeps a null nested object and array", {
  type <- ellmer::type_object(
    detail = ellmer::type_object(
      x = ellmer::type_string("X"),
      .required = FALSE
    ),
    tags = ellmer::type_array(ellmer::type_string(), required = FALSE),
    n = ellmer::type_integer("N")
  )
  value <- validate_against_type(list(detail = NULL, tags = NULL, n = 1), type, "$")
  expect_identical(names(value), c("detail", "tags", "n"))
  expect_null(value$detail)
  expect_null(value$tags)

  # And a required nested object cannot be null or empty of its required field
  strict <- ellmer::type_object(detail = ellmer::type_object(x = ellmer::type_string("X")))
  expect_error(
    validate_against_type(list(detail = NULL), strict, "$"),
    "\\$\\.detail is required and cannot be null"
  )
  expect_error(
    validate_against_type(list(detail = jsonlite::fromJSON("{}", simplifyVector = FALSE)), strict, "$"),
    "\\$\\.detail\\.x is required but missing"
  )
})

test_that("validate_against_type keeps extras when additional_properties is TRUE", {
  withr::local_options(lifecycle_verbosity = "quiet")
  type <- ellmer::type_object(
    a = ellmer::type_string("A"),
    .additional_properties = TRUE
  )

  expect_equal(
    validate_against_type(list(a = "x", b = 2), type, "$"),
    list(a = "x", b = 2)
  )
})

test_that("validate_against_type enforces integer bounds", {
  type <- ellmer::type_object(n = ellmer::type_integer("N"))

  expect_equal(validate_against_type(list(n = 3), type, "$"), list(n = 3))
  expect_error(validate_against_type(list(n = 3.5), type, "$"), "must be a integer")
  expect_error(validate_against_type(list(n = 1e10), type, "$"), "must be a integer")
})

test_that("validate_against_type reads ellmer's parsed JSON as it reads jsonlite's", {
  # ellmer parses a structured response lazily from its text; the same
  # object and array distinctions must hold for either source
  type <- ellmer::type_object(
    labels = ellmer::type_array(ellmer::type_string()),
    meta = ellmer::type_object(k = ellmer::type_string("K", required = FALSE),
                               .required = FALSE)
  )
  text <- '{"labels": [], "meta": {}}'
  from_ellmer <- json_turn(string = text)@contents[[1]]@parsed
  from_jsonlite <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  expect_identical(from_ellmer, from_jsonlite)
  expect_equal(
    validate_against_type(from_ellmer, type, "$"),
    list(labels = list(), meta = list(k = NULL))
  )
})

# validate_structured_value() -------------------------------------------------

test_that("validate_structured_value wraps the outcome for a caller", {
  schema <- test_schema()
  good <- validate_structured_value(list(score = 1, lab = "pos"), schema)
  expect_true(good$ok)
  expect_equal(good$value, list(score = 1, lab = "pos"))

  bad <- validate_structured_value(list(score = "1", lab = "pos"), schema)
  expect_false(bad$ok)
  expect_match(bad$error, "^\\$\\.score must be a number")
  expect_null(bad$value)
})

# check_coding_schema() -------------------------------------------------------

test_that("check_coding_schema accepts every supported type, nested", {
  schema <- ellmer::type_object(
    s = ellmer::type_string("S"),
    b = ellmer::type_boolean("B"),
    i = ellmer::type_integer("I"),
    n = ellmer::type_number("N"),
    e = ellmer::type_enum(values = c("a", "b")),
    xs = ellmer::type_array(ellmer::type_object(
      inner = ellmer::type_array(ellmer::type_enum(values = c("x")))
    ))
  )
  expect_invisible(check_coding_schema(schema))
  expect_identical(check_coding_schema(schema), schema)
})

test_that("check_coding_schema refuses a missing schema and a non-object root", {
  expect_error(check_coding_schema(NULL), "has no schema")
  expect_error(
    check_coding_schema(ellmer::type_array(ellmer::type_string())),
    "must be a `type_object\\(\\)` at the root"
  )
  expect_error(
    check_coding_schema(ellmer::type_string()),
    "must be a `type_object\\(\\)` at the root"
  )
})

test_that("check_coding_schema names the path of an opaque type", {
  opaque <- ellmer::type_from_schema('{"type": "object", "properties": {"a": {"type": "string"}}}')
  schema <- ellmer::type_object(
    ok = ellmer::type_string("OK"),
    items = ellmer::type_array(ellmer::type_object(deep = opaque))
  )
  expect_error(
    check_coding_schema(schema),
    "cannot be validated: `\\$\\.items\\[\\]\\.deep \\(TypeJsonSchema\\)`"
  )
  expect_error(
    check_coding_schema(ellmer::type_object(a = opaque, b = opaque)),
    "`\\$\\.a \\(TypeJsonSchema\\)` and `\\$\\.b \\(TypeJsonSchema\\)`"
  )
})

test_that("qlm_code refuses an unsupported schema before building a chat", {
  codebook <- qlm_codebook(
    "Array root", "Label each.",
    ellmer::type_array(ellmer::type_string())
  )
  expect_error(
    qlm_code("text", codebook, model = "openai/gpt-4.1-mini"),
    "must be a `type_object\\(\\)` at the root"
  )
})
