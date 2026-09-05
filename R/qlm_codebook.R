#' Define a qualitative codebook
#'
#' Creates a codebook definition for use with [qlm_code()]. A codebook specifies
#' what information to extract from input data, including the instructions
#' that guide the LLM and the structured output schema.
#'
#' This function replaces [task()], which is now deprecated. The returned object
#' has dual class inheritance (`c("qlm_codebook", "task")`) to maintain
#' backward compatibility.
#'
#' @param name Name of the codebook (character).
#' @param instructions Instructions to guide the model in performing the coding task.
#' @param schema Structured output definition, e.g., created by
#'   [type_object()], [type_array()], or [type_enum()].
#' @param role Optional role description for the model (e.g., "You are an expert
#'   annotator"). If provided, this will be prepended to the instructions when
#'   creating the system prompt.
#' @param input_type Type of input data: `"text"` (default) or `"image"`.
#' @param image_file_resize How image files are resized before they are sent,
#'   for `input_type = "image"` only; setting it on a text codebook is an
#'   error. One of `"high"` (the default: fit within 2000x768 or 768x2000
#'   pixels, whichever suits the orientation), `"low"` (fit within 512x512),
#'   `"none"` (send the file as it is), or a \pkg{magick} geometry string
#'   such as `"1024x1024>"`. Anything other than `"none"` needs the
#'   \pkg{magick} package, and [qlm_code()] says so if it is missing. The
#'   value is stored on the codebook, so replications and backfills code at
#'   the same resolution and [qlm_trail()] records it. It applies to file
#'   paths only: an element of `x` that is a URL is fetched by the provider
#'   as it is. Codebooks saved before this field existed, including
#'   [task()] objects, are read as `"low"`, the resolution they were coded
#'   at, rather than the current default. See [ellmer::content_image_file()].
#' @param levels Optional named list specifying measurement levels for each
#'   variable in the schema. Names should match schema property names. Values
#'   should be one of `"nominal"`, `"ordinal"`, `"interval"`, or `"ratio"`.
#'   If `NULL` (default), levels are auto-detected from schema types using the
#'   following mapping: `type_boolean` and `type_enum` = nominal, `type_string`
#'   = nominal, `type_integer` = ordinal, `type_number` = interval.
#'
#'   Names may refer to properties at any depth of the schema, including those
#'   inside a nested [type_object()] or the items of a [type_array()]. A level
#'   declared for a nested variable describes that variable in a table
#'   unnested to one row per item. [qlm_compare()] and [qlm_validate()] merge
#'   on `.id`, so such a table needs an identifier that is unique per item
#'   (a document-item key, not the document's `.id` alone) and must be
#'   re-wrapped with [as_qlm_coded()], passing this codebook as `codebook`,
#'   for the declared levels to be found. Because `levels` is a flat list, a
#'   name that occurs at more than one place in the schema cannot be declared
#'   and is an error; rename the properties to make them distinct.
#'
#' @return A codebook object (a list with class `c("qlm_codebook", "task")`)
#'   containing the codebook definition. Use with [qlm_code()] to apply the
#'   codebook to data.
#'
#' @seealso [qlm_code()] for applying codebooks to data,
#'   [data_codebook_sentiment] for a predefined codebook example,
#'   [task()] for the deprecated function.
#'
#' @examples
#' # Define a custom codebook
#' my_codebook <- qlm_codebook(
#'   name = "Sentiment",
#'   instructions = "Rate the sentiment from -1 (negative) to 1 (positive).",
#'   schema = type_object(
#'     score = type_number("Sentiment score from -1 to 1"),
#'     explanation = type_string("Brief explanation")
#'   )
#' )
#'
#' # With a role
#' my_codebook_role <- qlm_codebook(
#'   name = "Sentiment",
#'   instructions = "Rate the sentiment from -1 (negative) to 1 (positive).",
#'   schema = type_object(
#'     score = type_number("Sentiment score from -1 to 1"),
#'     explanation = type_string("Brief explanation")
#'   ),
#'   role = "You are an expert sentiment analyst."
#' )
#'
#' # With explicit measurement levels
#' my_codebook_levels <- qlm_codebook(
#'   name = "Sentiment",
#'   instructions = "Rate the sentiment from -1 (negative) to 1 (positive).",
#'   schema = type_object(
#'     score = type_number("Sentiment score from -1 to 1"),
#'     explanation = type_string("Brief explanation")
#'   ),
#'   levels = list(score = "interval", explanation = "nominal")
#' )
#'
#' \dontrun{
#' # Use with qlm_code() (requires API key)
#' texts <- c("I love this!", "This is terrible.")
#' coded <- qlm_code(texts, my_codebook, model = "openai/gpt-4o-mini")
#' coded
#' }
#'
#' @export
qlm_codebook <- function(name, instructions, schema, role = NULL,
                         input_type = c("text", "image"), levels = NULL,
                         image_file_resize = NULL) {
  input_type <- match.arg(input_type)
  image_file_resize <- check_image_file_resize(image_file_resize, input_type)

  # Auto-detect levels if not provided
  if (is.null(levels)) {
    levels <- auto_detect_levels(schema)
  } else {
    validate_levels(levels, schema)
  }

  codebook <- list(
    name = name,
    instructions = instructions,
    schema = schema,
    role = role,
    input_type = input_type,
    levels = levels
  )
  # Stored on image codebooks only, and always resolved there, so that every
  # image codebook says what resolution it codes at and a text codebook
  # carries no meaningless value (#177)
  if (input_type == "image") {
    codebook$image_file_resize <- image_file_resize
  }

  structure(
    codebook,
    class = c("qlm_codebook", "task")  # Dual class for backward compatibility
  )
}


#' Validate the image resize setting of a codebook
#'
#' Accepts what [ellmer::content_image_file()] accepts as `resize`: one of
#' the keywords or a magick geometry string. `NULL` on an image codebook
#' resolves to the default; on any other codebook the setting is refused
#' rather than stored and ignored.
#'
#' @param x The value to check.
#' @param input_type The codebook's input type.
#' @param call The calling environment, for the error.
#'
#' @return The resolved value: a string for image codebooks, `NULL`
#'   otherwise.
#' @keywords internal
#' @noRd
check_image_file_resize <- function(x, input_type, call = rlang::caller_env()) {
  if (input_type != "image") {
    if (!is.null(x)) {
      cli::cli_abort(c(
        "{.arg image_file_resize} applies to image codebooks only.",
        "x" = "This codebook has {.code input_type = \"{input_type}\"}."
      ), call = call)
    }
    return(NULL)
  }
  if (is.null(x)) {
    return("high")
  }
  keywords <- c("high", "low", "none")
  # A magick geometry: digits with an optional x, a scale or fit flag, and
  # an optional offset, e.g. "1024x1024>", "50%", "800x", "x600!"
  geometry <- "^[0-9]*%?(x[0-9]*%?)?[!<>^@]*(\\+[0-9]+\\+[0-9]+)?$"
  ok <- is.character(x) && length(x) == 1L && !is.na(x) &&
    (x %in% keywords || (grepl("[0-9]", x) && grepl(geometry, x)))
  if (!ok) {
    cli::cli_abort(c(
      "{.arg image_file_resize} must be one of {.val {keywords}} or a magick geometry string.",
      "i" = "For example {.val 1024x1024>} fits each image within 1024 pixels a side.",
      "i" = "See {.fn ellmer::content_image_file}."
    ), call = call)
  }
  x
}


#' Convert objects to qlm_codebook
#'
#' Generic function to convert objects to qlm_codebook class.
#'
#' @param x An object to convert to qlm_codebook.
#' @param ... Additional arguments passed to methods.
#'
#' @return A qlm_codebook object.
#' @keywords internal
#' @export
as_qlm_codebook <- function(x, ...) {
  UseMethod("as_qlm_codebook")
}


#' @rdname as_qlm_codebook
#' @keywords internal
#' @export
as_qlm_codebook.task <- function(x, ...) {
  # If already a qlm_codebook, return as-is
  if (inherits(x, "qlm_codebook")) {
    return(x)
  }

  # Convert task to qlm_codebook by adding the class
  fill_codebook_fields(structure(
    x,
    class = c("qlm_codebook", "task")
  ))
}


#' @rdname as_qlm_codebook
#' @keywords internal
#' @export
as_qlm_codebook.qlm_codebook <- function(x, ...) {
  fill_codebook_fields(x)
}


#' Fill fields a saved codebook predates
#'
#' Image codebooks made before `image_file_resize` existed were coded at
#' ellmer's default of `"low"`. That is what they are read as, not the
#' current default, so that [qlm_replicate()] on such a run measures what the
#' original run measured (#177).
#'
#' @param x A qlm_codebook.
#' @return `x` with every field present.
#' @keywords internal
#' @noRd
fill_codebook_fields <- function(x) {
  if (identical(x$input_type, "image") && is.null(x$image_file_resize)) {
    x$image_file_resize <- "low"
  }
  x
}


#' Print a qlm_codebook object
#'
#' @param x A qlm_codebook object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects (printing to console).
#' @keywords internal
#' @export
print.qlm_codebook <- function(x, ...) {
  cat("quallmer codebook:", x$name, "\n")
  cat("  Input type:   ", x$input_type, "\n", sep = "")
  if (!is.null(x$image_file_resize)) {
    cat("  Image resize: ", x$image_file_resize, "\n", sep = "")
  }
  if (!is.null(x$role)) {
    cat("  Role:         ", substr(x$role, 1, 60),
        if (nchar(x$role) > 60) "..." else "", "\n", sep = "")
  }
  cat("  Instructions: ", substr(x$instructions, 1, 60),
      if (nchar(x$instructions) > 60) "..." else "", "\n", sep = "")
  cat("  Output schema:", class(x$schema)[1], "\n", sep = "")

  # Print levels if available
  if (!is.null(x$levels) && length(x$levels) > 0) {
    cat("  Levels:\n")
    for (i in seq_along(x$levels)) {
      cat("    ", names(x$levels)[i], ": ", x$levels[[i]], "\n", sep = "")
    }
  }

  invisible(x)
}


#' Auto-detect measurement levels from schema types
#'
#' Maps ellmer schema types to measurement levels using standard conventions.
#'
#' @param schema An ellmer schema object (TypeObject)
#' @return Named character vector of levels
#' @keywords internal
#' @noRd
auto_detect_levels <- function(schema) {
  # Handle NULL schema
  if (is.null(schema)) {
    return(NULL)
  }

  # Extract properties from schema
  # For TypeObject, properties are stored in the @properties slot
  if (!inherits(schema, "ellmer::TypeObject")) {
    return(NULL)
  }

  props <- schema@properties
  if (is.null(props) || length(props) == 0) {
    return(NULL)
  }

  # Detect level for each property
  levels <- vapply(props, detect_level_from_type, character(1))

  levels
}


#' Detect measurement level from a single type
#'
#' @param type_obj An ellmer type object (TypeBasic, TypeEnum, etc.)
#' @return Character string: "nominal", "ordinal", "interval", or "ratio"
#' @keywords internal
#' @noRd
detect_level_from_type <- function(type_obj) {
  # Get the class of the type object
  type_class <- class(type_obj)[1]

  # For TypeEnum, always nominal
  if (grepl("TypeEnum", type_class)) {
    return("nominal")
  }

  # For TypeBasic, check the @type slot
  if (grepl("TypeBasic", type_class)) {
    basic_type <- type_obj@type

    level <- switch(basic_type,
      "boolean" = "nominal",
      "string" = "nominal",
      "integer" = "ordinal",
      "number" = "interval",
      "nominal"  # Default if unknown
    )
    return(level)
  }

  # Default to nominal for any other type
  "nominal"
}


#' Validate user-provided levels
#'
#' Checks that user-provided levels are valid and match schema properties.
#'
#' @param levels Named list of levels
#' @param schema An ellmer schema object
#' @keywords internal
#' @noRd
validate_levels <- function(levels, schema) {
  if (is.null(levels)) {
    return(invisible(TRUE))
  }

  # Check that levels is a named list or vector
  if (is.null(names(levels)) || any(names(levels) == "")) {
    cli::cli_abort(c(
      "{.arg levels} must be a named list or vector.",
      "i" = "Names should correspond to variables in the schema."
    ))
  }

  # Check that all level values are valid
  valid_levels <- c("nominal", "ordinal", "interval", "ratio")
  invalid <- !levels %in% valid_levels

  if (any(invalid)) {
    invalid_names <- names(levels)[invalid]
    cli::cli_abort(c(
      "Invalid measurement level{?s} provided for: {.var {invalid_names}}",
      "i" = "Valid levels are: {.val {valid_levels}}"
    ))
  }

  # If schema is provided, check that variable names match schema properties.
  # Properties are collected at every depth, so a variable inside a nested
  # type_object() or the items of a type_array() can be declared too (#131).
  # The root need not be a type_object(): a root type_array() is a documented
  # schema shape, and its items' properties are resolved the same way.
  if (!is.null(schema)) {
    all_props <- schema_prop_names(schema)
    schema_props <- unique(all_props)
    unknown <- !names(levels) %in% schema_props

    if (any(unknown)) {
      unknown_names <- names(levels)[unknown]
      hint <- if (length(schema_props) == 0) {
        "The schema declares no named properties."
      } else {
        "Schema properties are: {.val {schema_props}}"
      }
      cli::cli_abort(c(
        "Variable{?s} not found in schema: {.var {unknown_names}}",
        "i" = hint
      ))
    }

    # A flat levels list cannot say which of two same-named properties at
    # different depths it means, so refuse rather than pick one silently.
    duplicated_props <- unique(all_props[duplicated(all_props)])
    ambiguous <- names(levels) %in% duplicated_props

    if (any(ambiguous)) {
      ambiguous_names <- names(levels)[ambiguous]
      cli::cli_abort(c(
        "Variable{?s} {.var {ambiguous_names}} occur{?s/} more than once in the schema.",
        "i" = "{.arg levels} cannot tell same-named properties at different depths apart.",
        "i" = "Rename the properties so that each declared variable is unique."
      ))
    }
  }

  invisible(TRUE)
}


#' Collect property names at every depth of a schema
#'
#' Descends through nested `type_object()` properties and the items of a
#' `type_array()`. Names are returned in document order and are not
#' deduplicated, so a name occurring at two depths appears twice; callers
#' use that to detect ambiguity.
#'
#' @param x An ellmer type object
#' @return Character vector of property names (possibly with duplicates)
#' @keywords internal
#' @noRd
schema_prop_names <- function(x) {
  if (inherits(x, "ellmer::TypeObject")) {
    props <- x@properties
    c(names(props), unlist(lapply(unname(props), schema_prop_names)))
  } else if (inherits(x, "ellmer::TypeArray")) {
    schema_prop_names(x@items)
  } else {
    character()
  }
}


#' Extract levels from a codebook
#'
#' Returns the measurement levels for variables in a codebook, either from
#' stored levels or by auto-detection from the schema.
#'
#' @param codebook A qlm_codebook object
#' @return Named character vector of measurement levels
#' @keywords internal
#' @noRd
qlm_levels <- function(codebook) {
  # Return stored levels if available
  if (!is.null(codebook$levels)) {
    return(codebook$levels)
  }

  # Otherwise auto-detect from schema
  auto_detect_levels(codebook$schema)
}
