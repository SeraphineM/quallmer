#' Validate a response value against the codebook schema
#'
#' The one set of rules both coding paths apply to what a model returns,
#' before ellmer's converter can coerce a wrong type to `NA`, drop an extra
#' property, or turn a missing array and a valid empty one into the same
#' zero-length cell (#140). The JSON path calls it on parsed text; the
#' structured path on the parsed value ellmer extracted from the response.
#'
#' @param value A parsed JSON value: a named list for an object, an unnamed
#'   list for an array, a length-one vector for a scalar.
#' @param schema The codebook schema, an ellmer type object.
#'
#' @return A list with `ok`, and either `value` (validated, unchanged) or
#'   `error` (a message naming the offending JSON path).
#' @keywords internal
#' @noRd
validate_structured_value <- function(value, schema) {
  checked <- tryCatch(
    validate_against_type(value, schema, path = "$"),
    error = function(e) e
  )
  if (inherits(checked, "error")) {
    return(list(ok = FALSE, error = conditionMessage(checked)))
  }
  list(ok = TRUE, value = checked)
}


#' Validate a parsed JSON value against an ellmer type specification
#'
#' Returns the value unchanged; conversion happens once, after every valid
#' record has passed the same deterministic checks. Signals with `stop()`
#' rather than `cli::cli_abort()` because the condition is caught by
#' [validate_structured_value()] and reported as a coding failure, not
#' raised to the user.
#'
#' Scalars are checked without coercion: `"3"` is not a number, `1` is not a
#' boolean, `3.5` is not an integer. A JSON object is a named list and a JSON
#' array an unnamed one, which is how jsonlite and ellmer both parse them, so
#' `{}` and `[]` stay distinct. A property the schema declares is always
#' present in the returned object, as `NULL` when it was absent or null, so
#' that every validated value has the same shape for conversion.
#'
#' @param value A parsed JSON value.
#' @param type An ellmer type object.
#' @param path JSON path of `value`, used in error messages.
#'
#' @return `value`, unchanged.
#' @keywords internal
#' @noRd
validate_against_type <- function(value, type, path) {
  if (is.null(value)) {
    if (isTRUE(type@required)) {
      stop(path, " is required and cannot be null.", call. = FALSE)
    }
    return(NULL)
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    valid <- switch(type@type,
      string = is.character(value) && length(value) == 1L && !is.na(value),
      boolean = is.logical(value) && length(value) == 1L && !is.na(value),
      integer = is.numeric(value) && length(value) == 1L && is.finite(value) &&
        value == trunc(value) && abs(value) <= .Machine$integer.max,
      number = is.numeric(value) && length(value) == 1L && is.finite(value),
      FALSE
    )
    if (!isTRUE(valid)) {
      stop(path, " must be a ", type@type, ".", call. = FALSE)
    }
    return(value)
  }

  if (inherits(type, "ellmer::TypeEnum")) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !value %in% type@values) {
      stop(path, " must be one of: ", paste(type@values, collapse = ", "), ".",
           call. = FALSE)
    }
    return(value)
  }

  if (inherits(type, "ellmer::TypeArray")) {
    if (!is.list(value) || !is.null(names(value))) {
      stop(path, " must be a JSON array.", call. = FALSE)
    }
    return(lapply(seq_along(value), function(i) {
      validate_against_type(value[[i]], type@items, paste0(path, "[", i, "]"))
    }))
  }

  if (inherits(type, "ellmer::TypeObject")) {
    if (!is.list(value) || is.null(names(value))) {
      stop(path, " must be a JSON object.", call. = FALSE)
    }
    property_names <- names(type@properties)
    extras <- setdiff(names(value), property_names)
    if (!isTRUE(type@additional_properties) && length(extras)) {
      stop(path, " has unexpected propert", if (length(extras) == 1L) "y: " else "ies: ",
           paste(extras, collapse = ", "), ".", call. = FALSE)
    }
    output <- vector("list", length(type@properties))
    names(output) <- property_names
    for (property in property_names) {
      property_type <- type@properties[[property]]
      if (!property %in% names(value)) {
        if (isTRUE(property_type@required)) {
          stop(path, ".", property, " is required but missing.", call. = FALSE)
        }
        output[property] <- list(NULL)
      } else {
        # Single-bracket assignment: `output[[property]] <- NULL` would delete
        # the property, so an optional value sent as null would vanish from
        # the object while an absent one stayed. Both must read as NULL.
        output[property] <- list(validate_against_type(
          value[[property]], property_type, paste0(path, ".", property)
        ))
      }
    }
    if (isTRUE(type@additional_properties) && length(extras)) {
      output[extras] <- value[extras]
    }
    return(output)
  }

  stop(path, " uses an unsupported schema type.", call. = FALSE)
}


#' Refuse a codebook schema the coding paths cannot validate or tabulate
#'
#' Checked before any upload or request, so that an unsupported schema is one
#' error rather than a paid run in which every unit fails. Two things are
#' required. The root must be a `type_object()`: its properties become the
#' columns of the result, and ellmer's converter turns anything else into a
#' list that has no rows. And every type in the schema must be one the
#' validator has rules for: `type_string()`, `type_boolean()`,
#' `type_integer()`, `type_number()`, `type_enum()`, `type_array()` and
#' `type_object()`. A `type_from_schema()` or other opaque type carries no
#' structure to check a response against.
#'
#' `qlm_codebook()` accepts a root `type_array()`, since a codebook is also
#' an instrument for other uses; it is a coding call that needs rows.
#'
#' @param schema The codebook schema.
#' @param call The environment to report from.
#'
#' @return `schema`, invisibly.
#' @keywords internal
#' @noRd
check_coding_schema <- function(schema, call = rlang::caller_env()) {
  if (is.null(schema)) {
    cli::cli_abort(c(
      "The codebook has no schema.",
      "i" = "Coding needs a {.fn type_object} whose properties become the coded variables."
    ), call = call)
  }
  if (!inherits(schema, "ellmer::TypeObject")) {
    cli::cli_abort(c(
      "The codebook schema must be a {.fn type_object} at the root.",
      "i" = "Its properties become the columns of the result; a {.fn type_array} root has no rows to tabulate.",
      "i" = "Wrap the array in an object: {.code type_object(items = type_array(...))}."
    ), call = call)
  }
  unsupported <- unsupported_schema_paths(schema, "$")
  if (length(unsupported)) {
    cli::cli_abort(c(
      "The codebook schema uses a type that cannot be validated: {.code {unsupported}}.",
      "i" = "Supported types are {.fn type_string}, {.fn type_boolean}, {.fn type_integer}, {.fn type_number}, {.fn type_enum}, {.fn type_array} and {.fn type_object}.",
      "i" = "A response is checked against the schema before it is tabulated, and an opaque type gives nothing to check."
    ), call = call)
  }
  invisible(schema)
}


#' JSON paths in a schema whose type the validator has no rules for
#'
#' @param type An ellmer type object.
#' @param path The JSON path of `type`.
#'
#' @return A character vector of paths, each with the type's class, empty
#'   when every type is supported.
#' @keywords internal
#' @noRd
unsupported_schema_paths <- function(type, path) {
  if (inherits(type, c("ellmer::TypeBasic", "ellmer::TypeEnum"))) {
    return(character())
  }
  if (inherits(type, "ellmer::TypeArray")) {
    return(unsupported_schema_paths(type@items, paste0(path, "[]")))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    props <- type@properties
    return(unlist(lapply(names(props), function(name) {
      unsupported_schema_paths(props[[name]], paste0(path, ".", name))
    })))
  }
  cls <- sub("^ellmer::", "", class(type)[[1]])
  paste0(path, " (", cls, ")")
}
