#' Upgrade old metadata structure to new format
#'
#' Migrates objects using the old `attr(x, "run")` structure to the new
#' `attr(x, "meta")` structure. This function is called automatically by
#' accessor functions when they detect an old-style object.
#'
#' @param x A quallmer object with old-style metadata
#' @return The same object with upgraded metadata structure
#' @keywords internal
#' @noRd
upgrade_meta <- function(x) {
  # Check if already upgraded
  if (!is.null(attr(x, "meta"))) {
    return(x)
  }

  # Get old run attribute
  run <- attr(x, "run")
  if (is.null(run)) {
    return(x)  # Nothing to upgrade
  }

  # Build new metadata structure
  if (inherits(x, "qlm_coded")) {
    # For qlm_coded objects
    object_meta <- list(
      batch = run$batch,
      call = run$call,
      chat_args = run$chat_args,
      execution_args = run$execution_args,
      parent = run$parent,
      n_units = run$metadata$n_units,
      input_type = attr(x, "input_type")
    )

    # Add source and is_gold if present (for human-coded data)
    if (!is.null(run$metadata$source)) {
      object_meta$source <- run$metadata$source
    }
    if (!is.null(run$metadata$is_gold)) {
      object_meta$is_gold <- run$metadata$is_gold
    }

    new_meta <- list(
      user = list(
        name = run$name,
        notes = run$metadata$notes
      ),
      object = object_meta,
      system = list(
        timestamp = run$metadata$timestamp,
        ellmer_version = run$metadata$ellmer_version,
        quallmer_version = run$metadata$quallmer_version,
        R_version = run$metadata$R_version
      )
    )

    # Set new attributes
    attr(x, "meta") <- new_meta
    attr(x, "codebook") <- run$codebook

    # Remove old attributes
    attr(x, "run") <- NULL
    # Keep attr(x, "data") and old attr(x, "input_type") for now, will be cleaned up

  } else if (inherits(x, "qlm_comparison")) {
    # For qlm_comparison objects
    new_meta <- list(
      user = list(
        name = run$name,
        notes = run$metadata$notes
      ),
      object = list(
        call = run$call,
        parent = run$parent,
        n_raters = run$metadata$n_raters,
        variables = run$metadata$variables
      ),
      system = list(
        timestamp = run$metadata$timestamp,
        quallmer_version = run$metadata$quallmer_version,
        R_version = run$metadata$R_version
      )
    )

    attr(x, "meta") <- new_meta
    attr(x, "run") <- NULL

  } else if (inherits(x, "qlm_validation")) {
    # For qlm_validation objects
    new_meta <- list(
      user = list(
        name = run$name,
        notes = run$metadata$notes
      ),
      object = list(
        call = run$call,
        parent = run$parent,
        variables = run$metadata$variables,
        average = run$metadata$average
      ),
      system = list(
        timestamp = run$metadata$timestamp,
        quallmer_version = run$metadata$quallmer_version,
        R_version = run$metadata$R_version
      )
    )

    attr(x, "meta") <- new_meta
    attr(x, "run") <- NULL
  }

  x
}


#' Get or set quallmer object metadata
#'
#' Get or set metadata from `qlm_coded`, `qlm_codebook`, `qlm_comparison`, and
#' `qlm_validation` objects. Metadata is organized into three types: user,
#' object, and system. Only user metadata can be modified.
#'
#' @param x A quallmer object (`qlm_coded`, `qlm_codebook`, `qlm_comparison`, or `qlm_validation`).
#' @param field Optional character string specifying a single metadata field to extract or set.
#'   If `NULL` (default), `qlm_meta()` returns all metadata of the specified type, and
#'   `qlm_meta<-()` expects `value` to be a named list.
#' @param type Character string specifying the type of metadata to extract:
#'   \describe{
#'     \item{`"user"`}{User-specified descriptive information (default). These fields
#'       are modifiable via `qlm_meta<-()`: `name` (run label) and `notes` (documentation).}
#'     \item{`"object"`}{Parameters defining how coding was executed. Read-only fields
#'       include: `batch`, `call`, `chat_args`, `execution_args`, `parent`, `n_units`,
#'       `input_type`.}
#'     \item{`"system"`}{Automatically captured environment information. Read-only fields
#'       include: `timestamp`, `ellmer_version`, `quallmer_version`, `R_version`.}
#'     \item{`"all"`}{Returns a named list combining all three types.}
#'   }
#'
#' @param value For `qlm_meta<-()`, the new value for the metadata field, or a
#'   named list of user metadata fields.
#'
#' @return `qlm_meta()` returns the requested metadata (a named list or single value).
#'   `qlm_meta<-()` returns the modified object (invisibly).
#'
#' @details
#' Metadata is stratified into three types following the quanteda convention:
#'
#' **User metadata** (`type = "user"`, default): User-specified descriptive information
#' that can be modified via `qlm_meta<-()`. Fields: `name`, `notes`.
#'
#' **Object metadata** (`type = "object"`): Parameters and intrinsic properties set
#' at object creation time. Read-only. Fields vary by object type but typically include:
#' `batch`, `call`, `chat_args`, `execution_args`, `parent`, `n_units`, `input_type`.
#'
#' **System metadata** (`type = "system"`): Automatically captured environment and
#' version information. Read-only. Fields: `timestamp`, `ellmer_version`,
#' `quallmer_version`, `R_version`.
#'
#' For `qlm_codebook` objects, user metadata includes `name` and `instructions`
#' (the codebook instructions text), both of which can be modified.
#'
#' **Modification via `qlm_meta<-()` (assignment):**
#'
#' Only user metadata can be modified. For `qlm_coded`, `qlm_comparison`, and
#' `qlm_validation` objects, modifiable fields are `name` and `notes`. For
#' `qlm_codebook` objects, modifiable fields are `name` and `instructions`.
#'
#' Object and system metadata are read-only and set at creation time. Attempting
#' to modify these will produce an informative error.
#'
#' @seealso
#' - [accessors] for an overview of the accessor function system
#' - [codebook()] for extracting the codebook component
#' - [inputs()] for extracting input data
#'
#' @examples
#' # Load example objects
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#' coded <- examples$example_coded_sentiment
#'
#' # User metadata (default)
#' qlm_meta(coded)
#' qlm_meta(coded, "name")
#'
#' # Object metadata
#' qlm_meta(coded, type = "object")
#' qlm_meta(coded, "call", type = "object")
#' qlm_meta(coded, "n_units", type = "object")
#'
#' # System metadata
#' qlm_meta(coded, type = "system")
#' qlm_meta(coded, "timestamp", type = "system")
#'
#' # All metadata
#' qlm_meta(coded, type = "all")
#'
#' # Modify user metadata
#' qlm_meta(coded, "name") <- "updated_run"
#' qlm_meta(coded, "notes") <- "Analysis notes"
#'
#' # Set multiple fields at once
#' qlm_meta(coded) <- list(name = "final_run", notes = "Final analysis")
#'
#' \dontrun{
#' # This will error - object and system metadata are read-only
#' qlm_meta(coded, "timestamp") <- Sys.time()
#' }
#'
#' @export
qlm_meta <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
  UseMethod("qlm_meta")
}


#' @export
qlm_meta.qlm_coded <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
  type <- match.arg(type)

  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  if (is.null(meta_attr)) {
    cli::cli_abort("Object has no metadata attributes.")
  }

  # Extract metadata by type
  if (type == "user") {
    metadata <- meta_attr$user
  } else if (type == "object") {
    metadata <- meta_attr$object
  } else if (type == "system") {
    metadata <- meta_attr$system
  } else if (type == "all") {
    metadata <- list(
      user = meta_attr$user,
      object = meta_attr$object,
      system = meta_attr$system
    )
  }

  # Return specific field or all metadata
  if (!is.null(field)) {
    if (type == "all") {
      cli::cli_abort(c(
        "Cannot specify {.arg field} when {.code type = 'all'}.",
        "i" = "Use a specific type or omit {.arg field}."
      ))
    }
    if (!field %in% names(metadata)) {
      cli::cli_abort(c(
        "Field {.val {field}} not found in {.val {type}} metadata.",
        "i" = "Available fields: {.val {names(metadata)}}"
      ))
    }
    return(metadata[[field]])
  }

  metadata
}


#' @export
qlm_meta.qlm_comparison <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
  type <- match.arg(type)

  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  if (is.null(meta_attr)) {
    cli::cli_abort("Object has no metadata attributes.")
  }

  # Extract metadata by type
  if (type == "user") {
    metadata <- meta_attr$user
  } else if (type == "object") {
    metadata <- meta_attr$object
  } else if (type == "system") {
    metadata <- meta_attr$system
  } else if (type == "all") {
    metadata <- list(
      user = meta_attr$user,
      object = meta_attr$object,
      system = meta_attr$system
    )
  }

  # Return specific field or all metadata
  if (!is.null(field)) {
    if (type == "all") {
      cli::cli_abort(c(
        "Cannot specify {.arg field} when {.code type = 'all'}.",
        "i" = "Use a specific type or omit {.arg field}."
      ))
    }
    if (!field %in% names(metadata)) {
      cli::cli_abort(c(
        "Field {.val {field}} not found in {.val {type}} metadata.",
        "i" = "Available fields: {.val {names(metadata)}}"
      ))
    }
    return(metadata[[field]])
  }

  metadata
}


#' @export
qlm_meta.qlm_validation <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
  type <- match.arg(type)

  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  if (is.null(meta_attr)) {
    cli::cli_abort("Object has no metadata attributes.")
  }

  # Extract metadata by type
  if (type == "user") {
    metadata <- meta_attr$user
  } else if (type == "object") {
    metadata <- meta_attr$object
  } else if (type == "system") {
    metadata <- meta_attr$system
  } else if (type == "all") {
    metadata <- list(
      user = meta_attr$user,
      object = meta_attr$object,
      system = meta_attr$system
    )
  }

  # Return specific field or all metadata
  if (!is.null(field)) {
    if (type == "all") {
      cli::cli_abort(c(
        "Cannot specify {.arg field} when {.code type = 'all'}.",
        "i" = "Use a specific type or omit {.arg field}."
      ))
    }
    if (!field %in% names(metadata)) {
      cli::cli_abort(c(
        "Field {.val {field}} not found in {.val {type}} metadata.",
        "i" = "Available fields: {.val {names(metadata)}}"
      ))
    }
    return(metadata[[field]])
  }

  metadata
}


#' @export
qlm_meta.qlm_codebook <- function(x, field = NULL, type = c("user", "object", "system", "all")) {
  type <- match.arg(type)

  # For codebooks, user metadata includes name and instructions
  # Object metadata includes schema, role, input_type, levels
  # System metadata is empty (codebooks don't have timestamps etc)

  if (type == "user") {
    metadata <- list(
      name = x$name,
      instructions = x$instructions
    )
  } else if (type == "object") {
    metadata <- list(
      schema = x$schema,
      role = x$role,
      input_type = x$input_type,
      levels = x$levels
    )
  } else if (type == "system") {
    # Codebooks don't have system metadata
    metadata <- list()
  } else if (type == "all") {
    metadata <- list(
      user = qlm_meta.qlm_codebook(x, field = NULL, type = "user"),
      object = qlm_meta.qlm_codebook(x, field = NULL, type = "object"),
      system = qlm_meta.qlm_codebook(x, field = NULL, type = "system")
    )
  }

  # Return specific field or all metadata
  if (!is.null(field)) {
    if (type == "all") {
      cli::cli_abort(c(
        "Cannot specify {.arg field} when {.code type = 'all'}.",
        "i" = "Use a specific type or omit {.arg field}."
      ))
    }
    if (!field %in% names(metadata)) {
      cli::cli_abort(c(
        "Field {.val {field}} not found in {.val {type}} metadata.",
        "i" = "Available fields: {.val {names(metadata)}}"
      ))
    }
    return(metadata[[field]])
  }

  metadata
}


#' @rdname qlm_meta
#' @export
`qlm_meta<-` <- function(x, field = NULL, value) {
  UseMethod("qlm_meta<-")
}


#' @export
`qlm_meta<-.qlm_coded` <- function(x, field = NULL, value) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  if (is.null(meta_attr)) {
    cli::cli_abort("Object has no metadata attributes.")
  }

  # Valid user metadata fields for qlm_coded
  valid_fields <- c("name", "notes")

  if (!is.null(field)) {
    # Single field update
    if (!field %in% valid_fields) {
      cli::cli_abort(c(
        "Cannot set field {.val {field}}.",
        "x" = "Object and system metadata are read-only and set at creation time.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    # Update the appropriate field in user metadata
    meta_attr$user[[field]] <- value
  } else {
    # Multiple field update - value should be a list
    if (!is.list(value)) {
      cli::cli_abort(c(
        "When {.arg field} is {.code NULL}, {.arg value} must be a named list.",
        "i" = "Example: {.code qlm_meta(x) <- list(name = 'new_name', notes = 'new notes')}"
      ))
    }

    # Check for invalid fields
    invalid_fields <- setdiff(names(value), valid_fields)
    if (length(invalid_fields) > 0) {
      cli::cli_abort(c(
        "Cannot set field{?s} {.val {invalid_fields}}.",
        "x" = "Object and system metadata are read-only and set at creation time.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    # Update fields in user metadata
    for (field_name in names(value)) {
      meta_attr$user[[field_name]] <- value[[field_name]]
    }
  }

  # Update the meta attribute
  attr(x, "meta") <- meta_attr
  x
}


#' @export
`qlm_meta<-.qlm_comparison` <- function(x, field = NULL, value) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  if (is.null(meta_attr)) {
    cli::cli_abort("Object has no metadata attributes.")
  }

  # Valid user metadata fields
  valid_fields <- c("name", "notes")

  if (!is.null(field)) {
    if (!field %in% valid_fields) {
      cli::cli_abort(c(
        "Cannot set field {.val {field}}.",
        "x" = "Object and system metadata are read-only and set at creation time.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    meta_attr$user[[field]] <- value
  } else {
    if (!is.list(value)) {
      cli::cli_abort(c(
        "When {.arg field} is {.code NULL}, {.arg value} must be a named list.",
        "i" = "Example: {.code qlm_meta(x) <- list(name = 'new_name', notes = 'new notes')}"
      ))
    }

    invalid_fields <- setdiff(names(value), valid_fields)
    if (length(invalid_fields) > 0) {
      cli::cli_abort(c(
        "Cannot set field{?s} {.val {invalid_fields}}.",
        "x" = "Object and system metadata are read-only and set at creation time.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    for (field_name in names(value)) {
      meta_attr$user[[field_name]] <- value[[field_name]]
    }
  }

  attr(x, "meta") <- meta_attr
  x
}


#' @export
`qlm_meta<-.qlm_validation` <- function(x, field = NULL, value) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  if (is.null(meta_attr)) {
    cli::cli_abort("Object has no metadata attributes.")
  }

  # Valid user metadata fields
  valid_fields <- c("name", "notes")

  if (!is.null(field)) {
    if (!field %in% valid_fields) {
      cli::cli_abort(c(
        "Cannot set field {.val {field}}.",
        "x" = "Object and system metadata are read-only and set at creation time.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    meta_attr$user[[field]] <- value
  } else {
    if (!is.list(value)) {
      cli::cli_abort(c(
        "When {.arg field} is {.code NULL}, {.arg value} must be a named list.",
        "i" = "Example: {.code qlm_meta(x) <- list(name = 'new_name', notes = 'new notes')}"
      ))
    }

    invalid_fields <- setdiff(names(value), valid_fields)
    if (length(invalid_fields) > 0) {
      cli::cli_abort(c(
        "Cannot set field{?s} {.val {invalid_fields}}.",
        "x" = "Object and system metadata are read-only and set at creation time.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    for (field_name in names(value)) {
      meta_attr$user[[field_name]] <- value[[field_name]]
    }
  }

  attr(x, "meta") <- meta_attr
  x
}


#' @export
`qlm_meta<-.qlm_codebook` <- function(x, field = NULL, value) {
  # Valid user metadata fields for codebooks
  valid_fields <- c("name", "instructions")

  if (!is.null(field)) {
    if (!field %in% valid_fields) {
      cli::cli_abort(c(
        "Cannot set field {.val {field}}.",
        "x" = "Object metadata is read-only for codebooks.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    x[[field]] <- value
  } else {
    if (!is.list(value)) {
      cli::cli_abort(c(
        "When {.arg field} is {.code NULL}, {.arg value} must be a named list.",
        "i" = "Example: {.code qlm_meta(cb) <- list(name = 'new_name', instructions = 'new instructions')}"
      ))
    }

    invalid_fields <- setdiff(names(value), valid_fields)
    if (length(invalid_fields) > 0) {
      cli::cli_abort(c(
        "Cannot set field{?s} {.val {invalid_fields}}.",
        "x" = "Object metadata is read-only for codebooks.",
        "i" = "Only user metadata fields can be modified: {.val {valid_fields}}"
      ))
    }

    if ("name" %in% names(value)) {
      x$name <- value$name
    }
    if ("instructions" %in% names(value)) {
      x$instructions <- value$instructions
    }
  }

  x
}


#' Extract codebook from quallmer objects
#'
#' Extracts the codebook component from `qlm_coded`, `qlm_comparison`, and
#' `qlm_validation` objects. The codebook is a constitutive part of the coding
#' run, defining the coding instrument used.
#'
#' @param x A quallmer object (`qlm_coded`, `qlm_comparison`, or `qlm_validation`).
#'
#' @return A `qlm_codebook` object, or `NULL` if no codebook is available.
#'
#' @details
#' The codebook is a core component of coded objects, analogous to `formula()`
#' for `lm` objects. It specifies the coding instrument (instructions, schema,
#' role) used in the coding run.
#'
#' This function is an extractor for the codebook component, not a metadata
#' accessor. For codebook metadata (name, instructions), use [qlm_meta()].
#'
#' Note: `qlm_codebook()` is the constructor for creating codebooks; `codebook()`
#' is the extractor for retrieving them from coded objects.
#'
#' @seealso
#' - [accessors] for an overview of the accessor function system
#' - [qlm_codebook()] for creating codebooks
#' - [qlm_meta()] for extracting metadata
#' - [inputs()] for extracting input data
#'
#' @examples
#' # Load example objects
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#' coded <- examples$example_coded_sentiment
#'
#' # Extract codebook
#' cb <- codebook(coded)
#' cb
#'
#' # Access codebook metadata
#' qlm_meta(cb, "name")
#'
#' @export
#' @keywords internal
codebook <- function(x) {
  UseMethod("codebook")
}


#' @export
codebook.qlm_coded <- function(x) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  attr(x, "codebook")
}


#' @export
codebook.qlm_comparison <- function(x) {
  # For comparison objects, try to extract codebook from parent runs
  # Return NULL if not available (comparisons may not always have codebooks)
  run <- attr(x, "run")
  if (is.null(run)) {
    return(NULL)
  }
  # Comparison objects don't directly store codebooks
  NULL
}


#' @export
codebook.qlm_validation <- function(x) {
  # For validation objects, try to extract codebook from parent runs
  # Return NULL if not available
  run <- attr(x, "run")
  if (is.null(run)) {
    return(NULL)
  }
  # Validation objects don't directly store codebooks
  NULL
}


#' Extract input data from qlm_coded objects
#'
#' Extracts the original input data (texts or image paths) from `qlm_coded`
#' objects. The inputs are the source material that was coded, constituting
#' a core component of the coded object.
#'
#' @param x A `qlm_coded` object.
#'
#' @return The original input data: a character vector of texts (for text
#'   codebooks) or file paths to images (for image codebooks). If the original
#'   input had names, these are preserved.
#'
#' @details
#' The inputs are a core component of coded objects, representing the source
#' material that was coded. Like [codebook()], this is a component extractor
#' rather than a metadata accessor.
#'
#' The function name mirrors the `inputs` argument in [qlm_code()], providing
#' a direct conceptual mapping: what is passed in via `inputs =` is retrieved
#' back via `inputs()`.
#'
#' @seealso
#' - [accessors] for an overview of the accessor function system
#' - [qlm_code()] for creating coded objects
#' - [codebook()] for extracting the codebook
#' - [qlm_meta()] for extracting metadata
#'
#' @examples
#' # Load example objects
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#' coded <- examples$example_coded_sentiment
#'
#' # Extract inputs
#' texts <- inputs(coded)
#' texts
#'
#' @export
#' @keywords internal
inputs <- function(x) {
  UseMethod("inputs")
}


#' @export
inputs.qlm_coded <- function(x) {
  attr(x, "data")
}
