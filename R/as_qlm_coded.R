#' Convert coded data to qlm_coded format
#'
#' Converts a data frame or quanteda corpus of coded data (human-coded or from
#' external sources) into a `qlm_coded` object. This enables provenance tracking
#' and integration with `qlm_compare()`, `qlm_validate()`, and `qlm_trail()` for
#' coded data alongside LLM-coded results.
#'
#' @param x A data frame or quanteda corpus object containing coded data.
#'   For data frames: Must include a column with unit identifiers (default
#'   `".id"`).
#'   For corpus objects: Document variables (docvars) are treated as coded
#'   variables, and document names are used as identifiers by default.
#'
#' @return A `qlm_coded` object (tibble with additional class and attributes)
#'   for provenance tracking. When `is_gold = TRUE`, the object is marked as
#'   a gold standard in its attributes.
#'
#' @details
#' When printed, objects created with `as_qlm_coded()` display "Source: Human coder"
#' instead of model information, clearly distinguishing human from LLM coding.
#'
#' ## Gold Standards
#'
#' Objects marked with `is_gold = TRUE` are automatically detected by
#' [qlm_validate()], allowing simpler syntax:
#'
#' ```r
#' # With is_gold = TRUE
#' gold <- as_qlm_coded(gold_data, name = "Expert", is_gold = TRUE)
#' qlm_validate(coded1, coded2, gold, by = "sentiment")  # gold = not needed!
#'
#' # Without is_gold (or explicit gold =)
#' gold <- as_qlm_coded(gold_data, name = "Expert")
#' qlm_validate(coded1, coded2, gold = gold, by = "sentiment")
#' ```
#'
#' @seealso
#' [qlm_code()] for LLM coding, [qlm_compare()] for inter-rater reliability,
#' [qlm_validate()] for validation against gold standards, [qlm_trail()] for
#' provenance tracking.
#'
#' @examples
#' # Basic usage with data frame (default .id column)
#' human_data <- data.frame(
#'   .id = 1:10,
#'   sentiment = sample(c("pos", "neg"), 10, replace = TRUE)
#' )
#'
#' coder_a <- as_qlm_coded(human_data, name = "Coder_A")
#' coder_a
#'
#' # Use custom id column with NSE (unquoted)
#' data_with_custom_id <- data.frame(
#'   doc_id = 1:10,
#'   sentiment = sample(c("pos", "neg"), 10, replace = TRUE)
#' )
#' coder_custom <- as_qlm_coded(data_with_custom_id, id = doc_id, name = "Coder_C")
#'
#' # Or use quoted string
#' coder_custom2 <- as_qlm_coded(data_with_custom_id, id = "doc_id", name = "Coder_D")
#'
#' # Create a gold standard from data frame
#' gold <- as_qlm_coded(
#'   human_data,
#'   name = "Expert",
#'   is_gold = TRUE
#' )
#'
#' # Validate with automatic gold detection
#' coder_b_data <- data.frame(
#'   .id = 1:10,
#'   sentiment = sample(c("pos", "neg"), 10, replace = TRUE)
#' )
#' coder_b <- as_qlm_coded(coder_b_data, name = "Coder_B")
#'
#' # No need for gold = when gold object is marked (NSE works for 'by' too)
#' qlm_validate(coder_a, coder_b, gold = gold, by = sentiment, level = "nominal")
#'
#' # Create from corpus object (simplified workflow)
#' data("data_corpus_manifsentsUK2010sample")
#' crowd <- as_qlm_coded(
#'   data_corpus_manifsentsUK2010sample,
#'   is_gold = TRUE
#' )
#' # Document names automatically become .id, all docvars included
#'
#' # Use a docvar as identifier with NSE (unquoted)
#' crowd_party <- as_qlm_coded(
#'   data_corpus_manifsentsUK2010sample,
#'   id = party,
#'   is_gold = TRUE
#' )
#'
#' # Or use quoted string
#' crowd_party2 <- as_qlm_coded(
#'   data_corpus_manifsentsUK2010sample,
#'   id = "party",
#'   is_gold = TRUE
#' )
#'
#' # With complete metadata
#' expert <- as_qlm_coded(
#'   human_data,
#'   name = "expert_rater",
#'   is_gold = TRUE,
#'   codebook = list(
#'     name = "Sentiment Analysis",
#'     instructions = "Code overall sentiment as positive or negative"
#'   ),
#'   metadata = list(
#'     coder_name = "Dr. Smith",
#'     coder_id = "EXP001",
#'     training = "5 years experience",
#'     date = "2024-01-15"
#'   )
#' )
#'
#' @export
as_qlm_coded <- function(x,
                          id,
                          name = NULL,
                          is_gold = FALSE,
                          codebook = NULL,
                          texts = NULL,
                          notes = NULL,
                          metadata = list(),
                          qlm_segment = FALSE,
                          source_text = NULL) {
  UseMethod("as_qlm_coded")
}


#' @rdname as_qlm_coded
#' @param id For data frames: Name of the column containing unit identifiers
#'   (supports both quoted and unquoted). Default is `NULL`, which looks for a
#'   column named `".id"`. Can be an unquoted column name (`id = doc_id`) or a
#'   quoted string (`id = "doc_id"`). For corpus objects: `NULL` (default) uses
#'   document names from `names(x)`, or specify a docvar name (quoted or
#'   unquoted) to use as identifiers.
#' @param name Character. a string identifying this coding run (e.g., "Coder_A",
#'   "expert_rater", "Gold_Standard"). Default is `NULL`.
#' @param is_gold Logical. If `TRUE`, marks this object as a gold standard for
#'   automatic detection by [qlm_validate()]. When a gold standard object is
#'   passed to `qlm_validate()`, the `gold =` parameter becomes optional.
#'   Default is `FALSE`.
#' @param codebook Optional list containing coding instructions. Can include:
#'   \describe{
#'     \item{`name`}{Name of the coding scheme}
#'     \item{`instructions`}{Text describing coding instructions}
#'     \item{`schema`}{NULL (not used for human coding)}
#'   }
#'   If `NULL` (default), a minimal placeholder codebook is created.
#' @param texts Optional vector of original texts or data that were coded.
#'   Should correspond to the `.id` values in `data`. If provided, enables
#'   more complete provenance tracking.
#' @param notes Optional character string with descriptive notes about this
#'   coding. Useful for documenting details when viewing results in
#'   [qlm_trail()]. Default is `NULL`.
#' @param metadata Optional list of metadata about the coding process. Can
#'   include any relevant information such as:
#'   \describe{
#'     \item{`coder_name`}{Name of the human coder}
#'     \item{`coder_id`}{Identifier for the coder}
#'     \item{`training`}{Description of coder training}
#'     \item{`date`}{Date of coding}
#'   }
#'   The function automatically adds `timestamp`, `n_units`, `notes`, and
#'   `source = "human"`.
#' @param qlm_segment Logical. If `TRUE`, converts the data to a segmented
#'   quanteda corpus suitable for unitizing comparison with [qlm_compare()].
#'   Requires a `text` column in `x` and a `source_text` argument. Default
#'   is `FALSE`.
#' @param source_text A named character vector of source texts. Required when
#'   `qlm_segment = TRUE`. Names must match `docid` values in `x` (or a
#'   single unnamed string for single-document data). Used to compute
#'   character-level segment positions for unitizing reliability.
#' @export
as_qlm_coded.data.frame <- function(
  x,
  id,
  name = NULL,
  is_gold = FALSE,
  codebook = NULL,
  texts = NULL,
  notes = NULL,
  metadata = list(),
  qlm_segment = FALSE,
  source_text = NULL
) {
  # Segmentation path: return a quanteda corpus with positions

  if (isTRUE(qlm_segment)) {
    return(as_qlm_coded_segment(x, source_text = source_text, name = name,
                                 is_gold = is_gold))
  }

  # Handle id parameter with NSE support
  if (missing(id)) {
    id <- ".id"
  } else {
    id <- tryCatch({
      # Try to capture as symbol first (supports bare names)
      rlang::as_string(rlang::ensym(id))
    }, error = function(e) {
      # If that fails, check if it's already a character string
      if (is.character(id) && length(id) == 1) {
        id
      } else {
        cli::cli_abort(c(
          "Invalid {.arg id} argument.",
          "i" = "Use an unquoted column name (e.g., {.code id = doc_id}) or a quoted string (e.g., {.code id = \"doc_id\"})."
        ))
      }
    })
  }

  # Validate and handle id column
  if (!id %in% names(x)) {
    cli::cli_abort(c(
      "{.arg x} must contain a {.val {id}} column.",
      "i" = "Available columns: {.val {names(x)}}"
    ))
  }

  # Rename id column to .id if needed
  if (id != ".id") {
    names(x)[names(x) == id] <- ".id"
  }

  # Create minimal codebook if not provided
  if (is.null(codebook)) {
    codebook <- list(
      name = "Human-coded data",
      instructions = "Data coded by human annotator",
      schema = NULL
    )
  } else {
    # Ensure codebook has required structure
    if (!is.list(codebook)) {
      cli::cli_abort("{.arg codebook} must be a list or NULL.")
    }
    if (is.null(codebook$name)) {
      codebook$name <- "Human-coded data"
    }
    if (is.null(codebook$instructions)) {
      codebook$instructions <- "Data coded by human annotator"
    }
    codebook$schema <- NULL  # Always NULL for human coding
  }

  # Add qlm_codebook class
  class(codebook) <- c("qlm_codebook", "task")

  # Merge user metadata with defaults
  full_metadata <- c(
    list(
      timestamp = Sys.time(),
      n_units = nrow(x),
      notes = notes,
      source = "human",
      is_gold = is_gold
    ),
    metadata
  )

  # Create qlm_coded object using the internal constructor
  result <- new_qlm_coded(
    results = x,
    codebook = codebook,
    data = texts,
    input_type = "human",
    chat_args = list(source = "human"),
    execution_args = list(),
    metadata = full_metadata,
    name = name,
    call = match.call(),
    parent = NULL
  )

  # Add qlm_humancoded class for backwards compatibility
  class(result) <- c("qlm_humancoded", class(result))

  result
}


#' @export
as_qlm_coded.corpus <- function(
  x,
  id,
  name = NULL,
  is_gold = FALSE,
  codebook = NULL,
  texts = NULL,
  notes = NULL,
  metadata = list(),
  qlm_segment = FALSE,
  source_text = NULL
) {
  # Wrap corpus with qlm_corpus class
  x <- as_qlm_corpus(x)

  # Extract docvars from corpus
  docvars <- attr(x, "docvars")

  # Handle case where corpus has no docvars
  if (is.null(docvars) || ncol(docvars) == 0) {
    cli::cli_abort(c(
      "Corpus has no document variables to convert to coded data.",
      "i" = "Use a corpus with docvars containing coded variables, or convert to a data frame manually."
    ))
  }

  # Skip internal quanteda docvars (docname_, docid_, segid_)
  internal_vars <- c("docname_", "docid_", "segid_")
  user_docvars <- setdiff(names(docvars), internal_vars)

  if (length(user_docvars) == 0) {
    cli::cli_abort(c(
      "Corpus has no user-defined document variables to convert to coded data.",
      "i" = "Only internal quanteda variables (docname_, docid_, segid_) were found.",
      "i" = "Use a corpus with docvars containing coded variables."
    ))
  }

  # Determine how to create .id column
  if (missing(id)) {
    # Default: use document names
    doc_names <- names(x)
    if (is.null(doc_names) || length(doc_names) == 0) {
      cli::cli_abort(c(
        "Corpus has no document names.",
        "i" = "Specify {.arg id} parameter to use a docvar as identifier."
      ))
    }
    id_col <- doc_names
    data_cols <- docvars[, user_docvars, drop = FALSE]
  } else {
    # User specified an id - use NSE to capture it
    id <- tryCatch({
      # Try to capture as symbol first (supports bare names)
      rlang::as_string(rlang::ensym(id))
    }, error = function(e) {
      # If that fails, check if it's already a character string
      if (is.character(id) && length(id) == 1) {
        id
      } else {
        cli::cli_abort(c(
          "Invalid {.arg id} argument.",
          "i" = "Use an unquoted docvar name (e.g., {.code id = party}) or a quoted string (e.g., {.code id = \"party\"})."
        ))
      }
    })

    # Check if the specified id is a docvar
    if (!id %in% names(docvars)) {
      cli::cli_abort(c(
        "Specified {.arg id} docvar {.val {id}} not found in corpus.",
        "i" = "Available docvars: {.val {names(docvars)}}"
      ))
    }

    # Use specified docvar as .id
    id_col <- docvars[[id]]

    # Remove id column from other data
    other_vars <- setdiff(user_docvars, id)
    if (length(other_vars) == 0) {
      cli::cli_abort(c(
        "No coded variables remain after using {.val {id}} as identifier.",
        "i" = "Corpus must have at least one docvar in addition to the identifier."
      ))
    }
    data_cols <- docvars[, other_vars, drop = FALSE]
  }

  # Combine .id with other columns
  df <- data.frame(.id = id_col, data_cols, stringsAsFactors = FALSE)

  # Extract corpus text if texts not provided
  if (is.null(texts)) {
    texts <- as.character(x)
  }

  # Delegate to data.frame method
  as_qlm_coded.data.frame(
    x = df,
    name = name,
    is_gold = is_gold,
    codebook = codebook,
    texts = texts,
    notes = notes,
    metadata = metadata
  )
}


#' @rdname as_qlm_coded
#' @export
as_qlm_coded.default <- function(x,
                                  id,
                                  name = NULL,
                                  is_gold = FALSE,
                                  codebook = NULL,
                                  texts = NULL,
                                  notes = NULL,
                                  metadata = list(),
                                  qlm_segment = FALSE,
                                  source_text = NULL) {
  cli::cli_abort(c(
    "{.arg x} must be a data frame or corpus object.",
    "x" = "You supplied an object of class {.cls {class(x)}}.",
    "i" = "Provide a data frame with an id column and coded variables, or a quanteda corpus with docvars."
  ))
}


#' Convert a data.frame of segments to a segmented corpus
#'
#' Internal helper for `as_qlm_coded(qlm_segment = TRUE)`. Converts a
#' data.frame with a `text` column into a quanteda corpus with character
#' positions computed by `align_segments()`.
#'
#' @param x A data.frame. Must contain a `text` column. All other columns
#'   become docvars.
#' @param source_text A named character vector of source texts. Names must
#'   match values in `x$docid` (if present) or a single unnamed string.
#' @param name Optional name for provenance.
#' @param is_gold Logical. Whether this is a gold standard.
#'
#' @return A quanteda corpus with `qlm_segment` metadata.
#' @keywords internal
#' @noRd
as_qlm_coded_segment <- function(x, source_text, name = NULL, is_gold = FALSE) {
  rlang::check_installed(
    "quanteda",
    reason = "to create a segmented corpus from {.fn as_qlm_coded}"
  )

  if (is.null(source_text) || !is.character(source_text)) {
    cli::cli_abort(c(
      "{.arg source_text} is required when {.code qlm_segment = TRUE}.",
      "i" = "Provide the original unsegmented text as a named character vector."
    ))
  }

  if (!"text" %in% names(x)) {
    cli::cli_abort(c(
      "{.arg x} must contain a {.val text} column when {.code qlm_segment = TRUE}."
    ))
  }

  # Determine source document grouping
  if ("docid" %in% names(x)) {
    doc_ids <- unique(x$docid)
  } else {
    doc_ids <- if (!is.null(names(source_text))) names(source_text) else "text1"
    x$docid <- doc_ids[1L]
  }

  # Ensure source_text is named
  if (is.null(names(source_text))) {
    if (length(source_text) != 1L || length(doc_ids) != 1L) {
      cli::cli_abort(c(
        "{.arg source_text} must be named when {.arg x} contains multiple documents."
      ))
    }
    names(source_text) <- doc_ids[1L]
  }

  # Process each source document
  all_texts    <- character(0)
  all_docnames <- character(0)
  dvar_rows    <- list()

  non_dv_cols <- c("text", "docid")
  dv_cols <- setdiff(names(x), non_dv_cols)

  for (doc_id in doc_ids) {
    rows <- x[x$docid == doc_id, , drop = FALSE]
    src  <- source_text[[doc_id]]

    if (is.null(src) || is.na(src)) {
      cli::cli_abort("No source text found for document {.val {doc_id}}.")
    }

    positions <- align_segments(src, rows$text)

    n_segs <- nrow(rows)
    all_texts    <- c(all_texts, rows$text)
    all_docnames <- c(all_docnames, paste0(doc_id, ".", seq_len(n_segs)))

    dv <- data.frame(
      docid      = rep(doc_id, n_segs),
      segid      = seq_len(n_segs),
      char_start = positions$start,
      char_end   = positions$end,
      stringsAsFactors = FALSE
    )

    for (col in dv_cols) {
      dv[[col]] <- rows[[col]]
    }

    dvar_rows[[length(dvar_rows) + 1L]] <- dv
  }

  all_dvars <- do.call(rbind, dvar_rows)
  out <- quanteda::corpus(all_texts, docnames = all_docnames)
  quanteda::docvars(out) <- all_dvars

  # Set segmentation metadata
  quanteda::meta(out, "qlm_segment") <- TRUE
  quanteda::meta(out, "is_gold") <- is_gold
  quanteda::meta(out, "name") <- name
  quanteda::meta(out, "continuum_lengths") <- stats::setNames(
    nchar(source_text[doc_ids]),
    doc_ids
  )

  out
}
