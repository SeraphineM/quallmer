#' Code qualitative data with an LLM
#'
#' Applies a codebook to input data using a large language model, returning
#' a rich object that includes the codebook, execution settings, results, and
#' metadata for reproducibility.
#'
#' Arguments in `...` are dynamically routed to either [ellmer::chat()],
#' [ellmer::parallel_chat_structured()], or [ellmer::batch_chat_structured()]
#' based on their names.
#'
#' @param x Input data: a character vector of texts (for text codebooks) or
#'   file paths to images (for image codebooks). Named vectors will use names
#'   as identifiers in the output; unnamed vectors will use sequential integers.
#' @param codebook A codebook object created with [qlm_codebook()]. Also accepts
#'   deprecated [task()] objects for backward compatibility.
#' @param model Provider (and optionally model) name in the form
#'   `"provider/model"` or `"provider"` (which will use the default model for
#'   that provider). Passed to the `name` argument of [ellmer::chat()].
#'   Examples: `"openai/gpt-4o-mini"`, `"anthropic/claude-3-5-sonnet-20241022"`,
#'   `"ollama/llama3.2"`, `"openai"` (uses default OpenAI model).
#' @param batch Logical. If `TRUE`, uses [ellmer::batch_chat_structured()]
#'   instead of [ellmer::parallel_chat_structured()]. Batch processing is more
#'   cost-effective for large jobs but may have longer turnaround times.
#'   Default is `FALSE`. See [ellmer::batch_chat_structured()] for details.
#' @param ... Additional arguments passed to [ellmer::chat()],
#'   [ellmer::parallel_chat_structured()], or [ellmer::batch_chat_structured()].
#'   Arguments recognized by [ellmer::parallel_chat_structured()] or
#'   [ellmer::batch_chat_structured()] are routed there; all other arguments
#'   (including provider-specific arguments like `base_url`, `credentials`, or
#'   `api_args` for OpenAI-compatible endpoints) are passed to [ellmer::chat()].
#' @param name Character string identifying this coding run. Default is `NULL`.
#' @param notes Optional character string with descriptive notes about this
#'   coding run. Useful for documenting the purpose or rationale when viewing
#'   results in [qlm_trail()]. Default is `NULL`.
#'
#' @details
#' Progress indicators and error handling are provided by the underlying
#' [ellmer::parallel_chat_structured()] or [ellmer::batch_chat_structured()]
#' function. Set `verbose = TRUE` to see progress messages during coding.
#' Retry logic for API failures should be configured through ellmer's options.
#'
#' When `batch = TRUE`, the function uses [ellmer::batch_chat_structured()]
#' which submits jobs to the provider's batch API. This is typically more
#' cost-effective but has longer turnaround times. The `path` argument specifies
#' where batch results are cached, `wait` controls whether to wait for completion,
#' and `ignore_hash` can force reprocessing of cached results.
#'
#' @return A `qlm_coded` object (a tibble with additional attributes):
#'   \describe{
#'     \item{Data columns}{The coded results with a `.id` column for identifiers.}
#'     \item{Attributes}{`data`, `input_type`, and `run` (list containing name, batch, call, codebook, chat_args, execution_args, metadata, parent).}
#'   }
#'   The object prints as a tibble and can be used directly in data manipulation workflows.
#'   The `batch` flag in the `run` attribute indicates whether batch processing was used.
#'   The `execution_args` contains all non-chat execution arguments (for either parallel or batch processing).
#'
#' @seealso
#' [qlm_codebook()] for creating codebooks, [qlm_replicate()] for replicating
#' coding runs, [qlm_compare()] and [qlm_validate()] for assessing reliability.
#'
#' @examples
#' @examples
#' # Requires API credentials and internet access; not run in package checks.
#' \dontrun{
#' # Basic sentiment analysis
#' texts <- c("I love this product!", "Terrible experience.", "It's okay.")
#' coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded
#'
#' # With named inputs (names become IDs in output)
#' texts_named <- c(review1 = "Great service!", review2 = "Very disappointing.")
#' coded2 <- qlm_code(texts_named, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded2
#' }
#'
#' @export
qlm_code <- function(x, codebook, model, ..., batch = FALSE, name = NULL, notes = NULL) {
  # Accept both qlm_codebook and task objects, converting if needed
  if (inherits(codebook, "task") && !inherits(codebook, "qlm_codebook")) {
    codebook <- as_qlm_codebook(codebook)
  }

  if (!inherits(codebook, "qlm_codebook")) {
    cli::cli_abort(c(
      "{.arg codebook} must be created using {.fn qlm_codebook}.",
      "i" = "Use {.fn qlm_codebook} or one of the predefined codebook functions."
    ))
  }

  # Input validation
  if (codebook$input_type == "text" && !is.character(x)) {
    cli::cli_abort("This codebook expects text input (a character vector).")
  }
  if (codebook$input_type == "image" && !is.character(x)) {
    cli::cli_abort("This codebook expects image file paths (a character vector).")
  }

  # Get valid argument names from ellmer functions
  chat_arg_names <- names(formals(ellmer::chat))
  pcs_arg_names <- names(formals(ellmer::parallel_chat_structured))
  batch_arg_names <- names(formals(ellmer::batch_chat_structured))

  # Common model parameters that should go in params
  model_param_names <- c("temperature", "max_tokens", "top_p", "top_k",
                         "frequency_penalty", "presence_penalty", "stop",
                         "seed", "response_format")

  # Route ... arguments
  # execution_args go to parallel_chat_structured or batch_chat_structured
  # Everything else (including provider-specific args like base_url) goes to chat()
  dots <- list(...)
  dot_names <- names(dots)

  # execution_args contains arguments for parallel_chat_structured or batch_chat_structured
  execution_arg_names <- unique(c(pcs_arg_names, batch_arg_names))
  execution_args <- dots[dot_names %in% execution_arg_names]

  # chat_args gets everything NOT destined for execution functions

  # This allows provider-specific args (base_url, credentials, api_args, etc.)
  # to pass through to ellmer::chat() which forwards them to the provider
  chat_args <- dots[!dot_names %in% execution_arg_names]

  # Build system prompt from role and instructions
  system_prompt <- if (!is.null(codebook$role)) {
    paste(codebook$role, codebook$instructions, sep = "\n\n")
  } else {
    codebook$instructions
  }

  # Build chat object using ellmer::chat()
  chat <- do.call(ellmer::chat, c(
    list(
      name = model,
      system_prompt = system_prompt
    ),
    chat_args
  ))

  # Prepare prompts based on input type
  if (codebook$input_type == "image") {
    prompts <- lapply(x, ellmer::content_image_file)
  } else {
    prompts <- as.list(x)
  }

  # Execute with appropriate function based on batch parameter
  if (batch) {
    results <- do.call(ellmer::batch_chat_structured, c(
      list(
        chat = chat,
        prompts = prompts,
        type = codebook$schema
      ),
      execution_args
    ))
  } else {
    results <- do.call(ellmer::parallel_chat_structured, c(
      list(
        chat = chat,
        prompts = prompts,
        type = codebook$schema
      ),
      execution_args
    ))
  }

  # Add ID column from input names or sequence
  results$id <- names(x) %||% seq_along(x)

  # Build metadata list
  metadata <- list(
    timestamp = Sys.time(),
    n_units = length(x),
    notes = notes,
    ellmer_version = tryCatch(
      as.character(utils::packageVersion("ellmer")),
      error = function(e) NA_character_
    ),
    quallmer_version = tryCatch(
      as.character(utils::packageVersion("quallmer")),
      error = function(e) NA_character_
    ),
    R_version = paste(R.version$major, R.version$minor, sep = ".")
  )

  # Add model to chat_args for easy access
  chat_args$name <- model

  # Create and return qlm_coded object
  new_qlm_coded(
    results = results,
    codebook = codebook,
    data = x,
    input_type = codebook$input_type,
    chat_args = chat_args,
    execution_args = execution_args,
    batch = batch,
    metadata = metadata,
    name = name,
    call = match.call(),
    parent = NULL
  )
}

#' Create a qlm_coded object (internal)
#'
#' Low-level constructor for qlm_coded objects. This function is not exported
#' and is intended for internal use by [qlm_code()] and [qlm_replicate()].
#'
#' The object is a tibble with additional qlm_coded class and attributes.
#'
#' @param results Data frame of coded results with id column.
#' @param codebook A qlm_codebook object.
#' @param data The original input data (x from qlm_code).
#' @param input_type Type of input ("text" or "image").
#' @param chat_args List of arguments passed to ellmer::chat().
#' @param execution_args List of arguments passed to ellmer::parallel_chat_structured()
#'   or ellmer::batch_chat_structured(). For backward compatibility, also accepts
#'   pcs_args as an alias.
#' @param batch Logical indicating whether batch processing was used.
#' @param metadata List of metadata (timestamp, versions, etc.).
#' @param name Character string identifying this run.
#' @param call The call that created this object.
#' @param parent Character string identifying parent run (NULL for originals).
#' @param pcs_args Deprecated. Use execution_args instead.
#'
#' @return A qlm_coded object (tibble with attributes).
#' @importFrom utils head
#' @keywords internal
#' @noRd
new_qlm_coded <- function(results, codebook, data, input_type, chat_args,
                           execution_args = NULL, batch = FALSE, metadata,
                           name, call, parent = NULL, pcs_args = NULL) {
  # Backward compatibility: if pcs_args is provided but execution_args is not
  if (is.null(execution_args) && !is.null(pcs_args)) {
    execution_args <- pcs_args
  }
  # Rename id column to .id
  names(results)[names(results) == "id"] <- ".id"

  # Reorder columns to put .id first
  results <- results[, c(".id", setdiff(names(results), ".id"))]

  # Convert to tibble (always available via ellmer)
  results <- tibble::as_tibble(results)

  # Add qlm_coded class and attributes with new metadata structure
  # Build object metadata - include source and is_gold if present
  object_meta <- list(
    batch = batch,
    call = call,
    chat_args = chat_args,
    execution_args = execution_args,
    parent = parent,
    n_units = metadata$n_units,
    input_type = input_type
  )

  # Add source and is_gold from metadata if present (for human-coded data)
  if (!is.null(metadata$source)) {
    object_meta$source <- metadata$source
  }
  if (!is.null(metadata$is_gold)) {
    object_meta$is_gold <- metadata$is_gold
  }

  # Build user metadata: start with name and notes, then add custom metadata
  user_meta <- list(
    name = name,
    notes = metadata$notes
  )

  # Add any custom metadata fields (exclude system, object, and user-handled fields)
  system_fields <- c("timestamp", "ellmer_version", "quallmer_version", "R_version")
  object_fields <- c("n_units", "source", "is_gold")
  user_handled <- c("name", "notes")
  exclude_fields <- c(system_fields, object_fields, user_handled)

  custom_metadata <- metadata[!names(metadata) %in% exclude_fields]
  if (length(custom_metadata) > 0) {
    user_meta <- c(user_meta, custom_metadata)
  }

  structure(
    results,
    class = c("qlm_coded", class(results)),
    data = data,
    codebook = codebook,
    meta = list(
      user = user_meta,
      object = object_meta,
      system = list(
        timestamp = metadata$timestamp,
        ellmer_version = metadata$ellmer_version,
        quallmer_version = metadata$quallmer_version,
        R_version = metadata$R_version
      )
    )
  )
}


#' Print a qlm_coded object
#'
#' @param x A qlm_coded object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects (printing to console).
#' @keywords internal
#' @export
print.qlm_coded <- function(x, ...) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  codebook_attr <- attr(x, "codebook")

  # Print header
  cat("# quallmer coded object\n")
  cat("# Run:      ", meta_attr$user$name, "\n", sep = "")

  # Distinguish human vs LLM coding
  if (!is.null(meta_attr$object$source) && meta_attr$object$source == "human") {
    cat("# Source:   Human coder\n")
    if (!is.null(codebook_attr$name) && codebook_attr$name != "Human-coded data") {
      cat("# Codebook: ", codebook_attr$name, "\n", sep = "")
    }
  } else {
    cat("# Codebook: ", codebook_attr$name, "\n", sep = "")
    cat("# Model:    ", meta_attr$object$chat_args$name %||% "unknown", "\n", sep = "")
  }

  # Show if this is a gold standard
  if (!is.null(meta_attr$object$is_gold) && isTRUE(meta_attr$object$is_gold)) {
    cat("# Gold:     Yes\n")
  }

  cat("# Units:    ", meta_attr$object$n_units, "\n", sep = "")

  if (!is.null(meta_attr$object$parent)) {
    cat("# Parent:   ", meta_attr$object$parent, "\n", sep = "")
  }

  # Show notes if present
  if (!is.null(meta_attr$user$notes)) {
    cat("# Notes:    ", meta_attr$user$notes, "\n", sep = "")
  }

  cat("\n")

  # Print data using parent class method
  NextMethod()
}

