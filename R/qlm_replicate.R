#' Replicate a coding task
#'
#' Re-executes a coding task from a `qlm_coded` object, optionally with
#' modified settings. If no overrides are provided, uses identical settings
#' to the original coding.
#'
#' @param x A `qlm_coded` object.
#' @param ... Optional overrides passed to [qlm_code()], such as `temperature`
#'   or `max_tokens`.
#' @param codebook Optional replacement codebook. If `NULL` (default), uses
#'   the codebook from `x`.
#' @param model Optional replacement model (e.g., `"openai/gpt-4o"`). If `NULL`
#'   (default), uses the model from `x`.
#' @param batch Optional logical to override batch processing setting. If `NULL`
#'   (default), uses the batch setting from `x`. Set to `TRUE` to use batch
#'   processing or `FALSE` to use parallel processing, regardless of the
#'   original setting.
#' @param name Optional name for this run. If `NULL`, defaults to the model
#'   name (if changed) or `"replication_N"` where N is the replication count.
#' @param notes Optional character string with descriptive notes about this
#'   replication. Useful for documenting why this replication was run or what
#'   differs from the original. Default is `NULL`.
#'
#' @return A `qlm_coded` object with `run$parent` set to the parent's run name.
#'
#' @seealso [qlm_code()] for initial coding, [qlm_compare()] for comparing
#'   replicated results.
#'
#' @examples
#' \donttest{
#' # First create a coded object
#' texts <- c("I love this!", "Terrible.", "It's okay.")
#' coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini", name = "run1")
#'
#' # Replicate with same model
#' coded2 <- qlm_replicate(coded, name = "run2")
#'
#' # Compare results
#' qlm_compare(coded, coded2, by = "sentiment", level = "nominal")
#' }
#'
#' @importFrom utils modifyList
#' @export
qlm_replicate <- function(x, ..., codebook = NULL, model = NULL, batch = NULL, name = NULL, notes = NULL) {
  # Input validation
  if (!inherits(x, "qlm_coded")) {
    cli::cli_abort("{.arg x} must be a {.cls qlm_coded} object.")
  }

  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  # Extract original components
  original_data <- attr(x, "data")
  meta_attr <- attr(x, "meta")
  original_codebook <- attr(x, "codebook")
  original_model <- meta_attr$object$chat_args$name
  # Ensure it's always a list (empty if NULL)
  original_execution_args <- meta_attr$object$execution_args %||% list()
  # Extract batch flag (default to FALSE for backward compatibility)
  original_batch <- meta_attr$object$batch %||% FALSE
  parent_name <- meta_attr$user$name

  # Apply batch override if provided, otherwise use original
  use_batch <- batch %||% original_batch

  # Capture the current call
  current_call <- match.call()

  # Apply overrides (NULL means use original)
  use_codebook <- codebook %||% original_codebook
  use_model <- model %||% original_model

  # Determine run name
  if (is.null(name)) {
    if (!is.null(model) && model != original_model) {
      # Use new model name as run name
      name <- sub(".*/", "", model)  # extract model part after provider/
    } else {
      # Generate replication name
      name <- paste0("replication_",
                     sum(grepl("^replication_", c(parent_name))) + 1)
    }
  }

  # Merge additional overrides with original execution_args
  overrides <- list(...)
  execution_args <- modifyList(original_execution_args, overrides)

  # Call qlm_code with merged arguments, including batch flag
  result <- do.call(qlm_code, c(
    list(
      x = original_data,
      codebook = use_codebook,
      model = use_model,
      batch = use_batch,
      name = name,
      notes = notes
    ),
    execution_args
  ))

  # Override the metadata to reflect this is a replication
  result_meta <- attr(result, "meta")
  result_meta$object$call <- current_call
  result_meta$object$parent <- parent_name
  attr(result, "meta") <- result_meta

  result
}
