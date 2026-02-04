#' Convert coded data to qlm_coded format
#'
#' Converts a data frame of coded data (human-coded or from external sources)
#' into a `qlm_coded` object. This enables provenance tracking and integration
#' with `qlm_compare()`, `qlm_validate()`, and `qlm_trail()` for coded data
#' alongside LLM-coded results.
#'
#' @param x A data frame containing coded data. Must include a `.id`
#'   column for unit identifiers and one or more coded variables.
#' @param name Character string identifying this coding run (e.g., "Coder_A",
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
#' @param metadata Optional list of metadata about the coding process. Can
#'   include any relevant information such as:
#'   \describe{
#'     \item{`coder_name`}{Name of the human coder}
#'     \item{`coder_id`}{Identifier for the coder}
#'     \item{`training`}{Description of coder training}
#'     \item{`date`}{Date of coding}
#'     \item{`notes`}{Any additional notes}
#'   }
#'   The function automatically adds `timestamp`, `n_units`, and
#'   `source = "human"`.
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
#' # Basic usage
#' human_data <- data.frame(
#'   .id = 1:10,
#'   sentiment = sample(c("pos", "neg"), 10, replace = TRUE)
#' )
#'
#' coder_a <- as_qlm_coded(human_data, name = "Coder_A")
#' coder_a
#'
#' # Create a gold standard
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
#' # No need for gold = when gold object is marked
#' qlm_validate(coder_a, coder_b, gold, by = "sentiment", level = "nominal")
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
as_qlm_coded <- function(
  x,
  name = NULL,
  is_gold = FALSE,
  codebook = NULL,
  texts = NULL,
  metadata = list()
) {
  # Validate inputs
  if (!is.data.frame(x)) {
    cli::cli_abort(c(
      "{.arg x} must be a data frame.",
      "i" = "Provide a data frame with a {.var .id} column and coded variables."
    ))
  }

  if (!".id" %in% names(x)) {
    cli::cli_abort(c(
      "{.arg x} must contain a {.var .id} column.",
      "i" = "Add a {.var .id} column with unique identifiers for each unit."
    ))
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
