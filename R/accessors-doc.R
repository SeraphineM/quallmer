#' Accessor functions for quallmer objects
#'
#' Functions to safely access and modify metadata from quallmer objects
#' (`qlm_coded`, `qlm_comparison`, `qlm_validation`, `qlm_codebook`). These
#' functions provide a stable API for accessing object metadata without
#' directly manipulating internal attributes.
#'
#' @name accessors
#'
#' @section Metadata types:
#'
#' quallmer objects store metadata in three categories:
#'
#' **User metadata** (`type = "user"`):
#' - `name`: Run identifier (settable)
#' - `notes`: Descriptive notes (settable)
#' - Plus any custom fields added via `as_qlm_coded(..., metadata = list(...))`
#'
#' **Object metadata** (`type = "object"`):
#' - `call`: Function call that created the object
#' - `parent`: Parent run name (for replications)
#' - `batch`: Whether batch processing was used
#' - `chat_args`: Arguments passed to the LLM chat
#' - `execution_args`: Arguments for parallel/batch execution
#' - `n_units`: Number of coded units
#' - `input_type`: Type of input ("text", "image", or "human")
#' - `source`: Coding source ("human" or "llm")
#' - `is_gold`: Whether this is a gold standard
#'
#' **System metadata** (`type = "system"`):
#' - `timestamp`: When the object was created
#' - `ellmer_version`: Version of ellmer package
#' - `quallmer_version`: Version of quallmer package
#' - `R_version`: Version of R
#'
#' @section Functions:
#'
#' - [qlm_meta()]: Get metadata fields
#' - [qlm_meta<-()]: Set user metadata fields (only `name` and `notes`)
#' - [codebook()]: Extract codebook from coded objects
#' - [inputs()]: Extract original input data
#'
#' @examples
#' \donttest{
#' # Create a coded object
#' texts <- c("I love this!", "Terrible.", "It's okay.")
#' coded <- qlm_code(
#'   texts,
#'   data_codebook_sentiment,
#'   model = "openai/gpt-4o-mini",
#'   name = "run1",
#'   notes = "Initial coding run"
#' )
#'
#' # Access metadata
#' qlm_meta(coded, "name")              # Get run name
#' qlm_meta(coded, type = "user")       # Get all user metadata
#' qlm_meta(coded, type = "system")     # Get system metadata
#'
#' # Modify user metadata
#' qlm_meta(coded, "name") <- "updated_run1"
#' qlm_meta(coded, "notes") <- "Revised notes"
#'
#' # Extract components
#' codebook(coded)                      # Get the codebook
#' inputs(coded)                        # Get original texts
#'
#' # Custom metadata from human coding
#' human_data <- data.frame(
#'   .id = 1:5,
#'   sentiment = c("pos", "neg", "pos", "neg", "pos")
#' )
#' human_coded <- as_qlm_coded(
#'   human_data,
#'   name = "coder_A",
#'   metadata = list(
#'     coder_name = "Dr. Smith",
#'     experience = "5 years"
#'   )
#' )
#'
#' # Access custom metadata
#' qlm_meta(human_coded, "coder_name")  # "Dr. Smith"
#' qlm_meta(human_coded, type = "user") # All user fields
#' }
#'
#' @seealso
#' - [qlm_code()] for creating coded objects
#' - [as_qlm_coded()] for converting human-coded data
#' - [qlm_trail()] for viewing coding history
NULL
