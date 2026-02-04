#' Convert human-coded data to qlm_coded format (deprecated)
#'
#' This function is retained for backwards compatibility. New code should use
#' [as_qlm_coded()] instead, which provides the same functionality with an
#' additional `is_gold` parameter for marking gold standards.
#'
#' @param x A data frame containing human-coded data. Must include a `.id`
#'   column for unit identifiers and one or more coded variables.
#' @param name Character string identifying this coding run (e.g., "Coder_A",
#'   "expert_rater"). Default is `NULL`.
#' @param codebook Optional list containing coding instructions.
#' @param texts Optional vector of original texts or data that were coded.
#' @param notes Optional character string with descriptive notes.
#' @param metadata Optional list of metadata about the coding process.
#'
#' @return A `qlm_humancoded` object (inherits from `qlm_coded`).
#'
#' @seealso [as_qlm_coded()] for the current recommended function.
#'
#' @keywords internal
#' @export
qlm_humancoded <- function(
  x,
  name = NULL,
  codebook = NULL,
  texts = NULL,
  notes = NULL,
  metadata = list()
) {
  as_qlm_coded(
    x = x,
    name = name,
    is_gold = FALSE,
    codebook = codebook,
    texts = texts,
    notes = notes,
    metadata = metadata
  )
}
