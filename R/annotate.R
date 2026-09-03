#' Apply an annotation task to input data (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `annotate()` has been deprecated in favor of [qlm_code()]. The new function
#' returns a richer object that includes metadata and settings for reproducibility.
#'
#' @inheritParams qlm_code
#' @param task A task object created with [task()] or [qlm_codebook()].
#'
#' @return A data frame with one row per input element, containing:
#'   \describe{
#'     \item{`id`}{Identifier for each input (from names or sequential integers).}
#'     \item{...}{Additional columns as defined by the task's schema.}
#'   }
#'
#' @seealso
#' [qlm_code()] for the replacement function.
#'
#' @examples
#' \dontrun{
#' # Deprecated usage
#' texts <- c("I love this product!", "This is terrible.")
#' annotate(texts, data_codebook_sentiment, model_name = "openai/gpt-4o-mini")
#'
#' # New recommended usage
#' coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded  # Print as tibble
#' }
#'
#' @keywords internal
#' @export
annotate <- function(.data, task, model_name, ...) {
  lifecycle::deprecate_warn("0.2.0", "annotate()", "qlm_code()")

  coded <- qlm_code(.data, codebook = task, model = model_name, ...)

  # qlm_code() keys its rows on `.id`; annotate() always returned that column
  # as `id`, and callers on the deprecation path still read it by that name.
  out <- as.data.frame(coded)
  names(out)[names(out) == ".id"] <- "id"
  out
}
