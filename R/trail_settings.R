#' Trail settings specification
#'
#' Simple description of an LLM configuration for use with quallmer Trails.
#'
#' @param provider Character. Backend provider identifier, e.g. "openai",
#'   "ollama", "azure", etc.
#' @param model Character. Model identifier, e.g. "gpt-4.1-mini",
#'   "gpt-4o-mini", "llama3.2:1b".
#' @param temperature Numeric scalar. Sampling temperature (default 0).
#' @param extra Named list of extra arguments merged into `api_args`.
#'
#' @return An object of class \code{"trail_setting"}.
#' @export
trail_settings <- function(
    provider    = "openai",
    model       = "gpt-4.1-mini",
    temperature = 0,
    extra       = list()
) {
  if (!is.character(provider) || length(provider) != 1L || !nzchar(provider)) {
    stop("`provider` must be a non-empty character scalar.")
  }
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("`model` must be a non-empty character scalar.")
  }
  if (!is.numeric(temperature) || length(temperature) != 1L) {
    stop("`temperature` must be a numeric scalar.")
  }
  if (!is.list(extra)) {
    stop("`extra` must be a list.")
  }

  structure(
    list(
      provider    = provider,
      model       = model,
      temperature = temperature,
      extra       = extra
    ),
    class = "trail_setting"
  )
}

#' @export
print.trail_setting <- function(x, ...) {
  cat("Trail setting\n")
  cat("  Provider:   ", x$provider, "\n", sep = "")
  cat("  Model:      ", x$model,    "\n", sep = "")
  cat("  Temp:       ", x$temperature, "\n", sep = "")
  if (length(x$extra)) {
    cat("  Extra:      ", paste(names(x$extra), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
