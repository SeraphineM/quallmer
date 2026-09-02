#' Provider prefix of a model specification
#'
#' `qlm_code()` and `qlm_segment()` take `model` in ellmer's
#' `"provider/model"` form, or `"provider"` alone. This returns the provider
#' part, which is what dispatch and several defaults key on.
#'
#' @param model A model specification, e.g. `"openai/gpt-4o-mini"`.
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
model_provider <- function(model) {
  sub("/.*$", "", model)
}


#' Providers reachable by name through ellmer
#'
#' [ellmer::chat()] dispatches on the model string's prefix: it looks up
#' `chat_<provider>()` in ellmer's namespace and additionally requires that
#' function to take `model`, `system_prompt` and `params`, calling anything
#' else unsupported. Both gates are mirrored here so that this list is exactly
#' the set of prefixes that can work.
#'
#' Derived from the installed ellmer at run time rather than hard-coded, so a
#' provider ellmer adds or drops later needs no change here.
#'
#' @return A sorted character vector of provider names, without the `chat_`
#'   prefix.
#' @keywords internal
#' @noRd
ellmer_providers <- function() {
  ns <- asNamespace("ellmer")
  fns <- grep("^chat_", getNamespaceExports("ellmer"), value = TRUE)

  # The same three formals ellmer::chat() insists on before dispatching.
  dispatchable <- vapply(
    fns,
    function(fn) {
      all(c("model", "system_prompt", "params") %in% names(formals(get(fn, envir = ns))))
    },
    logical(1)
  )

  sort(sub("^chat_", "", fns[dispatchable]))
}


#' Fail early when a model's provider cannot be reached by name
#'
#' Without this, `model = "qwen/qwen3-max"` reaches [ellmer::chat()] and aborts
#' with `Can't find provider ellmer::chat_qwen()`, which leaks an ellmer
#' internal and says nothing about what to do instead. Every provider ellmer
#' has no `chat_*()` for is reachable as `openai_compatible/<model>` with a
#' `base_url`, so the error names both the prefixes that work and that route.
#'
#' Checked before any request rather than by interpreting a failure, because
#' the answer is known without asking the provider anything.
#'
#' @param model A model specification, as passed to [qlm_code()].
#' @param call The calling environment, for the error message.
#'
#' @return `model`, invisibly.
#' @keywords internal
#' @noRd
check_model_provider <- function(model, call = rlang::caller_env()) {
  # Anything that is not a single string is not this function's business:
  # callers validate that themselves, or ellmer's own `check_string()` does.
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return(invisible(model))
  }

  provider <- model_provider(model)
  providers <- ellmer_providers()

  if (provider %in% providers) {
    return(invisible(model))
  }

  # cli truncates a vector this long by default ("openrouter", ..., "vllm"),
  # which would hide several of the very names the message exists to give.
  providers <- cli::cli_vec(providers, style = list("vec-trunc" = Inf))

  cli::cli_abort(
    c(
      "Can't reach provider {.val {provider}} by name.",
      "i" = "{.fn ellmer::chat} dispatches on the prefix of {.arg model}, and ellmer has no {.fn ellmer::chat_{provider}}.",
      "i" = "Reachable by name: {.val {providers}}.",
      "i" = "Any other OpenAI-compatible endpoint is reachable as {.code openai_compatible/<model>} with {.arg base_url} and {.arg credentials}.",
      "i" = "See the {.strong Provider-specific parameters} section of {.code ?qlm_code} for a worked example."
    ),
    call = call
  )
}
