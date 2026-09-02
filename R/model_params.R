#' Sampling parameters ellmer standardises
#'
#' The fields of [ellmer::params()], read from the installed ellmer rather than
#' listed here, so a parameter ellmer adds or drops later needs no change.
#'
#' @return A character vector of parameter names.
#' @keywords internal
#' @noRd
model_param_names <- function() {
  setdiff(names(formals(ellmer::params)), "...")
}


#' Names that are not `params()` fields but get typed as though they were
#'
#' Kept separate from `model_param_names()` so that nothing here implies
#' [ellmer::params()] accepts them:
#'
#' * `stop` is OpenAI's body field; ellmer spells the standardised form
#'   `stop_sequences`.
#' * `response_format` is a raw body field. The JSON-mode path sets it itself,
#'   so passing it at the top level is always wrong, while `api_args` remains
#'   the way to set it deliberately.
#'
#' @keywords internal
#' @noRd
model_param_aliases <- c("stop", "response_format")


#' Argument names something in the request path legitimately accepts
#'
#' A name is only worth rejecting if nothing would have taken it. Subtracting
#' this set at run time, rather than trusting a list fixed when quallmer was
#' built, means an installed quallmer running against a newer ellmer that gives
#' one of these names a real meaning stops rejecting it -- without an update
#' here. A test asserts the set is currently disjoint from the candidates, so
#' the reverse change fails loudly during package testing.
#'
#' @param model A model specification, as passed to [qlm_code()].
#'
#' @return A character vector of argument names.
#' @keywords internal
#' @noRd
top_level_arg_names <- function(model) {
  provider_fn <- get0(
    paste0("chat_", model_provider(model)),
    envir = asNamespace("ellmer")
  )

  unique(c(
    names(formals(ellmer::chat)),
    if (!is.null(provider_fn)) names(formals(provider_fn)),
    names(formals(ellmer::parallel_chat_structured)),
    names(formals(ellmer::batch_chat_structured))
  ))
}


#' Reject a model parameter passed at the top level
#'
#' A top-level `temperature` or `max_tokens` falls through to `chat_args` and
#' reaches [ellmer::chat()], which has no such argument, so the call died with
#' `unused argument (max_tokens = 100)` raised from inside ellmer -- naming the
#' argument but neither of the two places it could have gone.
#'
#' Deliberately does not say which of the two to use. The choice is not a
#' property of the name: `top_k` is a real [ellmer::params()] field, but ellmer
#' maps it onto `top_logprobs` for OpenAI-compatible providers, so a caller who
#' wants a provider's raw `top_k` needs `api_args` and one who wants ellmer's
#' standardised form needs `params`. Naming both and letting the caller pick is
#' the honest message. The two aliases are unambiguous and do get a specific
#' note.
#'
#' Values are not quoted back. The name is enough to locate the mistake, and a
#' value may be large or hold a credential.
#'
#' @param dot_names Names of the arguments in `...`.
#' @param model A model specification, as passed to [qlm_code()].
#' @param call The calling environment, for the error message.
#'
#' @return `dot_names`, invisibly.
#' @keywords internal
#' @noRd
check_model_params <- function(dot_names, model, call = rlang::caller_env()) {
  if (length(dot_names) == 0) {
    return(invisible(dot_names))
  }

  candidates <- setdiff(
    c(model_param_names(), model_param_aliases),
    top_level_arg_names(model)
  )
  offenders <- intersect(dot_names, candidates)

  if (length(offenders) == 0) {
    return(invisible(dot_names))
  }

  msg <- c(
    paste0(
      "{cli::qty(length(offenders))}Model parameter{?s} {.arg {offenders}} ",
      "cannot be passed at the top level."
    ),
    "i" = "Standard model parameters go in {.code params = ellmer::params()}.",
    "i" = "Provider-specific request fields go in {.code api_args = list()}."
  )

  if ("stop" %in% offenders) {
    msg <- c(msg, "i" = "ellmer spells {.arg stop} as {.arg stop_sequences} in {.arg params}.")
  }
  if ("response_format" %in% offenders) {
    msg <- c(msg, "i" = paste0(
      "{.arg response_format} belongs in {.arg api_args}; ",
      "JSON-mode coding sets it itself."
    ))
  }

  cli::cli_abort(msg, call = call)
}
