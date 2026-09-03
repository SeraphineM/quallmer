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


#' Explain a rejected run when the model name is the likely cause
#'
#' A model name that does not exist for its provider is the easiest mistake to
#' make and the worst reported: the first sign is an HTTP 4xx whose message
#' varies from a list of valid names (DeepSeek) to a bare "HTTP 400 Bad
#' Request." (#133). Called only once a run has already been rejected in its
#' entirety, so the happy path costs nothing; it then asks the provider for
#' its model list, through ellmer's `models_<provider>()`, and says whether
#' the name is on it.
#'
#' A diagnostic, never a gate. Whenever the lookup cannot run -- the provider
#' publishes no list, ellmer has no `models_*()` for it, no credentials, no
#' network -- nothing is added and the provider's own error stands unchanged.
#' Nothing is added either when the name is on the list, since the cause is
#' then something else, or when the provider's listing is known not to cover
#' every identifier it will invoke, since absence from it then proves
#' nothing; see `listing_is_complete()`. Nor when the name begins a listed
#' one, since that is the shape of an alias; see `may_be_alias()`.
#'
#' @param model The model specification, as passed to [qlm_code()].
#' @param chat_args The run's arguments to [ellmer::chat()]; `base_url`,
#'   `api_key` and `credentials` are passed on to the lookup where it takes
#'   them, so a custom endpoint is asked rather than the provider's default.
#'
#' @return A character vector of cli bullets, empty when there is nothing to
#'   say.
#' @keywords internal
#' @noRd
model_name_hint <- function(model, chat_args = list()) {
  # "provider" alone means the provider's default model, which exists
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !grepl("/", model, fixed = TRUE)) {
    return(character())
  }
  provider <- model_provider(model)
  id <- sub("^[^/]*/", "", model)
  if (!listing_is_complete(provider, id)) {
    return(character())
  }
  models <- provider_models(provider, chat_args)
  if (is.null(models) || id %in% models || may_be_alias(id, models)) {
    return(character())
  }

  lines <- cli::format_inline(
    "{.val {id}} is not a model that {.val {provider}} lists; ",
    "{.fn ellmer::models_{provider}} gives the {length(models)} it does."
  )
  close <- closest_model_names(id, models)
  if (length(close)) {
    lines <- c(lines, cli::format_inline("Did you mean {.val {close}}?"))
  }
  set_bullets(lines, n = Inf, bullet = "i")
}


#' Could a name absent from the listing be an alias of a listed one?
#'
#' Providers accept convenience aliases their listings do not carry.
#' Anthropic's listing gives the dated identifier, `claude-haiku-4-5-20251001`,
#' while the API also takes `claude-haiku-4-5`, and once took `-latest`
#' forms; every alias documented is the listed name with a suffix removed.
#' So a name that begins a listed one is left alone rather than called
#' wrong: the cost of silence is a lost hint for a truncated typo, the cost
#' of a false claim is a valid model made unusable on the structured path,
#' since the claim also stops the JSON-mode fallback. Applied to every
#' provider, since the rule is about the shape of aliases, not one API.
#'
#' @param id The model name as typed, without the prefix.
#' @param models The provider's listed names.
#'
#' @return `TRUE` when some listed name starts with `id`.
#' @keywords internal
#' @noRd
may_be_alias <- function(id, models) {
  stem <- sub("-latest$", "", id)
  nzchar(stem) && any(startsWith(models, stem))
}


#' Does a provider's model listing cover every identifier it will invoke?
#'
#' Absence from a listing proves a name wrong only where the listing is
#' complete, and for some providers it is not. AWS Bedrock's
#' `ListFoundationModels` returns foundation models only, while invocation
#' also accepts cross-region inference profiles (`us.anthropic...`),
#' provisioned, custom and imported models, and ARNs; Google Vertex likewise
#' serves tuned endpoints its publisher list does not name; Portkey and Posit
#' are gateways whose catalogue is not what they will route. Gemini lists
#' its models but not the caller's tuned ones, which live under
#' `tunedModels/`. For all of these a mistyped name gets no diagnosis rather
#' than a false one, which would also stop the JSON-mode fallback that might
#' have succeeded.
#'
#' An allowlist rather than a denylist, so that a provider ellmer adds later
#' loses only the hint until it is checked, never gains a false claim.
#'
#' @param provider The provider prefix.
#' @param id The model name as typed, without the prefix.
#'
#' @return `TRUE` when a name absent from the listing can be called wrong.
#' @keywords internal
#' @noRd
listing_is_complete <- function(provider, id) {
  if (!provider %in% complete_listings) {
    return(FALSE)
  }
  !(provider == "google_gemini" && startsWith(id, "tunedModels/"))
}

# Providers whose models_*() enumerates every identifier the endpoint will
# invoke, checked 2026-09-03 against ellmer 0.4.2.
complete_listings <- c(
  "anthropic", "claude", "deepseek", "github", "google_gemini", "groq",
  "lmstudio", "mistral", "ollama", "openai", "vllm"
)


#' The model names a provider publishes, or `NULL`
#'
#' Asks ellmer's `models_<provider>()`, a credentialed network call whose
#' answer depends on the account asking: fine-tuned and gated models differ
#' by key, by project and by region. So it is asked afresh each time and
#' never cached across runs, where a stale or foreign answer could pin a
#' wrong diagnosis to a repaired credential. It is asked at most once per
#' failed run, which is the only time it is asked at all.
#'
#' @param provider The provider prefix.
#' @param chat_args The run's arguments to [ellmer::chat()]; those the lister
#'   takes (`base_url`, `api_key`, `credentials`, and for some providers
#'   `profile`, `project_id` or `location`) are passed on.
#'
#' @return A character vector of model ids, or `NULL` when the provider has no
#'   `models_*()` in ellmer or the lookup failed.
#' @keywords internal
#' @noRd
provider_models <- function(provider, chat_args = list()) {
  lister <- models_lister(provider)
  if (is.null(lister)) {
    return(NULL)
  }
  args <- chat_args[intersect(names(chat_args), names(formals(lister)))]
  tryCatch(
    {
      listing <- suppressMessages(suppressWarnings(do.call(lister, args)))
      ids <- listing$id
      if (is.character(ids) && length(ids)) ids else NULL
    },
    error = function(e) NULL
  )
}


#' ellmer's model-listing function for a provider, or `NULL`
#'
#' Looked up in ellmer's exports at run time, as `ellmer::chat()` looks up
#' `chat_<provider>()`, so a listing ellmer adds later is used without a
#' change here. Fifteen of ellmer's providers have one; the rest are
#' bring-your-own-endpoint providers for which a list would be meaningless.
#'
#' @param provider The provider prefix.
#'
#' @return A function, or `NULL`.
#' @keywords internal
#' @noRd
models_lister <- function(provider) {
  fn <- paste0("models_", provider)
  if (!fn %in% getNamespaceExports("ellmer")) {
    return(NULL)
  }
  get(fn, envir = asNamespace("ellmer"))
}


#' The published model names nearest a mistyped one
#'
#' Edit distance, case-insensitive, keeping only names within half the length
#' of the one typed (and never fewer than two edits), so that a name unlike
#' anything on the list gets no suggestion rather than a misleading one.
#' Each listed name is also compared as its alias, without a trailing date,
#' since that is the form people type: against a dated listing,
#' `claude-haiku-4-6` is one edit from `claude-haiku-4-5`, not ten from
#' `claude-haiku-4-5-20251001`. The listed name is what is suggested.
#'
#' @param id The model name as typed.
#' @param models The provider's model names.
#' @param n At most this many suggestions.
#'
#' @return A character vector, possibly empty, nearest first.
#' @keywords internal
#' @noRd
closest_model_names <- function(id, models, n = 3L) {
  stems <- sub("-[0-9]{8}$", "", models)
  d <- pmin(
    utils::adist(tolower(id), tolower(models))[1, ],
    utils::adist(tolower(id), tolower(stems))[1, ]
  )
  near <- d <= max(2, ceiling(nchar(id) / 2))
  candidates <- models[near][order(d[near])]
  utils::head(unique(candidates), n)
}
