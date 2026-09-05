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


#' Why a run's cost will be `NA`, if it will be
#'
#' ellmer prices a turn by exact lookup of the provider's name and the model
#' in a table frozen at its build, and answers `NA` on any miss. It does not
#' say which kind of miss. A provider absent from the table altogether
#' (DeepSeek and six others as of ellmer 0.4.2) will never price any model,
#' and only rates supplied by the user can help. A model newer than the
#' installed ellmer, on a provider it does price, is fixed by upgrading. The
#' remedies differ, so both are told apart here, before a request is spent,
#' rather than left to be inferred from a column of `NA` (#135).
#'
#' Read from the chat the run itself will use, rather than from one built for
#' the purpose: building a chat evaluates the credentials, which for some
#' mechanisms means token discovery or a refresh, and prints ellmer's
#' default-model message. Local endpoints charge nothing per token, so their
#' `NA` is expected rather than a gap, and is described as such from the
#' prefix alone.
#'
#' The table and ellmer's own predicate are read from its namespace at run
#' time, as the model listing is. Should a later ellmer drop either, no
#' diagnosis is made and the `NA` stands unexplained, which is what it did
#' before.
#'
#' @param chat The [ellmer::Chat] the run will use.
#' @param model The model specification, as passed to [qlm_code()].
#'
#' @return `NULL` when the model is priced, or when nothing can be said.
#'   Otherwise a list with `kind`, one of `"local"`, `"provider"` or
#'   `"model"`; `provider`, ellmer's name for it; and `model`.
#' @keywords internal
#' @noRd
unpriced_reason <- function(chat, model) {
  prefix <- model_provider(model)
  if (prefix %in% c("ollama", "lmstudio", "vllm")) {
    return(list(kind = "local", provider = prefix,
                model = sub("^[^/]*/?", "", model)))
  }

  ns <- asNamespace("ellmer")
  prices <- ellmer_prices(ns)
  has_cost <- get0("has_cost", envir = ns, inherits = FALSE)
  if (!is.data.frame(prices) || !is.function(has_cost)) {
    return(NULL)
  }

  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  if (is.null(provider)) {
    return(NULL)
  }
  # The model name lives on the chat; `Provider@model` is deprecated in
  # ellmer 0.5 and kept only as a fallback for a chat that cannot say
  model_name <- tryCatch(chat$get_model(), error = function(e) NULL)
  if (is.null(model_name)) {
    model_name <- suppressWarnings(provider@model)
  }
  # ellmer 0.5 takes the provider's name; earlier versions took the object
  priced <- if (identical(names(formals(has_cost))[1], "provider_name")) {
    has_cost(provider@name, model_name)
  } else {
    has_cost(provider, model_name)
  }
  if (isTRUE(priced)) {
    return(NULL)
  }

  list(
    kind = if (provider@name %in% prices$provider) "model" else "provider",
    provider = provider@name,
    model = model_name
  )
}


#' ellmer's price table, wherever this version keeps it
#'
#' A data frame `prices` in the namespace up to ellmer 0.4; from 0.5 a
#' `prices_get()` accessor. Neither is exported, so both are looked up
#' rather than called by name.
#'
#' @param ns ellmer's namespace.
#'
#' @return The table, or `NULL` when neither is found.
#' @keywords internal
#' @noRd
ellmer_prices <- function(ns) {
  prices <- get0("prices", envir = ns, inherits = FALSE)
  if (is.data.frame(prices)) {
    return(prices)
  }
  getter <- get0("prices_get", envir = ns, inherits = FALSE)
  if (is.function(getter)) {
    return(tryCatch(getter(), error = function(e) NULL))
  }
  NULL
}


#' Diagnose an unpriced run from the chat it will use, and say so once
#'
#' Called by each coding path right after it builds its chat and before it
#' sends anything. The structured path may fall back to the JSON path, which
#' builds a chat of its own; the diagnosis is the same, so the fallback is
#' told not to repeat the message.
#'
#' @param chat The [ellmer::Chat] the path will use.
#' @param model The model specification.
#' @param execution_args The caller's execution arguments, for `include_cost`
#'   and `include_tokens`.
#' @param say Whether to emit the message.
#'
#' @return What `unpriced_reason()` returned, or `NULL` when no cost was
#'   asked for.
#' @keywords internal
#' @noRd
cost_diagnosis <- function(chat, model, execution_args, say = TRUE) {
  if (!isTRUE(execution_args$include_cost)) {
    return(NULL)
  }
  reason <- unpriced_reason(chat, model)
  if (!is.null(reason) && say) {
    cli::cli_inform(
      unpriced_message(reason, tokens_recorded = isTRUE(execution_args$include_tokens))
    )
  }
  reason
}


#' The message for an unpriced run, and the note kept on the object
#'
#' `include_cost` and `include_tokens` are independent ellmer arguments, so
#' whether the token counts a cost could be worked out from are being
#' recorded is a fact about this call, and the message says which.
#'
#' @param reason What `unpriced_reason()` returned.
#' @param tokens_recorded Whether the caller set `include_tokens = TRUE`.
#'
#' @return `unpriced_message()`: a character vector for [cli::cli_inform()].
#'   `unpriced_note()`: one plain sentence, kept in the run's metadata and
#'   shown by `print.qlm_coded()`.
#' @keywords internal
#' @noRd
unpriced_message <- function(reason, tokens_recorded = FALSE) {
  v <- as.character(utils::packageVersion("ellmer"))
  # Interpolated here, where `reason` is in scope, rather than by the caller
  line <- function(...) cli::format_inline(paste0(...))
  tokens <- if (tokens_recorded) {
    line("Token counts are recorded; supply the provider's published rates as ",
         "{.arg prices} to cost the run from them.")
  } else {
    line("Supply the provider's published rates as {.arg prices} to cost the run ",
         "from its token counts.")
  }
  switch(reason$kind,
    local = c(
      "i" = line("{.field cost} will be {.code NA}: {reason$provider} runs locally, ",
                 "and there is no per-token charge to record.")
    ),
    provider = c(
      "i" = line("{.field cost} will be {.code NA}: ellmer {v} has no prices for ",
                 "{reason$provider} models."),
      " " = tokens
    ),
    model = c(
      "i" = line("{.field cost} will be {.code NA}: ellmer {v} has no price for ",
                 "{.val {reason$model}}, though it prices other ",
                 "{reason$provider} models."),
      " " = paste("A newer ellmer may price it.", tokens)
    )
  )
}

unpriced_note <- function(reason) {
  switch(reason$kind,
    local = paste0(reason$provider, " runs locally; no per-token charge"),
    provider = paste0("ellmer has no prices for ", reason$provider, " models"),
    model = paste0("ellmer ", utils::packageVersion("ellmer"),
                   " has no price for ", reason$model)
  )
}
