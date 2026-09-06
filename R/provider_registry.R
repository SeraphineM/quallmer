#' Register an OpenAI-compatible endpoint
#'
#' Register a provider prefix for use in `model = "provider/model"` with
#' [qlm_code()] and [qlm_segment()]. Registration lasts for this R session.
#' Native providers in the installed ellmer always take precedence.
#'
#' @param provider A lower-case prefix containing letters, digits, underscores
#'   or hyphens, starting with a letter.
#' @param base_url An HTTP(S) API base URL, without credentials, a query or a
#'   fragment. Include the API path, for example `"https://example.org/v1"`.
#' @param api_key_env Name of the environment variable holding the API key.
#'   ellmer reads its value when building the chat and making requests, and
#'   sends it as a bearer token.
#' @param overwrite Replace an existing registration? Defaults to `FALSE`.
#'   Native ellmer providers cannot be replaced.
#'
#' @details
#' Built-in prefixes are `dashscope` (Alibaba Model Studio, Singapore),
#' `dashscope-cn` (Beijing), `moonshot` (Moonshot's international endpoint),
#' and `zai` (Z.AI's general API, not its Coding Plan endpoint).
#' Both DashScope prefixes read `DASHSCOPE_API_KEY`; Moonshot reads
#' `MOONSHOT_API_KEY`, and Z.AI reads `ZHIPU_API_KEY`.
#' Keys must belong to the endpoint's region. Model names are passed through
#' unchanged, including any slashes; the registry does not maintain a model
#' catalogue, capability list or prices. Supply `prices` to [qlm_code()]
#' when needed.
#'
#' Use `batch = FALSE` with registered prefixes: ellmer currently has no
#' batch submission method for its OpenAI-compatible provider. Registration
#' supplies routing and credentials, not additional transport capabilities.
#'
#' Explicit `credentials` (or ellmer's deprecated `api_key`) overrides the
#' registered environment variable. Overriding `base_url` to a different URL
#' requires explicit credentials, so a registered key cannot travel to another
#' endpoint by accident. Use `credentials = function() ""` for an endpoint
#' requiring no authentication.
#'
#' Runs record the requested prefix and the effective
#' `openai_compatible/model`, base URL and credential source. Replication and
#' backfill reuse the recorded endpoint without consulting the registry.
#' Supplying a replacement `model` resolves that model afresh. Registry
#' entries contain environment variable names, never key values. A replication
#' that overrides only the endpoint retains the original requested model,
#' labelled "endpoint overridden" in print and trail output; its recorded
#' `base_url` is the effective endpoint used for replay.
#'
#' @seealso [qlm_code()], whose `model` argument takes a registered prefix.
#' @return The endpoint definition, invisibly.
#' @export
#' @examples
#' qlm_register_provider("my_gateway", "https://example.org/v1", "GATEWAY_KEY")
#' \dontrun{
#' # Another endpoint, using its own API key
#' qlm_register_provider(
#'   "minimax", "https://api.minimax.io/v1", "MINIMAX_API_KEY"
#' )
#' qlm_code(texts, codebook, model = "dashscope/qwen-plus")
#' qlm_code(texts, codebook, model = "my_gateway/organisation/model")
#' }
qlm_register_provider <- function(provider, base_url, api_key_env,
                                  overwrite = FALSE) {
  if (!rlang::is_string(provider) || !grepl("^[a-z][a-z0-9_-]*$", provider)) {
    cli::cli_abort("{.arg provider} must be a lower-case provider prefix.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be {.code TRUE} or {.code FALSE}.")
  }
  if (provider %in% ellmer_providers()) {
    cli::cli_abort("Cannot replace native ellmer provider {.val {provider}}.")
  }
  base_url <- check_registry_url(base_url)
  if (!rlang::is_string(api_key_env) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", api_key_env)) {
    cli::cli_abort("{.arg api_key_env} must be an environment variable name.")
  }
  if (!overwrite && !is.null(provider_definition(provider))) {
    cli::cli_abort("Provider {.val {provider}} is already registered; use {.code overwrite = TRUE} to replace it.")
  }
  entry <- list(base_url = base_url, api_key_env = api_key_env)
  provider_registry[[provider]] <- entry
  invisible(entry)
}

# Session state contains definitions only; no credential values or closures.
provider_registry <- new.env(parent = emptyenv())

provider_definition <- function(provider) {
  if (!nzchar(provider)) {
    return(NULL)
  }
  custom <- provider_registry[[provider]]
  if (!is.null(custom)) {
    return(custom)
  }
  switch(provider,
    dashscope = list(base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1",
                     api_key_env = "DASHSCOPE_API_KEY"),
    `dashscope-cn` = list(base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1",
                        api_key_env = "DASHSCOPE_API_KEY"),
    moonshot = list(base_url = "https://api.moonshot.ai/v1",
                   api_key_env = "MOONSHOT_API_KEY"),
    zai = list(base_url = "https://api.z.ai/api/paas/v4",
               api_key_env = "ZHIPU_API_KEY"),
    NULL
  )
}

check_registry_url <- function(base_url) {
  if (!rlang::is_string(base_url) ||
      !grepl("^https?://[^/?#@[:space:]]+(/[^?#@[:space:]]*)?$", base_url)) {
    cli::cli_abort("{.arg base_url} must be an HTTP(S) URL without credentials, a query or a fragment.")
  }
  sub("/+$", "", base_url)
}

resolve_provider <- function(model, args = list()) {
  check_model_provider(model)
  if (!rlang::is_string(model)) {
    return(list(model = model, args = args, resolution = NULL))
  }
  provider <- model_provider(model)
  if (provider %in% ellmer_providers()) {
    return(list(model = model, args = args, resolution = NULL))
  }
  entry <- provider_definition(provider)
  model_id <- sub("^[^/]*/", "", model)
  if (!grepl("/", model, fixed = TRUE) || !nzchar(model_id)) {
    cli::cli_abort("Registered provider {.val {provider}} requires a model: {.code {paste0(provider, '/<model>')}}.")
  }
  base_url <- if ("base_url" %in% names(args)) {
    check_registry_url(args$base_url)
  } else entry$base_url
  explicit_credentials <- !is.null(args$credentials) || !is.null(args$api_key)
  if (!identical(base_url, entry$base_url) && !explicit_credentials) {
    cli::cli_abort(c(
      "Changing the registered {.arg base_url} requires explicit {.arg credentials}.",
      "i" = "The registered API key belongs to its original endpoint."
    ))
  }
  args$base_url <- base_url
  if (!explicit_credentials) {
    args$credentials <- as.function(
      list(substitute(Sys.getenv(env), list(env = entry$api_key_env))),
      envir = baseenv()
    )
  }
  effective_model <- paste0("openai_compatible/", model_id)
  list(model = effective_model, args = args, resolution = list(
    requested_model = model, effective_model = effective_model,
    api_key_env = if (!explicit_credentials) entry$api_key_env else NULL
  ))
}

# Keep the original request visible without implying its prefix still names
# the effective endpoint. The URL remains in chat_args for replay/redaction.
provider_request_label <- function(resolution) {
  paste0(resolution$requested_model,
         if (isTRUE(resolution$endpoint_overridden)) " (endpoint overridden)")
}
