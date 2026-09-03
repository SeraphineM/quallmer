# A prefix ellmer will never export a `chat_*()` for.
#
# Naming a real provider here -- "qwen", "kimi" -- would assert that it stays
# unsupported, so these tests would start failing the day ellmer adds it. That
# is the opposite of the property this file exists to check: that the supported
# set is derived from the installed ellmer and needs no change here. See #129.
unknown_provider <- "quallmer_unknown_provider_7f3c"


test_that("model_provider() takes the prefix, or the whole string", {
  expect_equal(model_provider("openai/gpt-4o-mini"), "openai")
  expect_equal(model_provider("openai"), "openai")
  # ellmer joins the remainder back with "/", so only the first "/" separates.
  expect_equal(model_provider("openai_compatible/org/model-1"), "openai_compatible")
})


test_that("ellmer_providers() reports the prefixes ellmer::chat() can dispatch on", {
  providers <- ellmer_providers()

  expect_type(providers, "character")
  expect_true(length(providers) > 0)
  expect_equal(providers, sort(providers))
  expect_false(anyDuplicated(providers) > 0)

  # Named rather than counted: a count would break every time ellmer adds a
  # provider, which is the churn deriving the list at run time avoids.
  expect_true(all(c("openai", "anthropic", "openai_compatible") %in% providers))
  expect_false(unknown_provider %in% providers)

  # Every name reported must round-trip to a function ellmer exports.
  expect_true(all(paste0("chat_", providers) %in% getNamespaceExports("ellmer")))
})


test_that("ellmer_providers() excludes anything ellmer::chat() would refuse", {
  # ellmer::chat() requires `model`, `system_prompt` and `params` before it
  # dispatches, so a chat_*() lacking them is not reachable by name even
  # though it exists.
  required <- c("model", "system_prompt", "params")
  for (provider in ellmer_providers()) {
    fn <- get(paste0("chat_", provider), envir = asNamespace("ellmer"))
    expect_true(all(required %in% names(formals(fn))), info = provider)
  }
})


test_that("check_model_provider() explains an unreachable provider (#129)", {
  expect_error(
    check_model_provider(paste0(unknown_provider, "/some-model")),
    "Can't reach provider"
  )

  err <- tryCatch(check_model_provider(paste0(unknown_provider, "/some-model")), error = function(e) e)
  msg <- cli::ansi_strip(paste(conditionMessage(err), collapse = "\n"))

  # The three things the bare ellmer error left the user without.
  expect_match(msg, unknown_provider, fixed = TRUE)
  expect_match(msg, "openai_compatible/<model>", fixed = TRUE)
  expect_match(msg, "base_url", fixed = TRUE)
})


test_that("check_model_provider() names every provider, without cli truncation (#129)", {
  err <- tryCatch(check_model_provider(paste0(unknown_provider, "/some-model")), error = function(e) e)
  msg <- cli::ansi_strip(paste(conditionMessage(err), collapse = "\n"))

  # cli abbreviates a vector this long by default, which would silently drop
  # names from the one message whose job is to list them.
  for (provider in ellmer_providers()) {
    expect_match(msg, provider, fixed = TRUE)
  }
  expect_false(grepl("\u2026", msg))
})


test_that("check_model_provider() accepts every provider ellmer reports", {
  # The forward-compatible half of the same property: if ellmer gains
  # chat_qwen(), it appears in ellmer_providers() and is accepted here with no
  # change to this file.
  for (provider in ellmer_providers()) {
    expect_silent(check_model_provider(provider))
    expect_silent(check_model_provider(paste0(provider, "/some-model")))
  }
})


test_that("check_model_provider() passes anything reachable by name", {
  expect_silent(check_model_provider("openai/gpt-4o-mini"))
  expect_silent(check_model_provider("openai"))
  expect_silent(check_model_provider("anthropic/claude-sonnet-4-5"))
  expect_silent(check_model_provider("openai_compatible/qwen3-max"))

  expect_equal(check_model_provider("openai"), "openai")
})


test_that("check_model_provider() leaves non-strings to the caller's own check", {
  # qlm_code() reports these itself, and ellmer's check_string() covers the
  # rest; duplicating that here would report the wrong problem.
  expect_silent(check_model_provider(NULL))
  expect_silent(check_model_provider(character(0)))
  expect_silent(check_model_provider(NA_character_))
  expect_silent(check_model_provider(c("openai", "anthropic")))
  expect_silent(check_model_provider(42))
})


test_that("qlm_code() and qlm_segment() reject an unreachable provider before requesting (#129)", {
  cb <- qlm_codebook(
    "sentiment", "Code the sentiment.",
    ellmer::type_object(polarity = ellmer::type_string("pos or neg"))
  )

  # No request is made, so these need no credentials and no network.
  expect_error(
    qlm_code(c("great", "awful"), cb, model = paste0(unknown_provider, "/some-model")),
    "Can't reach provider"
  )
  expect_error(
    qlm_code(c("great", "awful"), cb, model = unknown_provider),
    "Can't reach provider"
  )

  skip_if_not_installed("quanteda")
  expect_error(
    qlm_segment("A sentence. Another one.", cb, model = paste0(unknown_provider, "/some-model")),
    "Can't reach provider"
  )
})


test_that("qlm_code() still reports a malformed model before looking at the provider", {
  cb <- qlm_codebook(
    "sentiment", "Code the sentiment.",
    ellmer::type_object(polarity = ellmer::type_string("pos or neg"))
  )

  expect_error(
    qlm_code(c("great", "awful"), cb, model = c("openai", "anthropic")),
    "must be a single string"
  )
})


# Model-name diagnosis (#133) --------------------------------------------------

test_that("models_lister() finds ellmer's listing function, or nothing", {
  expect_identical(models_lister("openai"), ellmer::models_openai)
  expect_identical(models_lister("deepseek"), ellmer::models_deepseek)
  # Bring-your-own-endpoint providers publish no list
  expect_null(models_lister("openai_compatible"))
  expect_null(models_lister("not_a_provider"))
})

test_that("provider_models() asks afresh each time, with the run's own credentials", {
  calls <- new.env()
  lister <- function(base_url = "https://api.example", api_key = NULL, credentials = NULL) {
    calls$n <- (calls$n %||% 0L) + 1L
    calls$base_url <- base_url
    if (grepl("down", base_url)) stop("HTTP 503 Service Unavailable.")
    data.frame(id = paste0("model-for-", api_key %||% "default"), created_at = Sys.Date())
  }
  f <- provider_models
  mockery::stub(f, "models_lister", function(provider) if (provider == "acme") lister else NULL)

  # The answer depends on who asks, so nothing is remembered between calls
  expect_equal(f("acme", list(api_key = "account-A")), "model-for-account-A")
  expect_equal(f("acme", list(api_key = "account-B")), "model-for-account-B")
  expect_equal(calls$n, 2)

  # Only the arguments the lister takes travel
  expect_equal(
    f("acme", chat_args = list(base_url = "https://eu.example", params = list(), api_args = list())),
    "model-for-default"
  )
  expect_equal(calls$base_url, "https://eu.example")

  # A failed lookup is NULL, and a later call after the cause is repaired asks again
  expect_null(f("acme", chat_args = list(base_url = "https://down.example")))
  expect_equal(f("acme"), "model-for-default")

  # No lister, no lookup
  expect_null(f("openai_compatible"))
  expect_equal(calls$n, 5)
})

test_that("provider_models() passes project, location and profile where the lister takes them", {
  seen <- NULL
  f <- provider_models
  mockery::stub(f, "models_lister", function(provider) {
    switch(provider,
      google_vertex = function(location = NULL, project_id = NULL, credentials = NULL) {
        seen <<- list(location = location, project_id = project_id)
        data.frame(id = "gemini-x")
      },
      aws_bedrock = function(profile = NULL, base_url = NULL) {
        seen <<- list(profile = profile)
        data.frame(id = "anthropic.claude-x")
      }
    )
  })
  f("google_vertex", list(location = "europe-west1", project_id = "p-1", api_key = "unused"))
  expect_equal(seen, list(location = "europe-west1", project_id = "p-1"))
  f("aws_bedrock", list(profile = "research"))
  expect_equal(seen, list(profile = "research"))
})

test_that("listing_is_complete() trusts absence only where the listing is exhaustive", {
  expect_true(listing_is_complete("openai", "gpt-4o-mimi"))
  expect_true(listing_is_complete("anthropic", "claude-sonnet-4"))
  expect_true(listing_is_complete("google_gemini", "gemini-2.5-pro"))
  # Bedrock invokes inference profiles, ARNs and custom models its listing omits
  expect_false(listing_is_complete("aws_bedrock", "us.anthropic.claude-3-5-sonnet-20241022-v2:0"))
  expect_false(listing_is_complete("aws_bedrock", "anthropic.claude-3-5-sonnet-20241022-v2:0"))
  # Tuned Gemini models live outside the listing
  expect_false(listing_is_complete("google_gemini", "tunedModels/my-classifier"))
  expect_false(listing_is_complete("google_vertex", "gemini-2.5-pro"))
  expect_false(listing_is_complete("portkey", "gpt-4o"))
  # A provider not yet checked gets no claim rather than a false one
  expect_false(listing_is_complete("some_new_provider", "m"))
})

test_that("closest_model_names() suggests near misses and nothing for the rest", {
  models <- c("gpt-4o-mini", "gpt-4o", "gpt-4.1", "o3-mini", "text-embedding-3-small")
  expect_equal(closest_model_names("gpt-4o-mimi", models)[1], "gpt-4o-mini")
  expect_equal(closest_model_names("GPT-4O", models)[1], "gpt-4o")
  # Compared as the alias people type, suggesting the listed name
  dated <- c("claude-haiku-4-5-20251001", "claude-sonnet-4-5-20250929")
  expect_equal(closest_model_names("claude-haiku-4-6", dated)[1], "claude-haiku-4-5-20251001")
  expect_length(closest_model_names("gpt-4o-mimi", models, n = 1L), 1)
  expect_length(closest_model_names("llama-3.3-70b-versatile", models), 0)
})

test_that("model_name_hint() speaks only when the provider has no such model", {
  f <- model_name_hint
  mockery::stub(f, "provider_models", function(provider, chat_args) {
    if (provider == "acme") c("m-large", "m-small") else NULL
  })
  mockery::stub(f, "listing_is_complete", function(provider, id) provider == "acme")

  hint <- f("acme/m-lrage")
  expect_named(hint, c("i", "i"))
  expect_match(hint[[1]], "\"m-lrage\" is not a model that \"acme\" lists")
  expect_match(hint[[1]], "ellmer::models_acme\\(\\)")
  expect_match(hint[[2]], "Did you mean \"m-large\"")

  # A name unlike anything on the list gets the list pointer but no guess
  hint <- f("acme/completely-different")
  expect_length(hint, 1)
  # A name that begins a listed one may be an alias, so it gets no claim
  expect_length(f("acme/m-larg"), 0)

  # Listed: the cause is something else, so nothing is added
  expect_length(f("acme/m-large"), 0)
  # No list to consult: nothing is claimed either way
  expect_length(f("openai_compatible/m-large"), 0)
  # A bare provider means its default model, which exists
  expect_length(f("acme"), 0)
  expect_length(f(NA_character_), 0)
})

test_that("may_be_alias() treats a name that begins a listed one as possibly valid", {
  listed <- c("claude-haiku-4-5-20251001", "claude-sonnet-4-5-20250929", "claude-opus-5")
  expect_true(may_be_alias("claude-haiku-4-5", listed))
  expect_true(may_be_alias("claude-sonnet-4-5-latest", listed))
  expect_true(may_be_alias("claude-opus-5", listed))
  expect_false(may_be_alias("claude-haiku-4-6", listed))
  expect_false(may_be_alias("claude-haiku-4-5-20251002", listed))
  expect_false(may_be_alias("", listed))
  expect_false(may_be_alias("-latest", listed))
})

test_that("model_name_hint() leaves an Anthropic alias alone against a canonical-only listing", {
  # Anthropic lists dated identifiers; the API also accepts the undated
  # alias, which a listing-only check would have called wrong and, worse,
  # used to stop the JSON-mode fallback
  f <- model_name_hint
  mockery::stub(f, "provider_models", function(provider, chat_args) {
    c("claude-opus-4-5-20251101", "claude-haiku-4-5-20251001", "claude-sonnet-4-5-20250929")
  })
  expect_length(f("anthropic/claude-haiku-4-5"), 0)
  expect_length(f("claude/claude-sonnet-4-5"), 0)
  expect_length(f("anthropic/claude-sonnet-4-5-latest"), 0)
  # A name that begins nothing listed is still a typo, with the nearest names
  hint <- f("anthropic/claude-haiku-4-6")
  expect_match(hint[[1]], "\"claude-haiku-4-6\" is not a model")
  expect_match(hint[[2]], "claude-haiku-4-5-20251001")
})

test_that("model_name_hint() makes no claim where the listing is not exhaustive (Bedrock)", {
  f <- model_name_hint
  asked <- FALSE
  mockery::stub(f, "provider_models", function(provider, chat_args) {
    asked <<- TRUE
    "anthropic.claude-3-5-sonnet-20241022-v2:0"
  })
  # A valid cross-region inference profile, absent from ListFoundationModels:
  # calling it wrong would also have stopped the JSON-mode fallback
  expect_length(f("aws_bedrock/us.anthropic.claude-3-5-sonnet-20241022-v2:0"), 0)
  expect_false(asked)
})

test_that("a diagnosis never reaches the network in tests", {
  # The lookups above are all stubbed; the real lister must fail closed when
  # it cannot run, returning nothing rather than raising
  withr::local_envvar(OPENAI_API_KEY = NA)
  f <- provider_models
  mockery::stub(f, "models_lister", function(provider) function(...) stop("no key"))
  expect_null(f("openai"))
})


# unpriced_reason() -----------------------------------------------------------

# A credentials function keeps construction off the environment and sends
# nothing: ellmer builds the provider without contacting it.
no_creds <- list(credentials = function() list(Authorization = "Bearer x"))

test_that("unpriced_reason() is NULL for a model ellmer prices (#135)", {
  expect_null(unpriced_reason("openai/gpt-4.1-mini", no_creds))
  expect_null(unpriced_reason("anthropic/claude-sonnet-4-5", no_creds))
})

test_that("unpriced_reason() tells a provider gap from a model gap (#135)", {
  # DeepSeek is absent from ellmer's table as a whole
  r <- unpriced_reason("deepseek/deepseek-chat", no_creds)
  expect_equal(r$kind, "provider")
  expect_equal(r$provider, "DeepSeek")
  expect_equal(r$model, "deepseek-chat")

  # OpenAI is priced, this model is not
  r <- unpriced_reason("openai/gpt-99-not-yet-released", no_creds)
  expect_equal(r$kind, "model")
  expect_equal(r$provider, "OpenAI")
  expect_equal(r$model, "gpt-99-not-yet-released")
})

test_that("unpriced_reason() names a local endpoint without building a chat (#135)", {
  # ellmer's ollama constructor looks for a running server; none is here
  r <- unpriced_reason("ollama/llama3")
  expect_equal(r$kind, "local")
  expect_equal(r$provider, "ollama")
  expect_equal(r$model, "llama3")
  expect_equal(unpriced_reason("vllm/x")$kind, "local")
  expect_equal(unpriced_reason("lmstudio/x")$kind, "local")
})

test_that("unpriced_reason() stays silent where it cannot decide (#135)", {
  # The chat cannot be built: openai_compatible needs a base_url. The run
  # itself will say so; nothing to add.
  expect_null(unpriced_reason("openai_compatible/qwen3", no_creds))

  # ellmer without the price table or its predicate: no diagnosis
  f <- unpriced_reason
  mockery::stub(f, "get0", function(x, ...) NULL)
  expect_null(f("deepseek/deepseek-chat", no_creds))
})

test_that("unpriced_message() and unpriced_note() cover every kind (#135)", {
  provider <- list(kind = "provider", provider = "DeepSeek", model = "deepseek-chat")
  model <- list(kind = "model", provider = "OpenAI", model = "gpt-99")
  local <- list(kind = "local", provider = "ollama", model = "llama3")

  expect_match(unpriced_message(provider)[["i"]], "no prices for DeepSeek models")
  expect_match(unpriced_message(model)[["i"]], "no price for \"gpt-99\"")
  expect_match(unpriced_message(model)[["i"]], "other OpenAI models")
  expect_match(unpriced_message(local)[["i"]], "runs locally")
  expect_length(unpriced_message(provider), 2)

  expect_equal(unpriced_note(provider), "ellmer has no prices for DeepSeek models")
  expect_match(unpriced_note(model), "^ellmer [0-9.]+ has no price for gpt-99$")
  expect_equal(unpriced_note(local), "ollama runs locally; no per-token charge")
})
