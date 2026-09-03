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

clear_model_list_cache <- function() {
  rm(list = ls(model_list_cache), envir = model_list_cache)
}

test_that("models_lister() finds ellmer's listing function, or nothing", {
  expect_identical(models_lister("openai"), ellmer::models_openai)
  expect_identical(models_lister("deepseek"), ellmer::models_deepseek)
  # Bring-your-own-endpoint providers publish no list
  expect_null(models_lister("openai_compatible"))
  expect_null(models_lister("not_a_provider"))
})

test_that("provider_models() asks once per provider and endpoint, negatives included", {
  clear_model_list_cache()
  on.exit(clear_model_list_cache())
  calls <- new.env()
  lister <- function(base_url = "https://api.example", api_key = NULL, credentials = NULL) {
    calls$n <- (calls$n %||% 0L) + 1L
    calls$base_url <- base_url
    if (grepl("down", base_url)) stop("HTTP 503 Service Unavailable.")
    data.frame(id = c("m-large", "m-small"), created_at = Sys.Date())
  }
  f <- provider_models
  mockery::stub(f, "models_lister", function(provider) if (provider == "acme") lister else NULL)

  expect_equal(f("acme"), c("m-large", "m-small"))
  expect_equal(f("acme"), c("m-large", "m-small"))
  expect_equal(calls$n, 1)

  # Only the arguments the lister takes travel, and a custom endpoint is a
  # different cache entry
  expect_equal(
    f("acme", chat_args = list(base_url = "https://eu.example", params = list(), api_args = list())),
    c("m-large", "m-small")
  )
  expect_equal(calls$n, 2)
  expect_equal(calls$base_url, "https://eu.example")

  # A failed lookup is NULL, and is not re-tried on the next report
  expect_null(f("acme", chat_args = list(base_url = "https://down.example")))
  expect_null(f("acme", chat_args = list(base_url = "https://down.example")))
  expect_equal(calls$n, 3)

  # No lister, no lookup
  expect_null(f("openai_compatible"))
})

test_that("closest_model_names() suggests near misses and nothing for the rest", {
  models <- c("gpt-4o-mini", "gpt-4o", "gpt-4.1", "o3-mini", "text-embedding-3-small")
  expect_equal(closest_model_names("gpt-4o-mimi", models)[1], "gpt-4o-mini")
  expect_equal(closest_model_names("GPT-4O", models)[1], "gpt-4o")
  expect_length(closest_model_names("gpt-4o-mimi", models, n = 1L), 1)
  expect_length(closest_model_names("llama-3.3-70b-versatile", models), 0)
})

test_that("model_name_hint() speaks only when the provider has no such model", {
  clear_model_list_cache()
  on.exit(clear_model_list_cache())
  f <- model_name_hint
  mockery::stub(f, "provider_models", function(provider, chat_args) {
    if (provider == "acme") c("m-large", "m-small") else NULL
  })

  hint <- f("acme/m-larg")
  expect_named(hint, c("i", "i"))
  expect_match(hint[[1]], "\"m-larg\" is not a model that \"acme\" lists")
  expect_match(hint[[1]], "ellmer::models_acme\\(\\)")
  expect_match(hint[[2]], "Did you mean \"m-large\"")

  # A name unlike anything on the list gets the list pointer but no guess
  hint <- f("acme/completely-different")
  expect_length(hint, 1)

  # Listed: the cause is something else, so nothing is added
  expect_length(f("acme/m-large"), 0)
  # No list to consult: nothing is claimed either way
  expect_length(f("openai_compatible/m-large"), 0)
  # A bare provider means its default model, which exists
  expect_length(f("acme"), 0)
  expect_length(f(NA_character_), 0)
})

test_that("a diagnosis never reaches the network in tests", {
  # The lookups above are all stubbed; the real lister must fail closed when
  # it cannot run, returning nothing rather than raising
  clear_model_list_cache()
  on.exit(clear_model_list_cache())
  withr::local_envvar(OPENAI_API_KEY = NA)
  f <- provider_models
  mockery::stub(f, "models_lister", function(provider) function(...) stop("no key"))
  expect_null(f("openai"))
})
