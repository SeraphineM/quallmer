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
  expect_false("qwen" %in% providers)

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
    check_model_provider("qwen/qwen3-max"),
    "Can't reach provider"
  )

  err <- tryCatch(check_model_provider("qwen/qwen3-max"), error = function(e) e)
  msg <- cli::ansi_strip(paste(conditionMessage(err), collapse = "\n"))

  # The three things the bare ellmer error left the user without.
  expect_match(msg, "qwen", fixed = TRUE)
  expect_match(msg, "openai_compatible/<model>", fixed = TRUE)
  expect_match(msg, "base_url", fixed = TRUE)
})


test_that("check_model_provider() names every provider, without cli truncation (#129)", {
  err <- tryCatch(check_model_provider("qwen/qwen3-max"), error = function(e) e)
  msg <- cli::ansi_strip(paste(conditionMessage(err), collapse = "\n"))

  # cli abbreviates a vector this long by default, which would silently drop
  # names from the one message whose job is to list them.
  for (provider in ellmer_providers()) {
    expect_match(msg, provider, fixed = TRUE)
  }
  expect_false(grepl("\u2026", msg))
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
    qlm_code(c("great", "awful"), cb, model = "qwen/qwen3-max"),
    "Can't reach provider"
  )
  expect_error(
    qlm_code(c("great", "awful"), cb, model = "kimi"),
    "Can't reach provider"
  )

  skip_if_not_installed("quanteda")
  expect_error(
    qlm_segment("A sentence. Another one.", cb, model = "qwen/qwen3-max"),
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
