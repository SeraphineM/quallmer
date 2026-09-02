test_codebook <- function() {
  qlm_codebook(
    "sentiment", "Code the sentiment.",
    ellmer::type_object(polarity = ellmer::type_string("pos or neg"))
  )
}

all_candidates <- function() c(model_param_names(), model_param_aliases)


test_that("model_param_names() comes from ellmer::params(), not a fixed list", {
  expect_equal(model_param_names(), setdiff(names(formals(ellmer::params)), "..."))
  expect_false("..." %in% model_param_names())
  expect_true(all(c("temperature", "max_tokens", "top_p") %in% model_param_names()))
})


test_that("the aliases are deliberately not params() fields", {
  # The whole point of keeping them separate: listing them alongside
  # model_param_names() would imply ellmer::params() accepts them.
  expect_false(any(model_param_aliases %in% model_param_names()))
  expect_true("stop_sequences" %in% model_param_names())
  expect_false("stop" %in% model_param_names())
})


test_that("qlm_code() rejects every model parameter passed at the top level (#139)", {
  cb <- test_codebook()

  for (nm in all_candidates()) {
    args <- list("great", cb, model = "openai/gpt-4o-mini")
    args[[nm]] <- 1
    expect_error(
      do.call(qlm_code, args),
      "cannot be passed at the top level",
      info = nm
    )
  }
})


test_that("qlm_segment() rejects them too, sharing the routing contract (#139)", {
  skip_if_not_installed("quanteda")
  cb <- test_codebook()

  for (nm in all_candidates()) {
    args <- list("A sentence. Another one.", cb, model = "openai/gpt-4o-mini")
    args[[nm]] <- 1
    expect_error(
      do.call(qlm_segment, args),
      "cannot be passed at the top level",
      info = nm
    )
  }
})


test_that("the message names both destinations and never the value (#139)", {
  cb <- test_codebook()
  err <- tryCatch(
    qlm_code("great", cb, model = "openai/gpt-4o-mini", max_tokens = 4242),
    error = function(e) e
  )
  msg <- cli::ansi_strip(paste(conditionMessage(err), collapse = "\n"))

  expect_match(msg, "max_tokens", fixed = TRUE)
  expect_match(msg, "params = ellmer::params()", fixed = TRUE)
  expect_match(msg, "api_args = list()", fixed = TRUE)

  # Values are not quoted back: one may be large, or hold a credential.
  expect_false(grepl("4242", msg, fixed = TRUE))

  # Deliberately does not prescribe one destination. `top_k` is a real params
  # field that ellmer maps onto `top_logprobs` for OpenAI-compatible
  # providers, so "use params" would be wrong advice for a raw `top_k`.
  expect_false(grepl("must go in", msg, fixed = TRUE))
})


test_that("every offending name is listed, not just the first (#139)", {
  cb <- test_codebook()
  err <- tryCatch(
    qlm_code("great", cb, model = "openai/gpt-4o-mini", max_tokens = 1, temperature = 0),
    error = function(e) e
  )
  msg <- cli::ansi_strip(paste(conditionMessage(err), collapse = "\n"))

  expect_match(msg, "max_tokens", fixed = TRUE)
  expect_match(msg, "temperature", fixed = TRUE)
  expect_match(msg, "parameters", fixed = TRUE)  # pluralised
})


test_that("the two unambiguous aliases get a specific destination (#139)", {
  cb <- test_codebook()

  stop_msg <- cli::ansi_strip(paste(conditionMessage(tryCatch(
    qlm_code("great", cb, model = "openai/gpt-4o-mini", stop = c("END")),
    error = function(e) e
  )), collapse = "\n"))
  expect_match(stop_msg, "stop_sequences", fixed = TRUE)

  rf_msg <- cli::ansi_strip(paste(conditionMessage(tryCatch(
    qlm_code("great", cb, model = "openai/gpt-4o-mini", response_format = "json"),
    error = function(e) e
  )), collapse = "\n"))
  expect_match(rf_msg, "api_args", fixed = TRUE)
  expect_match(rf_msg, "JSON-mode coding sets it itself", fixed = TRUE)
})


test_that("provider and credential arguments still pass through (#139)", {
  # The regression that would matter most: rejecting one of these would break
  # every OpenAI-compatible endpoint, and `api_key` is how a key held in a
  # keyring reaches the provider.
  expect_silent(check_model_params(
    c("params", "api_args", "base_url", "credentials", "api_key",
      "api_headers", "echo"),
    "openai/gpt-4o-mini"
  ))
})


test_that("execution arguments still pass through, including batch-only ones (#139)", {
  # `path` and `wait` belong to batch_chat_structured() rather than the
  # parallel path. Checked here rather than through a real `batch = TRUE`
  # call, which would issue a request.
  expect_silent(check_model_params(
    c("max_active", "rpm", "on_error", "convert", "include_tokens"),
    "openai/gpt-4o-mini"
  ))
  expect_silent(check_model_params(c("path", "wait", "ignore_hash"), "openai/gpt-4o-mini"))
})


test_that("check_model_params() is a no-op with no dots", {
  expect_silent(check_model_params(NULL, "openai/gpt-4o-mini"))
  expect_silent(check_model_params(character(0), "openai/gpt-4o-mini"))
  expect_equal(check_model_params(NULL, "openai/gpt-4o-mini"), NULL)
})


test_that("a name ellmer starts accepting stops being rejected, with no change here (#139)", {
  # The installed-quallmer-against-newer-ellmer case, which the collision test
  # below cannot reach: if a future ellmer gives `max_tokens` a real meaning at
  # the top level, the runtime subtraction picks that up.
  local_mocked_bindings(
    top_level_arg_names = function(model) {
      c(names(formals(ellmer::chat)), "max_tokens")
    }
  )

  expect_silent(check_model_params("max_tokens", "openai/gpt-4o-mini"))
  # Everything else still rejected.
  expect_error(
    check_model_params("temperature", "openai/gpt-4o-mini"),
    "cannot be passed at the top level"
  )
})


test_that("no candidate is currently accepted anywhere in the request path (#139)", {
  # The canary. Rejecting these is only safe while nothing in the request path
  # takes them; if an ellmer update changes that, this fails during package
  # testing rather than in a user's session.
  candidates <- all_candidates()

  for (provider in ellmer_providers()) {
    clash <- intersect(candidates, top_level_arg_names(paste0(provider, "/m")))
    expect_equal(clash, character(0), info = provider)
  }
})


test_that("a malformed model is still reported before a stray parameter (#139)", {
  cb <- test_codebook()

  # Validation precedence from #144: the model is the more fundamental problem
  # when both are wrong.
  expect_error(
    qlm_code("great", cb, model = c("openai", "anthropic"), max_tokens = 1),
    "must be a single string"
  )
  expect_error(
    qlm_code("great", cb, model = "quallmer_unknown_provider_7f3c/m", max_tokens = 1),
    "Can't reach provider"
  )
})
