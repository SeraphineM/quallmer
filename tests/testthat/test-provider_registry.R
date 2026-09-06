local_registry <- function(env = parent.frame()) {
  old <- as.list(provider_registry)
  withr::defer({
    rm(list = ls(provider_registry), envir = provider_registry)
    list2env(old, envir = provider_registry)
  }, envir = env)
}

registry_codebook <- function() {
  qlm_codebook("test", "Classify", ellmer::type_object(label = ellmer::type_string()))
}

test_that("registration validates configuration and cannot replace native providers", {
  local_registry()
  for (bad in c("", "a/b", "UPPER", "1name")) {
    expect_error(qlm_register_provider(bad, "https://example.org/v1", "KEY"), "prefix")
  }
  for (bad in c("file:///tmp", "https://user:key@example.org", "https://x?key=x", "https://x#f")) {
    expect_error(qlm_register_provider("gateway", bad, "KEY"), "HTTP")
  }
  expect_error(qlm_register_provider("gateway", "https://x", "secret value"), "variable name")
  expect_error(qlm_register_provider("openai", "https://x", "KEY", TRUE), "native")
  qlm_register_provider("gateway", "https://example.org/v1/", "KEY")
  expect_error(qlm_register_provider("gateway", "https://x", "KEY"), "already")
  qlm_register_provider("gateway", "http://localhost:8000/v1", "NEW_KEY", TRUE)
  expect_equal(provider_definition("gateway")$api_key_env, "NEW_KEY")
})

test_that("resolution preserves model suffixes, native priority and lazy credentials", {
  local_registry()
  qlm_register_provider("gateway", "https://example.org/v1", "REGISTRY_TEST_KEY")
  result <- resolve_provider("gateway/org/model")
  expect_identical(result$model, "openai_compatible/org/model")
  expect_identical(result$args$base_url, "https://example.org/v1")
  expect_identical(environment(result$args$credentials), baseenv())
  withr::local_envvar(REGISTRY_TEST_KEY = "first")
  expect_identical(result$args$credentials(), "first")
  Sys.setenv(REGISTRY_TEST_KEY = "second")
  expect_identical(result$args$credentials(), "second")
  expect_identical(redact_credentials(result$args$credentials), result$args$credentials)
  expect_error(resolve_provider("gateway"), "requires a model")
  expect_error(resolve_provider("gateway/"), "requires a model")
  provider_registry$openai <- provider_definition("gateway")
  expect_null(resolve_provider("openai/gpt-4.1-mini")$resolution)
  expect_length(resolve_provider("openai/gpt-4.1-mini")$args, 0)
})

test_that("URL overrides cannot redirect registered credentials", {
  expect_error(resolve_provider("dashscope/qwen-plus", list(base_url = "https://other/v1")),
               "explicit")
  expect_error(resolve_provider("dashscope/qwen-plus", list(base_url = NULL)), "HTTP")
  key <- function() "explicit"
  resolved <- resolve_provider("dashscope/qwen-plus", list(base_url = "https://other/v1", credentials = key))
  expect_identical(resolved$args$credentials, key)
  expect_null(resolved$resolution$api_key_env)
  resolved <- resolve_provider("dashscope/qwen-plus", list(api_key = "explicit"))
  expect_null(resolved$args$credentials)
})

test_that("coding and replay freeze the endpoint and retain safe provenance", {
  local_registry()
  qlm_register_provider("gateway", "https://original.example/v1", "ORIGINAL_KEY")
  calls <- new.env()
  local_mocked_bindings(structured_chat_turns = turns_stub(list(json_turn(list(label = "yes"))), calls))
  x <- qlm_code("text", registry_codebook(), "gateway/org/model", structured = "structured")
  meta <- attr(x, "meta")$object
  expect_identical(meta$chat_args$name, "openai_compatible/org/model")
  expect_identical(meta$provider_resolution$requested_model, "gateway/org/model")
  qlm_register_provider("gateway", "https://replacement.example/v1", "REPLACEMENT_KEY", TRUE)
  y <- qlm_replicate(x)
  expect_identical(attr(y, "meta")$object$chat_args, meta$chat_args)
  expect_identical(attr(y, "meta")$object$provider_resolution, meta$provider_resolution)
  z <- suppressMessages(qlm_replicate(x, model = "gateway/org/model"))
  expect_identical(attr(z, "meta")$object$chat_args$base_url, "https://replacement.example/v1")
  expect_identical(body(attr(z, "meta")$object$chat_args$credentials), quote(Sys.getenv("REPLACEMENT_KEY")))
  trail <- qlm_trail(x)
  expect_output(print(trail), "gateway/org/model")
  rm("gateway", envir = provider_registry)
  replay <- qlm_replicate(trail$runs[[1]]$coded)
  expect_identical(attr(replay, "meta")$object$chat_args, meta$chat_args)
})

test_that("backfill records resolved replacements for replay without a registry", {
  local_registry()
  qlm_register_provider("gateway", "https://example.org/v1", "GATEWAY_KEY")
  local_mocked_bindings(structured_chat_turns = turns_stub(list(json_turn(list(label = "yes")))))
  x <- qlm_code("text", registry_codebook(), "moonshot/kimi", structured = "structured")
  x$label <- NA_character_
  x$.error <- list(simpleError("invalid"))
  y <- suppressMessages(qlm_backfill(x, model = "gateway/org/model", passes = 1))
  pass <- attr(y, "meta")$object$backfill[[1]]
  expect_identical(pass$model, "openai_compatible/org/model")
  expect_identical(pass$overrides$base_url, "https://example.org/v1")
  expect_identical(pass$provider_resolution$requested_model, "gateway/org/model")
  rm("gateway", envir = provider_registry)
  replay <- suppressMessages(replay_backfill(x, y))
  expect_identical(replay$label, "yes")
  expect_identical(attr(replay, "meta")$object$backfill[[1]]$provider_resolution,
                   pass$provider_resolution)
})

# This checks argument routing with mocked transports, not batch support.
# ellmer currently has no batch_submit method for ProviderOpenAICompatible.
test_that("structured transport dispatch receives the resolved chat", {
  seen <- list()
  local_mocked_bindings(structured_chat_turns = function(chat, prompts, type, batch, execution_args) {
    p <- chat$get_provider()
    seen[[length(seen) + 1L]] <<- list(url = p@base_url, credentials = p@credentials, batch = batch)
    list(json_turn(list(label = "yes")))
  })
  withr::local_envvar(DASHSCOPE_API_KEY = "test-key")
  for (batch in c(FALSE, TRUE)) {
    qlm_code("text", registry_codebook(), "dashscope/org/model", batch = batch,
             structured = "structured", path = if (batch) tempfile() else NULL)
  }
  expect_identical(vapply(seen, `[[`, logical(1), "batch"), c(FALSE, TRUE))
  expect_true(all(vapply(seen, function(x) x$url == provider_definition("dashscope")$base_url, logical(1))))
  expect_true(all(vapply(seen, function(x) x$credentials() == "test-key", logical(1))))
})

test_that("JSON fallback uses the same endpoint and credential source", {
  seen <- NULL
  local_mocked_bindings(
    structured_chat_turns = turns_stub(list(json_turn(list(wrong = "field")))),
    json_chat_turns = function(chat, prompts, pc_args) {
      seen <<- chat$get_provider()
      turn_records(list(text_turn('{"label":"yes"}')))
    }
  )
  expect_warning(x <- qlm_code("text", registry_codebook(), "zai/glm-test", json_retries = 0),
                 "falling back")
  expect_identical(x$label, "yes")
  expect_identical(seen@base_url, provider_definition("zai")$base_url)
  expect_identical(body(seen@credentials), quote(Sys.getenv("ZHIPU_API_KEY")))
})

test_that("segmentation resolves registered providers and records their identity", {
  skip_if_not_installed("quanteda")
  seen <- NULL
  local_mocked_bindings(parallel_chat_structured = function(chat, prompts, type, ...) {
    seen <<- chat$get_provider()
    tibble::tibble(segments = list(tibble::tibble(text = "text", label = "yes")))
  }, .package = "ellmer")
  x <- qlm_segment("text", registry_codebook(), "moonshot/kimi")
  expect_identical(seen@base_url, provider_definition("moonshot")$base_url)
  expect_identical(quanteda::meta(x, "provider_resolution")$requested_model, "moonshot/kimi")
})

test_that("trail exports preserve the endpoint and callback without reading keys", {
  withr::local_envvar(MOONSHOT_API_KEY = "registry-secret-must-not-be-saved")
  local_mocked_bindings(structured_chat_turns = turns_stub(list(json_turn(list(label = "yes")))))
  x <- qlm_code("text", registry_codebook(), "moonshot/kimi", structured = "structured")
  trail <- qlm_trail(x)
  file <- tempfile(fileext = ".qmd")
  generate_trail_report(trail, file)
  report <- paste(readLines(file), collapse = "\n")
  expect_match(report, "**Requested model:** moonshot/kimi", fixed = TRUE)
  expect_match(report, "https://api.moonshot.ai/v1", fixed = TRUE)
  expect_false(grepl("registry-secret-must-not-be-saved", report, fixed = TRUE))
  text <- paste(capture.output(dput(trail)), collapse = "\n")
  expect_false(grepl("registry-secret-must-not-be-saved", text, fixed = TRUE))
  expect_match(text, "MOONSHOT_API_KEY", fixed = TRUE)
})

test_that("explicit replay credentials replace the recorded credential provenance", {
  local_mocked_bindings(structured_chat_turns = turns_stub(list(json_turn(list(label = "yes")))))
  x <- qlm_code("text", registry_codebook(), "moonshot/kimi", structured = "structured")
  restored <- restore_run_args(x, overrides = list(credentials = function() "different"))
  expect_null(restored$resolution$api_key_env)
  expect_identical(restored$resolution$requested_model, "moonshot/kimi")
})


test_that("empty provider prefixes retain the readable unknown-provider error", {
  for (model in c("", "/foo")) {
    expect_error(qlm_code("text", registry_codebook(), model), "Can't reach provider")
  }
})
