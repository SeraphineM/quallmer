# ---- redact_url() ------------------------------------------------------------

test_that("redact_url() drops userinfo and keeps the rest of the URL", {
  expect_equal(redact_url("https://user:tok3n@host/v1"), "https://host/v1")
  expect_equal(redact_url("https://tok3n@host:8443/v1/"), "https://host:8443/v1/")
  expect_equal(redact_url("http://localhost:11434"), "http://localhost:11434")
})

test_that("redact_url() redacts only credential-named query values", {
  expect_equal(
    redact_url("https://host/v1?api_key=abc&api-version=2024&sig=xyz"),
    "https://host/v1?api_key=<redacted>&api-version=2024&sig=<redacted>"
  )
  expect_equal(redact_url("https://host/v1?api-version=2024"), "https://host/v1?api-version=2024")
  # A fragment is not sent to the server and is left alone
  expect_equal(redact_url("https://host/v1?token=t#frag"), "https://host/v1?token=<redacted>#frag")
})

test_that("redact_url() leaves anything that is not one string alone", {
  expect_identical(redact_url(NULL), NULL)
  expect_identical(redact_url(NA_character_), NA_character_)
  expect_identical(redact_url(c("a", "b")), c("a", "b"))
  expect_identical(redact_url(42), 42)
})

# ---- redact_chat_args() ------------------------------------------------------

test_that("redact_chat_args() replaces api_key and credential headers, keeps the rest", {
  args <- list(
    name = "openai/gpt-4o",
    api_key = "sk-abc",
    api_headers = c(Authorization = "Bearer x", `anthropic-beta` = "b", `X-Api-Key` = "k"),
    base_url = "https://u:p@host/v1",
    params = list(temperature = 0)
  )
  out <- redact_chat_args(args)
  expect_equal(out$api_key, "<redacted>")
  expect_equal(
    out$api_headers,
    c(Authorization = "<redacted>", `anthropic-beta` = "b", `X-Api-Key` = "<redacted>")
  )
  expect_equal(out$base_url, "https://host/v1")
  expect_equal(out$name, "openai/gpt-4o")
  expect_equal(out$params, list(temperature = 0))
})

test_that("redact_chat_args() handles headers given as a list and Azure's endpoint", {
  out <- redact_chat_args(list(
    api_headers = list(Authorization = "Bearer x", accept = "json"),
    endpoint = "https://u:p@mine.openai.azure.com"
  ))
  expect_equal(out$api_headers, list(Authorization = "<redacted>", accept = "json"))
  expect_equal(out$endpoint, "https://mine.openai.azure.com")
})

test_that("redact_chat_args() leaves a credentials function and unnamed input alone", {
  f <- function() Sys.getenv("KEY")
  expect_identical(redact_chat_args(list(credentials = f))$credentials, f)
  expect_identical(redact_chat_args(list()), list())
  expect_identical(redact_chat_args(NULL), NULL)
  expect_identical(redact_chat_args(list(1, 2)), list(1, 2))
})

# ---- redact_call() -----------------------------------------------------------

test_that("redact_call() replaces credential literals and nothing else", {
  call <- quote(qlm_code(
    x, cb, model = "m",
    api_key = "sk-abc",
    base_url = "https://u:p@host/v1?token=t",
    api_headers = c(Authorization = "Bearer x", `anthropic-beta` = "b")
  ))
  out <- redact_call(call)
  expect_identical(out, quote(qlm_code(
    x, cb, model = "m",
    api_key = "<redacted>",
    base_url = "https://host/v1?token=<redacted>",
    api_headers = c(Authorization = "<redacted>", `anthropic-beta` = "b")
  )))
})

test_that("redact_call() keeps an argument that names its source rather than holding a value", {
  call <- quote(qlm_code(
    x, cb,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    base_url = my_url,
    api_headers = my_headers,
    credentials = function() Sys.getenv("KEY")
  ))
  expect_identical(redact_call(call), call)
})

test_that("redact_call() is a no-op on calls without named arguments and on non-calls", {
  expect_identical(redact_call(quote(qlm_code(x, cb))), quote(qlm_code(x, cb)))
  expect_identical(redact_call(quote(f())), quote(f()))
  expect_identical(redact_call(NULL), NULL)
  expect_identical(redact_call("a string"), "a string")
})

test_that("redact_header_expr() only touches credential-named string literals", {
  expr <- quote(list(Authorization = paste("Bearer", key), `X-Api-Key` = "k", accept = "json"))
  out <- redact_header_expr(expr)
  expect_identical(
    out,
    quote(list(Authorization = paste("Bearer", key), `X-Api-Key` = "<redacted>", accept = "json"))
  )
  expect_identical(redact_header_expr(quote(c("a", "b"))), quote(c("a", "b")))
  expect_identical(redact_header_expr(quote(my_headers)), quote(my_headers))
})

# ---- redact_meta() and is_credential_name() ----------------------------------

test_that("redact_meta() rewrites call and chat_args and leaves other slots alone", {
  meta <- list(
    user = list(name = "run1"),
    object = list(
      call = quote(qlm_code(x, cb, api_key = "sk-abc")),
      chat_args = list(name = "m", api_key = "sk-abc"),
      parent = NULL, n_units = 2
    ),
    system = list(timestamp = as.POSIXct("2024-01-01"))
  )
  out <- redact_meta(meta)
  expect_identical(out$object$call, quote(qlm_code(x, cb, api_key = "<redacted>")))
  expect_equal(out$object$chat_args$api_key, "<redacted>")
  expect_identical(out$user, meta$user)
  expect_identical(out$system, meta$system)
  expect_identical(out$object$n_units, 2)

  # A comparison's metadata has a call but no chat_args
  comp_meta <- list(object = list(call = quote(qlm_compare(a, b)), parent = c("a", "b")))
  expect_identical(redact_meta(comp_meta), comp_meta)
})

test_that("is_credential_name() catches the names credentials travel under", {
  expect_true(all(is_credential_name(c(
    "Authorization", "Proxy-Authorization", "x-api-key", "Ocp-Apim-Subscription-Key",
    "X-Auth-Token", "api_key", "apikey", "sig", "access_token", "Cookie", "password"
  ))))
  expect_false(any(is_credential_name(c(
    "anthropic-beta", "api-version", "accept", "content-type", "OpenAI-Organization", "model"
  ))))
})
