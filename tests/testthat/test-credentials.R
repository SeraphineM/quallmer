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

test_that("redact_chat_args() keeps only a Sys.getenv() credentials callback, rebuilt bare", {
  f <- local({ captured <- "sk-hidden"; function() Sys.getenv("KEY") })
  out <- redact_chat_args(list(credentials = f))$credentials
  expect_true(is.function(out))
  expect_identical(body(out), quote(Sys.getenv("KEY")))
  expect_null(formals(out))
  expect_identical(environment(out), baseenv())
  expect_equal(out(), Sys.getenv("KEY"))

  # Braces around the one call are fine; base:: is fine
  expect_true(is.function(redact_chat_args(list(credentials = function() { Sys.getenv("K") }))$credentials))
  expect_true(is.function(redact_chat_args(list(credentials = function() base::Sys.getenv("K")))$credentials))

  # Anything that can hold the secret is replaced
  expect_equal(redact_chat_args(list(credentials = function() "sk-secret"))$credentials, "<redacted>")
  expect_equal(redact_chat_args(list(credentials = local({ k <- "sk"; function() k })))$credentials, "<redacted>")
  expect_equal(redact_chat_args(list(credentials = function(key = "sk") key))$credentials, "<redacted>")
  expect_equal(redact_chat_args(list(credentials = function() Sys.getenv("K", "sk-fallback")))$credentials, "<redacted>")
  expect_equal(redact_chat_args(list(credentials = function() { x <- 1; Sys.getenv("K") }))$credentials, "<redacted>")
  expect_equal(redact_chat_args(list(credentials = "sk-secret"))$credentials, "<redacted>")
  expect_equal(redact_chat_args(list(credentials = sum))$credentials, "<redacted>")
})

test_that("redact_chat_args() leaves unnamed input alone", {
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
    credentials = my_creds
  ))
  expect_identical(redact_call(call), call)
})

test_that("redact_call() keeps only a Sys.getenv() credentials callback literal", {
  safe <- quote(qlm_code(x, cb, credentials = function() Sys.getenv("KEY")))
  expect_identical(redact_call(safe), safe)
  expect_equal(deparse(redact_call(safe)), 'qlm_code(x, cb, credentials = function() Sys.getenv("KEY"))')

  expect_identical(
    redact_call(quote(qlm_code(x, cb, credentials = function() "sk-secret"))),
    quote(qlm_code(x, cb, credentials = "<redacted>"))
  )
  expect_identical(
    redact_call(quote(qlm_code(x, cb, credentials = function(k = "sk") k))),
    quote(qlm_code(x, cb, credentials = "<redacted>"))
  )
  expect_identical(
    redact_call(quote(qlm_code(x, cb, credentials = function() paste0("sk-", "abc")))),
    quote(qlm_code(x, cb, credentials = "<redacted>"))
  )
})

test_that("a kept callback literal carries no source reference (#154)", {
  # keep.source = TRUE attaches the source text as the fourth element; a
  # comment on that line would travel with it
  old <- options(keep.source = TRUE); withr::defer(options(old))
  expr <- parse(text = 'qlm_code(x, cb, credentials = function() Sys.getenv("KEY") # sk-in-a-comment\n)',
                keep.source = TRUE)[[1]]
  out <- redact_call(expr)
  expect_null(out$credentials[[4]])
  expect_false(any(grepl("sk-in-a-comment", deparse(out), fixed = TRUE)))
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

# ---- backfill passes and reuse of a redacted record --------------------------

test_that("redact_meta() redacts the overrides of every backfill pass", {
  meta <- list(object = list(
    call = quote(qlm_code(x, cb)), chat_args = list(name = "m"),
    backfill = list(
      list(model = NULL, overrides = list(api_key = "sk-p1", params = list(temperature = 0)),
           attempted = "a", recovered = "a"),
      list(model = "other/m",
           overrides = list(base_url = "https://u:p@h/v1", api_headers = c(Authorization = "Bearer t"),
                            credentials = function() "sk"),
           attempted = "b", recovered = character(0), error = "HTTP 503")
    )
  ))
  out <- redact_meta(meta)
  expect_equal(out$object$backfill[[1]]$overrides, list(api_key = "<redacted>", params = list(temperature = 0)))
  second <- out$object$backfill[[2]]
  expect_equal(second$overrides$base_url, "https://h/v1")
  expect_equal(second$overrides$api_headers, c(Authorization = "<redacted>"))
  expect_equal(second$overrides$credentials, "<redacted>")
  expect_equal(second$attempted, "b")
  expect_equal(second$error, "HTTP 503")
  # A pass without overrides, and a run without passes, are left alone
  expect_identical(redact_meta(list(object = list(backfill = list(list(attempted = "a"))))),
                   list(object = list(backfill = list(list(attempted = "a")))))
  expect_identical(redact_meta(list(object = list(chat_args = list(name = "m")))),
                   list(object = list(chat_args = list(name = "m"))))
})

test_that("drop_redacted_args() removes what a trail redacted and nothing else", {
  args <- list(
    api_key = "<redacted>", credentials = "<redacted>", params = list(temperature = 0),
    api_headers = c(Authorization = "<redacted>", `anthropic-beta` = "b"),
    base_url = "https://h/v1?api_key=<redacted>&api-version=2024"
  )
  out <- drop_redacted_args(args)
  expect_equal(out$args, list(
    params = list(temperature = 0), api_headers = c(`anthropic-beta` = "b"),
    base_url = "https://h/v1?api-version=2024"
  ))
  expect_equal(out$dropped, c("api_key", "credentials", "api_headers", "base_url"))

  # Real values are untouched
  clean <- list(api_key = "sk-real", api_headers = c(Authorization = "Bearer x"), base_url = "https://h/v1")
  expect_identical(drop_redacted_args(clean), list(args = clean, dropped = character()))
  expect_identical(drop_redacted_args(list()), list(args = list(), dropped = character()))
  expect_identical(drop_redacted_args(NULL), list(args = NULL, dropped = character()))

  # A header set that was all credentials goes entirely; a query that was
  # only the key goes with its `?`
  out <- drop_redacted_args(list(
    api_headers = list(Authorization = "<redacted>"), base_url = "https://h/v1?token=<redacted>"
  ))
  expect_equal(out$args, list(base_url = "https://h/v1"))
  expect_equal(out$dropped, c("api_headers", "base_url"))
})

test_that("redacted_args_note() reads correctly for one and for several", {
  expect_match(redacted_args_note("api_key"), "^`api_key` carries a value redacted .* it is not sent\\. Supply it in `\\.\\.\\.` if the endpoint needs it\\.$")
  expect_match(redacted_args_note(c("api_key", "base_url")), "^`api_key`, `base_url` carry values .* they are not sent\\. Supply them .* needs them\\.$")
})


test_that("redact_call() reaches credential literals in a nested call (#178)", {
  call <- quote(qlm_code(
    qlm_transcribe(files, api_key = "sk-secret", base_url = "https://u:pw@host.example.org/v1"),
    codebook, model = "openai/gpt-4o-mini", api_key = "sk-outer"
  ))
  out <- redact_call(call)
  inner <- out[[2]]
  expect_equal(inner$api_key, "<redacted>")
  expect_equal(inner$base_url, "https://host.example.org/v1")
  expect_equal(out$api_key, "<redacted>")
  expect_identical(inner[[2]], quote(files))
  expect_false(grepl("sk-secret|pw@", paste(deparse(out), collapse = "")))

  # Deeper, and inside a credentials callback given as a literal
  deep <- quote(f(g(h(api_key = "k"), credentials = function() "sk-x")))
  out <- redact_call(deep)
  expect_equal(out[[2]][[2]]$api_key, "<redacted>")
  expect_equal(out[[2]]$credentials, "<redacted>")

  # An empty argument is stepped over rather than read
  expect_identical(redact_call(quote(qlm_code(x[1, ], cb))), quote(qlm_code(x[1, ], cb)))
})
