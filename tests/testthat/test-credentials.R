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

test_that("redact_call() replaces credential literals and keeps safe context", {
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
    credentials = my_creds,
    endpoint = base::identity
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

test_that("redact_header_expr() replaces whole credential-named entries", {
  expr <- quote(list(Authorization = paste("Bearer", key), `X-Api-Key` = "k", accept = "json"))
  out <- redact_header_expr(expr)
  expect_identical(
    out,
    quote(list(Authorization = "<redacted>", `X-Api-Key` = "<redacted>", accept = "json"))
  )
  expect_identical(redact_header_expr(quote(c("a", "b"))), REDACTED)
  expect_identical(redact_header_expr(quote(setNames("secret", "Authorization"))), REDACTED)
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

test_that("the trail keeps tools by description, and a replay does not send descriptions (#122)", {
  web_search <- ellmer::openai_tool_web_search()
  secret <- "t00l3cret"
  lookup <- local({
    captured <- secret
    ellmer::tool(function() captured, name = "lookup", description = "d")
  })
  meta <- list(object = list(
    chat_args = list(name = "openai/gpt-4o-mini", tools = list(web_search, lookup)),
    backfill = list(list(overrides = list(tools = list(lookup)), attempted = "a", recovered = "a"))
  ))
  out <- redact_meta(meta)
  expect_true(is_tool_record(out$object$chat_args$tools))
  expect_equal(vapply(out$object$chat_args$tools, `[[`, "", "name"), c("web_search", "lookup"))
  expect_true(is_tool_record(out$object$backfill[[1]]$overrides$tools))
  # The unredacted metadata serialises the closure's environment, with the
  # secret in it; the redacted metadata carries nothing of the kind
  expect_true(length(grepRaw(secret, serialize(meta, NULL), fixed = TRUE)) > 0)
  expect_length(grepRaw(secret, serialize(out, NULL), fixed = TRUE), 0)

  dropped <- drop_redacted_args(out$object$chat_args)
  expect_equal(dropped$dropped, "tools")
  expect_null(dropped$args$tools)
  kept <- drop_redacted_args(list(tools = list(web_search)))
  expect_length(kept$dropped, 0)
  expect_identical(kept$args$tools, list(web_search))
})

test_that("a bare tool in a backfill's overrides is recorded, not serialised (#122)", {
  secret <- "b4r3t00l"
  lookup <- local({
    captured <- secret
    ellmer::tool(function() captured, name = "lookup", description = "d")
  })
  web_search <- ellmer::openai_tool_web_search()
  # A backfill keeps `...` as given: one tool, unwrapped
  meta <- list(object = list(
    chat_args = list(name = "openai/gpt-4o-mini"),
    backfill = list(
      list(overrides = list(tools = lookup), attempted = "a", recovered = "a"),
      list(overrides = list(tools = web_search), attempted = "b", recovered = "b")
    )
  ))
  out <- redact_meta(meta)
  expect_true(is_tool_record(out$object$backfill[[1]]$overrides$tools))
  expect_equal(out$object$backfill[[1]]$overrides$tools[[1]]$name, "lookup")
  expect_equal(out$object$backfill[[2]]$overrides$tools[[1]]$type, "hosted")
  expect_length(grepRaw(secret, serialize(out, NULL), fixed = TRUE), 0)
  # And a bare tool on the run itself, should one ever be recorded that way
  bare <- list(object = list(chat_args = list(name = "m", tools = lookup)))
  expect_true(is_tool_record(redact_meta(bare)$object$chat_args$tools))
})

test_that("redact_call() replaces an inline tools expression and keeps a name (#122)", {
  inline <- quote(qlm_code(x, cb, tools = ellmer::tool(function() "SECRET", name = "t", description = "d")))
  expect_identical(redact_call(inline), quote(qlm_code(x, cb, tools = "<redacted>")))
  listed <- quote(qlm_code(x, cb, tools = list(ellmer::openai_tool_web_search())))
  expect_identical(redact_call(listed), quote(qlm_code(x, cb, tools = "<redacted>")))
  named <- quote(qlm_code(x, cb, tools = my_tools))
  expect_identical(redact_call(named), named)
  expect_false(any(grepl("SECRET", deparse(redact_call(inline)), fixed = TRUE)))
})

test_that("redact_call() reaches into nested calls (#122)", {
  nested <- quote(qlm_compare(
    qlm_code(x, cb, tools = ellmer::tool(function() "NESTED_SECRET", name = "t", description = "d")),
    qlm_code(x, cb, api_key = "sk-inner", base_url = "https://u:p@h/v1"),
    other
  ))
  out <- redact_call(nested)
  expect_identical(out, quote(qlm_compare(
    qlm_code(x, cb, tools = "<redacted>"),
    qlm_code(x, cb, api_key = "<redacted>", base_url = "https://h/v1"),
    other
  )))
  expect_false(any(grepl("NESTED_SECRET|sk-inner|u:p@", deparse(out))))
  # Positional inner calls, with no names on the outer call at all
  bare <- quote(f(qlm_code(x, cb, api_key = "sk-x")))
  expect_identical(redact_call(bare), quote(f(qlm_code(x, cb, api_key = "<redacted>"))))
})

test_that("computed credential expressions cannot survive in a trail (#123)", {
  secrets <- c("COMPUTED_SECRET", "CALLBACK_SECRET", "URL_SECRET", "HEADER_SECRET")
  meta <- list(object = list(call = quote(qlm_code(
    x, cb,
    api_key = paste0("sk-", "COMPUTED_SECRET"),
    credentials = local(function() "CALLBACK_SECRET"),
    base_url = paste0("https://u:", "URL_SECRET", "@host/v1"),
    api_headers = setNames(paste("Bearer", "HEADER_SECRET"), "Authorization")
  ))))

  out <- redact_meta(meta)
  expect_identical(out$object$call, quote(qlm_code(
    x, cb,
    api_key = "<redacted>",
    credentials = "<redacted>",
    base_url = "<redacted>",
    api_headers = "<redacted>"
  )))
  raw <- serialize(out, NULL)
  expect_true(all(vapply(secrets, function(secret) {
    length(grepRaw(secret, raw, fixed = TRUE)) == 0L
  }, logical(1))))
})
