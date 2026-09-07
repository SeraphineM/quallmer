test_that("qlm_code validates codebook argument", {
  skip_if_not_installed("ellmer")

  # Should error on invalid codebook objects
  expect_error(
    qlm_code(c("test"), codebook = list(name = "fake"), model = "test"),
    "must be created using.*qlm_codebook"
  )

  expect_error(
    qlm_code(c("test"), codebook = "not valid", model = "test"),
    "must be created using.*qlm_codebook"
  )
})


test_that("qlm_code accepts both task and qlm_codebook objects", {
  skip_if_not_installed("ellmer")

  withr::local_options(lifecycle_verbosity = "quiet")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Should accept qlm_codebook
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  expect_true(inherits(codebook, "qlm_codebook"))

  # Should accept old task (will be converted internally)
  old_task <- task("Test", "Prompt", type_obj)
  expect_true(inherits(old_task, "task"))

  # Both should pass validation (we can't test execution without APIs)
  # but we can verify they're accepted as valid input types
})


test_that("qlm_code validates input type matches codebook", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))

  # Text codebook expects character input
  text_codebook <- qlm_codebook("Test", "Prompt", type_obj, input_type = "text")

  # Should error on non-character input
  expect_error(
    qlm_code(x = 123, codebook = text_codebook, model = "test"),
    "expects text input.*character vector"
  )

  expect_error(
    qlm_code(x = list("a", "b"), codebook = text_codebook, model = "test"),
    "expects text input.*character vector"
  )

  # Image codebook also expects character input (file paths)
  image_codebook <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")

  expect_error(
    qlm_code(x = 123, codebook = image_codebook, model = "test"),
    "expects image file paths or URLs.*character vector"
  )
})


test_that("qlm_code checks image files exist before any request (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  image_codebook <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                                 image_file_resize = "none")
  existing <- withr::local_tempfile(fileext = ".png")
  writeLines("", existing)

  f <- qlm_code
  mockery::stub(f, "try_structured_call", function(...) {
    stop("a request was sent")
  })

  # Every missing path is named, and nothing is sent
  expect_error(
    f(c(existing, "no/such/poster.jpg", "nor/this.png"), image_codebook,
      model = "openai/gpt-4o"),
    "2 image files do not exist.*no/such/poster.jpg.*nor/this.png"
  )
  expect_error(
    f(c(existing, NA), image_codebook, model = "openai/gpt-4o"),
    "1 image file does not exist"
  )
  # A URL typed without its scheme is a path, and is named
  expect_error(
    f("example.org/poster.jpg", image_codebook, model = "openai/gpt-4o"),
    "example.org/poster.jpg"
  )
  # A directory is not an image file
  expect_error(
    f(dirname(existing), image_codebook, model = "openai/gpt-4o"),
    "does not exist"
  )

  # URLs are not checked for existence
  expect_error(
    f(c(existing, "https://example.org/poster.jpg", "data:image/png;base64,AAAA"),
      image_codebook, model = "openai/gpt-4o"),
    "a request was sent"
  )
})


test_that("qlm_code tells image paths from URLs (#177)", {
  aic <- as_image_content
  mockery::stub(aic, "ellmer::content_image_file", function(path, resize) {
    list(kind = "file", path = path, resize = resize)
  })
  mockery::stub(aic, "ellmer::content_image_url", function(url, detail) {
    list(kind = "url", url = url, detail = detail)
  })

  x <- c("posters/a.jpg", "https://example.org/b.jpg",
         "http://example.org/c.png", "data:image/png;base64,AAAA", "d.png")
  content <- aic(x, resize = "1024x1024>", detail = "high")

  expect_length(content, 5)
  expect_equal(vapply(content, `[[`, "", "kind"),
               c("file", "url", "url", "url", "file"))
  # The codebook's setting reaches every file, and no URL
  expect_equal(content[[1]]$resize, "1024x1024>")
  expect_equal(content[[5]]$resize, "1024x1024>")
  expect_equal(content[[2]]$url, "https://example.org/b.jpg")
  # and the detail reaches every URL, and no file
  expect_equal(content[[2]]$detail, "high")
  expect_equal(content[[4]]$detail, "high")
  expect_null(content[[1]]$detail)
})


test_that("as_input_content passes the codebook's URL detail to images (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  cb <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                     image_file_resize = "none", image_url_detail = "low")
  content <- as_input_content("https://example.org/b.jpg", cb, NULL)
  expect_identical(content[[1]]@detail, "low")

  # A codebook saved before the field existed asks for nothing
  cb$image_url_detail <- NULL
  content <- as_input_content("https://example.org/b.jpg", cb, NULL)
  expect_true(content[[1]]@detail %in% c("auto", ""))
})


test_that("qlm_code says when image_url_detail cannot take effect (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  make <- function(detail) {
    qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                 image_file_resize = "none", image_url_detail = detail)
  }
  withr::local_envvar(c(OPENAI_API_KEY = "x", GOOGLE_API_KEY = "x"))
  openai <- ellmer::chat_openai(model = "gpt-4o-mini")$get_provider()
  gemini <- ellmer::chat_google_gemini(model = "gemini-2.5-flash")$get_provider()
  chat_with <- function(provider) {
    list(get_provider = function() provider)
  }
  url <- "https://example.org/poster.jpg"

  # An ellmer that forwards it, to a provider that reads it: nothing to say
  said <- say_image_url_detail
  mockery::stub(said, "ellmer_forwards_image_detail", TRUE)
  expect_silent(said(make("high"), url, chat_with(openai)))

  # "auto" asks for nothing, and a file is governed by the resize instead
  mockery::stub(said, "ellmer_forwards_image_detail", FALSE)
  expect_silent(said(make("auto"), url, chat_with(openai)))
  expect_silent(said(make("high"), "poster.jpg", chat_with(openai)))
  # An ellmer that does not forward it says so, naming the setting
  expect_message(
    said(make("high"), url, chat_with(openai)),
    'image_url_detail = "high".*does not pass it.*1133'
  )

  # A provider that ignores it says so too, by name
  mockery::stub(said, "ellmer_forwards_image_detail", TRUE)
  expect_message(
    said(make("low"), url, chat_with(gemini)),
    'image_url_detail = "low".*Google/Gemini ignores it'
  )
})


test_that("the URL detail notice reaches the user through qlm_code, prices or not (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  cb <- qlm_codebook("Posters", "Describe.", type_obj, input_type = "image",
                     image_file_resize = "none", image_url_detail = "high")
  withr::local_envvar(c(OPENAI_API_KEY = "test"))

  said <- say_image_url_detail
  mockery::stub(said, "ellmer_forwards_image_detail", FALSE)
  tsc <- try_structured_call
  mockery::stub(tsc, "say_image_url_detail", said)
  mockery::stub(tsc, "structured_chat_turns", turns_stub(
    data.frame(score = 1, input_tokens = 10, output_tokens = 5,
               cached_input_tokens = 0, cost = NA_real_)
  ))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  url <- "https://example.org/poster.png"

  expect_message(
    f(url, cb, model = "openai/gpt-4o-mini"),
    'image_url_detail = "high".*does not pass it'
  )
  # Supplying prices silences the cost note; it must not silence this
  expect_message(
    f(url, cb, model = "openai/gpt-4o-mini", prices = c(input = 1, output = 2)),
    'image_url_detail = "high".*does not pass it'
  )
})


test_that("provider_reads_image_detail knows the OpenAI family (#177)", {
  withr::local_envvar(c(OPENAI_API_KEY = "x", ANTHROPIC_API_KEY = "x",
                        GOOGLE_API_KEY = "x"))
  reads <- function(chat) provider_reads_image_detail(chat$get_provider())
  expect_true(reads(ellmer::chat_openai(model = "gpt-4o-mini")))
  expect_true(reads(ellmer::chat_openai_compatible(
    model = "m", base_url = "https://example.org/v1"
  )))
  expect_false(reads(ellmer::chat_anthropic(model = "claude-sonnet-4-5")))
  expect_false(reads(ellmer::chat_google_gemini(model = "gemini-2.5-flash")))
})


test_that("try_structured_call resizes images as the codebook says (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  image_codebook <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                                 image_file_resize = "1024x1024>")

  resize_seen <- NULL
  aic <- as_input_content
  mockery::stub(aic, "as_image_content", function(x, resize, detail) {
    resize_seen <<- resize
    as.list(x)
  })
  tsc <- try_structured_call
  mockery::stub(tsc, "as_input_content", aic)
  mockery::stub(tsc, "ellmer::chat", function(...) structure(list(), class = "ellmer_chat"))
  mockery::stub(tsc, "structured_chat_turns", turns_stub(data.frame(score = 0.5)))

  tsc(c(a = "poster.jpg"), image_codebook, model = "openai/gpt-4o",
      chat_args = list(), execution_args = list(), batch = FALSE)
  expect_identical(resize_seen, "1024x1024>")
})


test_that("qlm_code checks for magick only when a file will be resized (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  make <- function(resize) {
    qlm_codebook("Test", "Prompt", type_obj, input_type = "image",
                 image_file_resize = resize)
  }
  existing <- withr::local_tempfile(fileext = ".png")
  writeLines("", existing)

  asked_for <- NULL
  cir <- check_image_resize
  mockery::stub(cir, "rlang::check_installed", function(pkg, ...) {
    asked_for <<- c(asked_for, pkg)
  })

  cir(make("high"), existing)
  cir(make("1024x1024>"), existing)
  expect_identical(asked_for, c("magick", "magick"))

  # Sending the file as it is needs nothing, and a URL is never resized
  asked_for <- NULL
  cir(make("none"), existing)
  cir(make("high"), "https://example.org/poster.jpg")
  expect_null(asked_for)

  # The reason names the setting and the way round it, and renders as cli
  mockery::stub(cir, "rlang::check_installed", function(pkg, reason, ...) {
    cli::cli_abort(paste("The package {.pkg {pkg}} is required", reason))
  })
  expect_error(
    cir(make("high"), existing),
    'magick.*image_file_resize = "high".*none'
  )
})


test_that("qlm_code records the resolution an old image codebook was coded at (#177)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  old_image <- qlm_codebook("Test", "Prompt", type_obj, input_type = "image")
  old_image$image_file_resize <- NULL
  existing <- withr::local_tempfile(fileext = ".png")
  writeLines("", existing)

  resize_seen <- NULL
  aic <- as_input_content
  mockery::stub(aic, "as_image_content", function(x, resize, detail) {
    resize_seen <<- resize
    as.list(x)
  })
  tsc <- try_structured_call
  mockery::stub(tsc, "as_input_content", aic)
  mockery::stub(tsc, "ellmer::chat", function(...) structure(list(), class = "ellmer_chat"))
  mockery::stub(tsc, "structured_chat_turns", turns_stub(data.frame(score = 0.5)))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  mockery::stub(f, "check_image_resize", function(...) NULL)

  result <- f(c(a = existing), old_image, model = "openai/gpt-4o")

  # Coded at "low", as the original run was, and the run says so
  expect_identical(resize_seen, "low")
  expect_identical(attr(result, "codebook")$image_file_resize, "low")
  expect_identical(qlm_meta(codebook(result), "image_file_resize", type = "object"), "low")
})


test_that("qlm_code returns qlm_coded object structure", {
  skip_if_not_installed("ellmer")

  # We can't test actual execution, but we can verify the structure
  # by examining what new_qlm_coded creates

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  # Add id column to mock_results
  mock_results$id <- 1:2

  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(
      timestamp = Sys.time(),
      n_units = 2,
      ellmer_version = "0.4.0",
      quallmer_version = "0.2.0",
      R_version = "4.3.0"
    ),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify structure - qlm_coded is now a data.frame with attributes
  expect_true(inherits(mock_coded, "qlm_coded"))
  expect_true(inherits(mock_coded, "data.frame"))
  expect_true(is.data.frame(mock_coded))

  # Verify data frame columns (id renamed to .id)
  expect_true(".id" %in% names(mock_coded))
  expect_true("score" %in% names(mock_coded))

  # Verify attributes with new hierarchical structure
  expect_true(!is.null(attr(mock_coded, "data")))
  expect_equal(attr(mock_coded, "meta")$object$input_type, "text")
  meta_attr <- attr(mock_coded, "meta")
  expect_true(!is.null(meta_attr))
  expect_identical(attr(mock_coded, "codebook"), codebook)
  expect_true(is.list(meta_attr$object$chat_args))
  expect_true(is.list(meta_attr$object$execution_args))
  expect_false(meta_attr$object$batch)  # batch flag should be FALSE by default
  expect_true(is.list(meta_attr$system))
  expect_equal(meta_attr$user$name, "original")
  expect_null(meta_attr$object$parent)
})


test_that("qlm_code routes arguments correctly", {
  skip_if_not_installed("ellmer")

  # Test that argument routing logic doesn't crash
  # (Can't test actual routing without API calls)

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Get valid argument names
  chat_args <- names(formals(ellmer::chat))
  pcs_args <- names(formals(ellmer::parallel_chat_structured))

  expect_true(length(chat_args) > 0)
  expect_true(length(pcs_args) > 0)

  # Verify some expected arguments exist
  expect_true("name" %in% chat_args)
  expect_true("system_prompt" %in% chat_args)
  expect_true("chat" %in% pcs_args)
  expect_true("prompts" %in% pcs_args)
  expect_true("type" %in% pcs_args)
})


test_that("qlm_code works with predefined codebooks", {
  skip_if_not_installed("ellmer")

  # Predefined codebook should be valid
  expect_true(inherits(data_codebook_sentiment, "qlm_codebook"))
})


test_that("print.qlm_coded displays correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test Codebook", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:3, score = c(0.5, -0.3, 0.8))

  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(timestamp = Sys.time(), n_units = 3),
    name = "original",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Test that print works without error (delegates to tibble print)
  expect_no_error(print(mock_coded))

  # Verify it's a tibble
  expect_true(tibble::is_tibble(mock_coded))
})


test_that("qlm_code routes all execution arguments to execution_args", {
  skip_if_not_installed("ellmer")

  # Get valid argument names from both functions
  pcs_arg_names <- names(formals(ellmer::parallel_chat_structured))
  batch_arg_names <- names(formals(ellmer::batch_chat_structured))

  # All of these should be routed to execution_args
  expect_true("path" %in% batch_arg_names)  # batch-specific
  expect_true("wait" %in% batch_arg_names)  # batch-specific
  expect_true("ignore_hash" %in% batch_arg_names)  # batch-specific
  expect_true("max_active" %in% pcs_arg_names)  # parallel-specific
  expect_true("rpm" %in% pcs_arg_names)  # parallel-specific
  expect_true("on_error" %in% pcs_arg_names)  # parallel-specific

  # Shared args
  expect_true("convert" %in% pcs_arg_names)
  expect_true("convert" %in% batch_arg_names)
  expect_true("include_tokens" %in% pcs_arg_names)
  expect_true("include_tokens" %in% batch_arg_names)
})


test_that("new_qlm_coded stores batch flag and execution_args", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  # Test with batch=TRUE and mixed execution args (parallel + batch)
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(path = "/tmp/batch", wait = TRUE, max_active = 5, convert = TRUE),
    batch = TRUE,
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "batch_test",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify batch flag is stored
  meta_attr <- attr(mock_coded, "meta")
  expect_true(meta_attr$object$batch)

  # Verify execution_args contains all args (both parallel and batch specific)
  expect_true(is.list(meta_attr$object$execution_args))
  expect_equal(meta_attr$object$execution_args$path, "/tmp/batch")
  expect_true(meta_attr$object$execution_args$wait)
  expect_equal(meta_attr$object$execution_args$max_active, 5)
  expect_true(meta_attr$object$execution_args$convert)
})


test_that("new_qlm_coded maintains backward compatibility with pcs_args", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))

  # Test with old pcs_args parameter
  mock_coded <- new_qlm_coded(
    results = mock_results,
    codebook = codebook,
    data = c("text1", "text2"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    pcs_args = list(max_active = 5),
    metadata = list(timestamp = Sys.time(), n_units = 2),
    name = "compat_test",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify pcs_args are converted to execution_args
  meta_attr <- attr(mock_coded, "meta")
  expect_true(is.list(meta_attr$object$execution_args))
  expect_equal(meta_attr$object$execution_args$max_active, 5)
})


test_that("qlm_code passes provider-specific arguments to ellmer::chat", {

  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Track what arguments are passed to ellmer::chat
  chat_args_received <- NULL
  mock_chat <- function(...) {
    chat_args_received <<- list(...)
    structure(list(), class = "ellmer_chat")
  }
  mock_results <- data.frame(score = c(0.5, 0.8))

  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "structured_chat_turns", turns_stub(mock_results))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  # Call with a provider-specific argument (like base_url for openai_compatible)
  f(c("text1", "text2"), codebook, model = "openai_compatible/test-model",
           base_url = "https://my-api.com/v1")

  # Verify the provider-specific argument was passed through to ellmer::chat
  expect_true("base_url" %in% names(chat_args_received))
  expect_equal(chat_args_received$base_url, "https://my-api.com/v1")
})


# The chat is built inside try_structured_call(), so tools are observed there:
# a stub with a chat that records what is registered on it.
tools_stub <- function(registered, results = data.frame(score = c(0.5, 0.8))) {
  results$id <- NULL
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) {
    structure(
      list(register_tool = function(tool) {
        registered$tools <- c(registered$tools, list(tool))
        invisible(NULL)
      }),
      class = "Chat"
    )
  })
  mockery::stub(tsc, "structured_chat_turns", turns_stub(results))
  f <- qlm_code
  mockery::stub(f, "code_handler_json", function(...) stop("Unexpected fallback in tools fixture"))
  mockery::stub(f, "try_structured_call", tsc)
  f
}

test_that("qlm_code registers a single tool passed via `tools`", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  registered <- new.env()
  f <- tools_stub(registered)
  web_search <- ellmer::openai_tool_web_search()

  # Passed bare, not wrapped in list() -- should be auto-wrapped.
  f(c("text1", "text2"), codebook, model = "openai/gpt-4o-mini", tools = web_search)

  expect_length(registered$tools, 1)
  expect_identical(registered$tools[[1]], web_search)
})

test_that("qlm_code registers multiple tools, in order", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  registered <- new.env()
  f <- tools_stub(registered)
  web_search <- ellmer::openai_tool_web_search()
  custom_tool <- ellmer::tool(function() "ok", name = "fake_tool", description = "A test tool.")

  f(c("text1", "text2"), codebook, model = "openai/gpt-4o-mini",
    tools = list(web_search, custom_tool))

  expect_length(registered$tools, 2)
  expect_identical(registered$tools[[1]], web_search)
  expect_identical(registered$tools[[2]], custom_tool)
})

test_that("qlm_code does not register any tools when tools is NULL (default)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  registered <- new.env()
  f <- tools_stub(registered)

  f(c("text1", "text2"), codebook, model = "openai/gpt-4o-mini")

  expect_null(registered$tools)
})

test_that("qlm_code rejects non-tool objects passed as `tools`", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  expect_error(
    qlm_code(c("text1"), codebook, model = "openai/gpt-4o-mini", tools = "not a tool"),
    "must be a list of.*tool objects"
  )
  expect_error(
    qlm_code(c("text1"), codebook, model = "openai/gpt-4o-mini",
             tools = list(ellmer::openai_tool_web_search(), "not a tool")),
    "must be a list of.*tool objects"
  )
})

test_that("qlm_code records tools in chat_args metadata for reproducibility", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  f <- tools_stub(new.env())
  web_search <- ellmer::openai_tool_web_search()

  result <- f(c("text1", "text2"), codebook, model = "openai/gpt-4o-mini", tools = web_search)

  meta_attr <- attr(result, "meta")
  expect_length(meta_attr$object$chat_args$tools, 1)
  expect_identical(meta_attr$object$chat_args$tools[[1]], web_search)
})


test_that("qlm_code runs the structured adapter in parallel when batch=FALSE", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Mock the functions
  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- data.frame(score = c(0.5, 0.8))

  mock_pcs <- mockery::mock(rows_as_turns(mock_results), cycle = TRUE)
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "structured_chat_turns", mock_pcs)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f(c("text1", "text2"), codebook,
                     model = "openai_compatible/test-model", batch = FALSE)

  # The adapter was asked once, for a parallel run
  mockery::expect_called(mock_pcs, 1)
  expect_false(mockery::mock_args(mock_pcs)[[1]]$batch)

  # Verify result structure
  expect_s3_class(result, "qlm_coded")
  expect_false(attr(result, "meta")$object$batch)
})


test_that("qlm_code runs the structured adapter as a batch when batch=TRUE", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Mock the functions
  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- data.frame(score = c(0.5, 0.8))

  mock_bcs <- mockery::mock(rows_as_turns(mock_results), cycle = TRUE)
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "structured_chat_turns", mock_bcs)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  # Use an execution arg that's valid (convert is in both parallel and batch)
  result <- suppressWarnings(
    f(c("text1", "text2"), codebook,
      model = "openai_compatible/test-model", batch = TRUE,
      convert = TRUE)
  )

  # The adapter was asked once, for a batch
  mockery::expect_called(mock_bcs, 1)
  expect_true(mockery::mock_args(mock_bcs)[[1]]$batch)

  # Verify result structure
  expect_s3_class(result, "qlm_coded")
  expect_true(attr(result, "meta")$object$batch)
  expect_equal(attr(result, "meta")$object$execution_args$convert, TRUE)
})


test_that("qlm_code builds metadata correctly", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # Mock the functions
  mock_chat <- structure(list(), class = "ellmer_chat")
  mock_results <- data.frame(score = c(0.5, 0.8, 0.2))

  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", mock_chat)
  mockery::stub(tsc, "structured_chat_turns", turns_stub(mock_results))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f(c("text1", "text2", "text3"), codebook,
              model = "openai_compatible/test-model")

  meta_attr <- attr(result, "meta")

  # Verify metadata structure
  expect_true(is.list(meta_attr$system))
  expect_true("timestamp" %in% names(meta_attr$system))
  expect_equal(meta_attr$object$n_units, 3)
  expect_true("ellmer_version" %in% names(meta_attr$system))
  expect_true("quallmer_version" %in% names(meta_attr$system))
  expect_true("R_version" %in% names(meta_attr$system))

  # Verify timestamp is recent
  expect_true(inherits(meta_attr$system$timestamp, "POSIXct"))
  expect_true(difftime(Sys.time(), meta_attr$system$timestamp, units = "secs") < 1)
})


test_that("qlm_code stores notes in metadata", {
  skip_if_not_installed("ellmer")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test Codebook", "Test instructions", type_obj)

  # Create a qlm_coded object with notes
  result <- new_qlm_coded(
    results = data.frame(id = 1:3, score = c(0.5, -0.3, 0.8)),
    codebook = codebook,
    data = c("text1", "text2", "text3"),
    input_type = "text",
    chat_args = list(name = "test/model"),
    execution_args = list(),
    metadata = list(
      timestamp = Sys.time(),
      n_units = 3,
      notes = "Test run with temperature 0.5"
    ),
    name = "test_run",
    call = quote(qlm_code(...)),
    parent = NULL
  )

  # Verify notes are stored in metadata
  meta_attr <- attr(result, "meta")
  expect_equal(meta_attr$user$notes, "Test run with temperature 0.5")

  # Test print output includes notes
  output <- capture.output(print(result))
  expect_true(any(grepl("Notes:.*Test run with temperature 0.5", output)))
})


test_that("default_structured_mode skips the structured call only where it cannot work", {
  # DeepSeek's API answers `response_format` with HTTP 400, so attempting it is
  # a guaranteed-wasted round trip
  expect_equal(default_structured_mode("deepseek/deepseek-v4-pro"), "json")
  expect_equal(default_structured_mode("deepseek"), "json")

  expect_equal(default_structured_mode("openai/gpt-4o-mini"), "auto")
  expect_equal(default_structured_mode("anthropic/claude-sonnet-4-5"), "auto")
  expect_equal(default_structured_mode("openai_compatible/kimi-k3"), "auto")
})


test_that("qlm_code delegates to the handler and records the backend", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  handler_args <- NULL
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           json_retries = 2L, model_hint = NULL, ...) {
    handler_args <<- list(model = model, batch = batch, json_retries = json_retries,
                          chat_args = chat_args, execution_args = execution_args)
    results <- tibble::tibble(score = c(0.5, 0.8))
    attr(results, "qlm_backend_meta") <- list(backend = "json_mode", n_invalid = 0)
    results
  }

  f <- qlm_code
  mockery::stub(f, "code_handler_json", fake_handler)

  result <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
              json_retries = 5, max_active = 3)

  expect_s3_class(result, "qlm_coded")
  expect_equal(result$.id, 1:2)
  expect_equal(result$score, c(0.5, 0.8))
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  expect_equal(qlm_meta(result, type = "user")$n_invalid, 0)

  # json_retries reaches the handler as a formal; max_active still routes to execution
  expect_equal(handler_args$json_retries, 5)
  # on_error is recorded with the run's execution arguments (#171)
  expect_equal(handler_args$execution_args, list(max_active = 3, on_error = "continue"))
  expect_length(handler_args$chat_args, 0)
})


test_that("qlm_code takes the structured path for other providers", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  mock_pcs <- mockery::mock(rows_as_turns(data.frame(score = c(0.5, 0.8))), cycle = TRUE)
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "structured_chat_turns", mock_pcs)
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  result <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini")

  mockery::expect_called(mock_pcs, 1)
  # Every run now says which path produced it
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_equal(qlm_meta(result, type = "object")$structured, "auto")
})


test_that("qlm_code stores an ordinal enum as an ordered factor in the enum's order (#165)", {
  skip_if_not_installed("mockery")
  lv <- c("low", "medium", "high")
  schema <- ellmer::type_object(
    sev = ellmer::type_enum(lv),
    tone = ellmer::type_enum(c("pos", "neg"))
  )
  ordinal <- qlm_codebook("Test", "Prompt", schema, levels = list(sev = "ordinal"))
  nominal <- qlm_codebook("Test", "Prompt", schema)

  # Structured path: ellmer converts an enum to a plain factor in enum order
  returned <- data.frame(
    sev = factor(c("high", "low"), levels = lv),
    tone = factor(c("pos", "neg"), levels = c("pos", "neg"))
  )
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "structured_chat_turns", rows_as_turns(returned))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  res <- f(c("a", "b"), ordinal, model = "openai/gpt-4o-mini")
  expect_identical(res$sev, factor(c("high", "low"), levels = lv, ordered = TRUE))
  # Nominal unless declared: left as ellmer returned it
  expect_false(is.ordered(res$tone))
  expect_equal(levels(res$tone), c("pos", "neg"))
  res <- f(c("a", "b"), nominal, model = "openai/gpt-4o-mini")
  expect_false(is.ordered(res$sev))

  # JSON path, from a handler that returns the values as text
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           json_retries = 2L, model_hint = NULL, ...) {
    results <- tibble::tibble(sev = c("high", "low"), tone = c("pos", "neg"))
    attr(results, "qlm_backend_meta") <- list(backend = "json_mode")
    results
  }
  g <- qlm_code
  mockery::stub(g, "code_handler_json", fake_handler)
  res <- g(c("a", "b"), ordinal, model = "deepseek/deepseek-chat")
  expect_identical(res$sev, factor(c("high", "low"), levels = lv, ordered = TRUE))
  expect_type(res$tone, "character")

  # The order survives row subsetting and a save/load round trip
  expect_identical(res[2, ]$sev, factor("low", levels = lv, ordered = TRUE))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(res, path)
  expect_identical(readRDS(path)$sev, res$sev)
})


test_that("qlm_code errors on json_retries only where JSON mode cannot be reached", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # `structured` forbids the JSON path, so repair attempts can never happen
  expect_error(
    qlm_code(c("a"), codebook, model = "openai/gpt-4o-mini",
             structured = "structured", json_retries = 2),
    "not supported with"
  )
  # Even when the value happens to equal the default
  expect_error(
    qlm_code(c("a"), codebook, model = "openai/gpt-4o-mini",
             structured = "structured", json_retries = 2L),
    "not supported with"
  )

  # Under "auto" the JSON path is reachable, so the argument applies
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "structured_chat_turns", turns_stub(data.frame(score = 0.5)))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  expect_s3_class(
    f(c("a"), codebook, model = "openai/gpt-4o-mini", json_retries = 4),
    "qlm_coded"
  )
})


test_that("qlm_code refuses a bad json_retries before any paid call", {
  skip_if_not_installed("mockery")
  codebook <- qlm_codebook("Test", "Prompt", ellmer::type_object(score = ellmer::type_number("Score")))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", function(...) stop("a paid call was made"))
  for (bad in list(-1, 1.5, NA, c(1, 2), "2", Inf, NaN, .Machine$integer.max + 1)) {
    expect_error(
      f("a", codebook, model = "openai/gpt-4o-mini", json_retries = bad),
      "single non-negative integer"
    )
  }
})

test_that("qlm_code passes its json_retries default to the handler", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  seen <- NULL
  fake_handler <- function(x, codebook, model, chat_args, execution_args, batch,
                           json_retries, model_hint = NULL, ...) {
    seen <<- json_retries
    tibble::tibble(score = 0.5)
  }
  f <- qlm_code
  mockery::stub(f, "code_handler_json", fake_handler)

  f("a", codebook, model = "deepseek/deepseek-chat")
  expect_equal(seen, 2L)
})


test_that("qlm_code requires a single model string", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  expect_error(
    qlm_code(c("a"), codebook, model = c("openai", "anthropic")),
    "must be a single string"
  )
})


# structured = c("auto", "structured", "json") --------------------------------

# A try_structured_call() with the chat and the structured adapter stubbed
# out. `results` is what the adapter answers: a list of turns, a table for
# rows_as_turns(), or a function of the execution arguments returning
# either, to model what ellmer would return under a given `on_error`.
# `errors` is a character vector of messages to throw, one per attempt, NA
# meaning "succeed". The execution arguments that reached the adapter, and
# how many prompts, are recorded in `calls`.
structured_stub <- function(results = data.frame(score = 0.5), errors = NULL,
                            calls = NULL) {
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) structure(list(), class = "Chat"))
  answer <- turns_stub(results, calls)
  i <- 0L
  adapter <- function(chat, prompts, type, batch = FALSE, execution_args = list()) {
    i <<- i + 1L
    err <- if (!is.null(errors) && i <= length(errors)) errors[[i]] else NA_character_
    if (!is.na(err)) {
      if (!is.null(calls)) calls$n <- i
      stop(err, call. = FALSE)
    }
    answer(chat, prompts, type, batch, execution_args)
  }
  mockery::stub(tsc, "structured_chat_turns", adapter)
  tsc
}

json_stub <- function(calls = NULL) {
  function(x, codebook, model, chat_args, execution_args, batch, json_retries,
           model_hint = NULL, prior_usage = NULL, ...) {
    if (!is.null(calls)) {
      calls$json <- TRUE
      calls$json_retries <- json_retries
      calls$model_hint <- model_hint
      calls$execution_args <- execution_args
      calls$x <- x
      calls$prior_usage <- prior_usage
    }
    results <- tibble::tibble(score = rep(0.99, length(x)))
    # The usage columns the handler would add, so a merge has them to align
    if (isTRUE(execution_args$include_tokens)) {
      results$input_tokens <- rep(1, length(x))
      results$output_tokens <- rep(1, length(x))
      results$cached_input_tokens <- rep(0, length(x))
    }
    if (isTRUE(execution_args$include_cost)) {
      results$cost <- rep(0.01, length(x))
    }
    attr(results, "qlm_backend_meta") <- list(backend = "json_mode", n_invalid = 0)
    results
  }
}

structured_test_codebook <- function() {
  qlm_codebook("Test", "Prompt", ellmer::type_object(score = ellmer::type_number("Score")))
}


test_that("structured = 'structured' never reaches the JSON path", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub())
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
              structured = "structured")

  expect_null(calls$json)
  expect_equal(result$score, 0.5)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("structured = 'json' never attempts the structured call", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
              structured = "json")

  expect_null(calls$n)
  expect_true(calls$json)
  expect_equal(result$score, 0.99)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  expect_equal(qlm_meta(result, type = "object")$structured, "json")
})


test_that("structured = 'auto' falls back to JSON mode when the call errors", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(errors = "HTTP 400 Bad Request.", calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f("a", structured_test_codebook(), model = "openai_compatible/kimi-k3",
                base_url = "https://example.com/v1"),
    "falling back to JSON mode"
  )

  expect_true(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
  # The reason is kept, so a run that switched path can say why
  expect_match(qlm_meta(result, type = "user")$fallback_reason, "400")
})


test_that("structured = 'auto' falls back when every completed response fails validation", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # HTTP 200, but the endpoint accepted the schema and ignored it: one answer
  # leaves the required field out, the other sends it as a string
  ignored <- list(
    json_turn(string = "{}"),
    json_turn(string = '{"score": "high"}')
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = ignored))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(), model = "openai_compatible/x",
                base_url = "https://example.com/v1"),
    "no usable values"
  )
  expect_true(calls$json)
  expect_match(qlm_meta(result, type = "user")$fallback_reason, "\\$\\.score is required")
})


test_that("the JSON fallback re-codes only the units it can help, and keeps the rest", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # Every completed response is invalid, so the run falls back. The first
  # unit was cut off at the output limit: JSON mode would hit the same
  # limit, and a backfill would leave it alone, so it keeps its row. The
  # refused request and the invalid response are re-coded.
  turns <- list(
    json_turn(string = '{"sco', tokens = c(10, 100, 0), cost = 0.4,
              finish_reason = "max_tokens"),
    request_error("HTTP 500 Internal Server Error.", 500L),
    json_turn(string = '{"score": "high"}', tokens = c(10, 6, 0), cost = 0.2)
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = turns))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    coded <- f(c(cut = "long", refused = "b", invalid = "c"), structured_test_codebook(),
               model = "openai_compatible/x", base_url = "https://example.com/v1",
               include_tokens = TRUE, include_cost = TRUE),
    "falling back to JSON mode"
  )

  # Only the eligible units reached the handler, with their own prior usage
  expect_equal(names(calls$x), c("refused", "invalid"))
  expect_equal(nrow(calls$prior_usage), 2L)
  expect_equal(unname(calls$prior_usage[2, "cost"]), 0.2)

  # The cut-off unit keeps its row, reason and usage; the others are re-coded
  expect_equal(coded$.id, c("cut", "refused", "invalid"))
  expect_equal(coded$score, c(NA, 0.99, 0.99))
  expect_s3_class(coded$.error[[1]], "quallmer_truncation_error")
  expect_null(coded$.error[[2]])
  expect_null(coded$.error[[3]])
  expect_equal(coded$output_tokens, c(100, 1, 1))
  expect_equal(coded$cost, c(0.4, 0.01, 0.01))
  expect_equal(names(coded)[names(coded) != ".id"],
               c("score", ".error", "input_tokens", "output_tokens",
                 "cached_input_tokens", "cost"))
  expect_equal(qlm_failures(coded)$.id, "cut")
  expect_equal(qlm_meta(coded, type = "object")$backend, "json_mode")
  expect_equal(qlm_meta(coded, type = "user")$fallback_kept, 1L)
})


test_that("where no fallback is possible, an all-invalid run is returned with its failures", {
  skip_if_not_installed("mockery")
  calls <- new.env()
  ignored <- list(json_turn(string = "{}"), json_turn(string = '{"score": "high"}'))

  # structured = "structured": no fallback by choice
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = ignored))
  mockery::stub(f, "code_handler_json", json_stub(calls))
  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mini",
                structured = "structured"),
    'every completed response failed validation.*structured = "auto"'
  )
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_equal(nrow(qlm_failures(result)), 2L)
  expect_match(qlm_failures(result)$reason[[2]], "\\$\\.score must be a number")

  # batch = TRUE under auto: no batch path to fall back to
  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mini",
                batch = TRUE),
    "every completed response failed validation.*no batch path"
  )
  expect_null(calls$json)
  expect_equal(nrow(qlm_failures(result)), 2L)
})


test_that("a response that fails validation is a failed unit, not grounds for a fallback", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # The middle answer sends the required field as null; the others are fine
  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(results = data.frame(score = c(1, NA_real_, 3))))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b", "c"), structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "1 response from the structured call could not be coded, out of 3"
  )
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_equal(qlm_meta(result, type = "object")$validation, "local")
  expect_equal(result$score, c(1, NA, 3))
  failures <- qlm_failures(result)
  expect_equal(failures$.id, 2L)
  expect_match(failures$reason, "\\$\\.score is required and cannot be null")
  expect_s3_class(result$.error[[2]], "quallmer_schema_error")
})


test_that("structured = 'auto' still falls back when the endpoint answered in prose", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # The endpoint answered, but not with JSON at all
  prose <- list(
    text_turn("The sentiment here is broadly positive."),
    text_turn("I would rate this a 4.")
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = prose))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(), model = "openai_compatible/x",
                base_url = "https://example.com/v1"),
    "no usable values"
  )
  expect_true(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "json_mode")
})


test_that("a structured run the provider rejects outright names the model (#133)", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # ellmer's parallel path hands back every rejected request as a row whose
  # .error is the HTTP condition, all fields NA
  http_400 <- function() {
    structure(
      list(message = "HTTP 400 Bad Request.", status = 400L, call = NULL),
      class = c("httr2_http_400", "httr2_http", "httr2_error", "rlang_error", "error", "condition")
    )
  }
  rejected <- tibble::tibble(
    score = c(NA_real_, NA_real_),
    .error = list(http_400(), http_400())
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = rejected))
  mockery::stub(f, "code_handler_json", json_stub(calls))
  mockery::stub(f, "model_name_hint", function(model, chat_args) {
    c("i" = "\"gpt-4o-mimi\" is not a model that \"openai\" lists.")
  })

  # The provider confirms the name is wrong: stop, rather than send it again
  # in JSON mode
  expect_error(
    f(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mimi"),
    "is not a model that"
  )
  expect_null(calls$json)

  # Without that confirmation, a rejection may be the provider refusing the
  # schema-constrained request itself, so the fallback runs as before
  g <- qlm_code
  mockery::stub(g, "try_structured_call", structured_stub(results = rejected))
  mockery::stub(g, "code_handler_json", json_stub(calls))
  mockery::stub(g, "model_name_hint", function(model, chat_args) character())
  expect_warning(
    g(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mimi"),
    "falling back to JSON mode"
  )
  expect_true(calls$json)
  # The provider was asked once; the JSON path is told the answer, not sent to ask again
  expect_identical(calls$model_hint, character())

  # And under structured = "structured" the rejection is the reported failure
  expect_error(
    g(c("a", "b"), structured_test_codebook(), model = "openai/gpt-4o-mimi",
      structured = "structured"),
    "HTTP 400 Bad Request"
  )
})


test_that("a wholly failed structured run is reported, not re-coded in JSON mode", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # The one response was cut off: a reason, but no evidence about the schema
  failed <- list(json_turn(string = '{"sco', tokens = c(10, 100, 0),
                           finish_reason = "max_tokens"))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = failed))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "could not be coded"
  )

  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_match(qlm_failures(result)$reason, "cut off at the max_tokens limit")
  expect_s3_class(result$.error[[1]], "quallmer_truncation_error")
})


# Truncated structured responses ----------------------------------------------

# declared_max_tokens() is what a backfill reads to tell a raised limit
test_that("declared_max_tokens reads params() and nothing else", {
  expect_equal(declared_max_tokens(list(params = ellmer::params(max_tokens = 200))), 200)
  expect_null(declared_max_tokens(list()))
  expect_null(declared_max_tokens(list(params = ellmer::params(temperature = 0))))
  expect_null(declared_max_tokens(list(params = "not a list")))
  expect_null(declared_max_tokens(list(params = list(max_tokens = -1))))
  # api_args is forwarded unread
  expect_null(declared_max_tokens(list(api_args = list(max_tokens = 5))))
})

test_that("the structured path reads the finish reason to flag a cut-off response", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  # The first response spent the whole limit and stopped mid-document; it
  # happens not to parse. The second is complete.
  turns <- list(
    json_turn(string = '{"score": 0.', tokens = c(50, 100, 0), finish_reason = "max_tokens"),
    json_turn(list(score = 0.7), tokens = c(40, 12, 0))
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = turns, calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(c("a", "b"), structured_test_codebook(),
                model = "anthropic/claude-sonnet-5",
                params = ellmer::params(max_tokens = 100)),
    "cut off at the max_tokens limit after 100 output tokens"
  )

  # Nothing extra was asked of ellmer for the check, and no usage column is
  # handed on that was not requested
  expect_null(calls$dots$include_tokens)
  expect_false("output_tokens" %in% names(result))
  # The cut-off row is a failure with a reason, not grounds for a JSON re-run
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  failures <- qlm_failures(result)
  expect_equal(nrow(failures), 1)
  expect_match(failures$reason, "max_tokens")
  expect_s3_class(result$.error[[1]], "quallmer_truncation_error")
  expect_equal(result$score[[2]], 0.7)
})

test_that("a cut-off response that happens to parse is still a failure", {
  skip_if_not_installed("mockery")

  # The object closed just before the limit: valid JSON, and the provider
  # still says the response was cut off. The provider's word wins.
  turns <- list(json_turn(list(score = 0.7), tokens = c(50, 100, 0),
                          finish_reason = "max_tokens"))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = turns))

  expect_warning(
    result <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "cut off at the max_tokens limit"
  )
  expect_true(is.na(result$score))
  expect_s3_class(result$.error[[1]], "quallmer_truncation_error")
})

test_that("a response the provider filtered or left unexplained is a failure with that reason", {
  skip_if_not_installed("mockery")

  turns <- list(
    text_turn("", finish_reason = "content_filter"),
    json_turn(list(score = 0.2), finish_reason = I("odd")),
    json_turn(list(score = 0.4), finish_reason = "tool_use"),
    json_turn(list(score = 0.6))
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = turns))

  expect_warning(
    result <- f(letters[1:4], structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "2 responses from the structured call could not be coded"
  )
  reasons <- qlm_failures(result)$reason
  expect_match(reasons[[1]], "content filter")
  expect_match(reasons[[2]], "finish reason \"odd\"")
  # A forced tool call finishes as tool_use; that is a normal completion
  expect_equal(result$score, c(NA, NA, 0.4, 0.6))
  # Neither is terminal for a backfill: only a cut at an output limit is
  expect_false(inherits(result$.error[[1]], "quallmer_truncation_error"))
  expect_false(inherits(result$.error[[2]], "quallmer_truncation_error"))
})


test_that("structured = 'structured' aborts rather than falling back", {
  skip_if_not_installed("mockery")

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(errors = "the endpoint said no"))

  expect_error(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
      structured = "structured"),
    "the endpoint said no"
  )
})


test_that("auto cannot fall back under batch, and says so", {
  skip_if_not_installed("mockery")

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(errors = "batch call failed"))

  expect_error(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini", batch = TRUE),
    "has no batch path"
  )
})


# on_error (#171) --------------------------------------------------------------

test_that("qlm_code sends on_error = 'continue' to a parallel run unless told otherwise (#171)", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  # The default reaches ellmer, and is recorded with the run
  coded <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
             structured = "structured")
  expect_equal(calls$dots$on_error, "continue")
  expect_equal(qlm_meta(coded, type = "object")$execution_args$on_error, "continue")

  # An explicit value is what reaches ellmer
  coded <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
             structured = "structured", on_error = "return")
  expect_equal(calls$dots$on_error, "return")
  expect_equal(qlm_meta(coded, type = "object")$execution_args$on_error, "return")

  # The JSON path is given the same setting: the default is qlm_code()'s, not
  # the handler's
  f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
    structured = "json")
  expect_equal(calls$execution_args$on_error, "continue")
  f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
    structured = "json", on_error = "stop")
  expect_equal(calls$execution_args$on_error, "stop")

  # Only ellmer's three values
  expect_error(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
      on_error = "carry on"),
    "should be one of"
  )
})


test_that("on_error never reaches the batch call, and cannot be set for one (#171)", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(calls = calls))

  coded <- f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
             batch = TRUE, structured = "structured")
  expect_equal(calls$n, 1)
  expect_false("on_error" %in% names(calls$dots))
  expect_false("on_error" %in% names(qlm_meta(coded, type = "object")$execution_args))

  # An explicit setting is refused before any request, rather than ignored
  # or left for ellmer to reject as an unused argument
  calls$n <- NULL
  expect_error(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini",
      batch = TRUE, on_error = "continue"),
    "not supported with `batch = TRUE`"
  )
  expect_null(calls$n)
})


test_that("the default leaves every failed unit discoverable, where 'return' would not (#171)", {
  skip_if_not_installed("mockery")

  # A codebook whose only required property is an array: after conversion a
  # missing answer and a valid empty one are the same zero-length cell, so
  # only `.error` identifies a failed unit.
  codebook <- qlm_codebook(
    "Themes", "List the themes.",
    ellmer::type_object(themes = ellmer::type_array(ellmer::type_string("A theme.")))
  )
  http_500 <- function() {
    structure(
      list(message = "HTTP 500 Internal Server Error.", status = 500L),
      class = c("httr2_http_500", "httr2_http", "httr2_error", "rlang_error",
                "error", "condition")
    )
  }
  # What ellmer returns from three units of which the first succeeds and the
  # second fails. Under "continue" the third is attempted too and fails with
  # its own reason; under "return" it is never sent, and ellmer's converted
  # table once showed it as an empty row with no `.error`, which this
  # codebook cannot tell from a coded unit with no themes. Read from the
  # turns, a request never sent is a failure with that reason.
  ellmer_would_return <- function(dots) {
    if (identical(dots$on_error, "continue")) {
      list(json_turn(list(themes = list("a"))), http_500(), http_500())
    } else {
      list(json_turn(list(themes = list("a"))), http_500(), NULL)
    }
  }

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = ellmer_would_return))

  coded <- suppressWarnings(
    f(c(u1 = "one", u2 = "two", u3 = "three"), codebook,
      model = "openai/gpt-4o-mini", structured = "structured")
  )
  expect_equal(qlm_failures(coded)$.id, c("u2", "u3"))
  expect_match(qlm_failures(coded)$reason[[2]], "HTTP 500")

  early <- suppressWarnings(
    f(c(u1 = "one", u2 = "two", u3 = "three"), codebook,
      model = "openai/gpt-4o-mini", structured = "structured",
      on_error = "return")
  )
  expect_equal(qlm_failures(early)$.id, c("u2", "u3"))
  expect_equal(qlm_failures(early)$reason[[2]], "API request failed: the request failed")
  # The coded unit's empty answer is still not a failure
  expect_equal(coded$themes[[1]], "a")
})


test_that("a wholly rejected default run sends every unit once, then gives the diagnosis (#171)", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  http_400 <- function() {
    structure(
      list(message = "HTTP 400 Bad Request.", status = 400L),
      class = c("httr2_http_400", "httr2_http", "httr2_error", "rlang_error",
                "error", "condition")
    )
  }
  # Under "continue" the parallel call does not stop at the first refusal, so
  # every unit is refused in turn: the cost accepted in #171
  rejected <- tibble::tibble(
    score = rep(NA_real_, 3),
    .error = list(http_400(), http_400(), http_400())
  )

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = rejected, calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))
  mockery::stub(f, "model_name_hint", function(model, chat_args) {
    c("i" = "\"gpt-4o-mimi\" is not a model that \"openai\" lists.")
  })

  expect_error(
    f(c("a", "b", "c"), structured_test_codebook(), model = "openai/gpt-4o-mimi"),
    "is not a model that"
  )
  expect_equal(calls$n, 1)
  expect_equal(calls$n_prompts, 3)
  expect_equal(calls$dots$on_error, "continue")
  expect_null(calls$json)
})


test_that("the DashScope json-word rejection retries structured before falling back", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  json_word <- paste(
    "<400> InternalError.Algo.InvalidParameter: 'messages' must contain the word",
    "'json' in some form, to use 'response_format' of type 'json_object'"
  )
  # Fails once with the json-word error, succeeds on the retry
  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(errors = json_word, calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  result <- f("a", structured_test_codebook(), model = "openai_compatible/qwen-flash",
              base_url = "https://example.com/v1")

  # Two structured attempts, and enforcement is kept rather than abandoned
  expect_equal(calls$n, 2)
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("an unrelated error falls back without a second structured attempt", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call",
                structured_stub(errors = "HTTP 500 Internal Server Error.", calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  suppressWarnings(
    f("a", structured_test_codebook(), model = "openai_compatible/x",
      base_url = "https://example.com/v1")
  )

  expect_equal(calls$n, 1)
  expect_true(calls$json)
})


test_that("json_retries reaches the JSON path under auto", {
  skip_if_not_installed("mockery")
  calls <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(errors = "nope"))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  suppressWarnings(
    f("a", structured_test_codebook(), model = "openai/gpt-4o-mini", json_retries = 7)
  )
  expect_equal(calls$json_retries, 7)
})


test_that("an array-only codebook is validated like any other", {
  skip_if_not_installed("mockery")

  # Required properties are all arrays. After conversion a missing answer and
  # a valid empty one are the same zero-length cell; validated before
  # conversion, the missing one is caught and the empty one is not.
  array_codebook <- qlm_codebook(
    "Test", "Prompt",
    ellmer::type_object(
      claims = ellmer::type_array(ellmer::type_string("A claim"))
    )
  )

  calls <- new.env()
  answers <- list(
    json_turn(string = '{"claims": ["x"]}'),
    json_turn(string = '{"claims": []}'),
    json_turn(string = "{}"),
    json_turn(string = '{"claims": {}}')
  )
  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub(results = answers, calls = calls))
  mockery::stub(f, "code_handler_json", json_stub(calls))

  expect_warning(
    result <- f(letters[1:4], array_codebook, model = "openai_compatible/kimi-k3",
                base_url = "https://example.com/v1"),
    "2 responses from the structured call could not be coded"
  )
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
  expect_equal(result$claims, list("x", character(0), character(0), character(0)))
  failures <- qlm_failures(result)
  expect_equal(failures$.id, c(3L, 4L))
  expect_match(failures$reason[[1]], "\\$\\.claims is required but missing")
  expect_match(failures$reason[[2]], "\\$\\.claims must be a JSON array")
})


test_that("a schema-valid empty array is not mistaken for a failure", {
  skip_if_not_installed("mockery")

  # An empty array is valid JSON Schema output for a required array, and after
  # conversion is indistinguishable from a missing one -- so detection must not
  # guess, and a codebook with a checkable scalar alongside must not fall back
  codebook <- qlm_codebook(
    "Test", "Prompt",
    ellmer::type_object(
      score  = ellmer::type_number("Score"),
      claims = ellmer::type_array(ellmer::type_string("A claim"))
    )
  )
  results <- tibble::tibble(score = c(1, 2), claims = list(character(0), character(0)))

  calls <- new.env()
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) {
    list(get_provider = function() {
      structure(list(), class = c("ellmer::ProviderOpenAICompatible", "S7_object"))
    })
  })
  mockery::stub(tsc, "structured_chat_turns", turns_stub(results))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  mockery::stub(f, "code_handler_json", json_stub(calls))

  suppressMessages(result <- f(c("a", "b"), codebook, model = "openai_compatible/x",
                               base_url = "https://example.com/v1"))
  expect_null(calls$json)
  expect_equal(qlm_meta(result, type = "object")$backend, "structured")
})


test_that("qlm_code rejects convert = FALSE rather than failing downstream", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  expect_error(
    qlm_code("a", codebook, model = "openai/gpt-4o-mini", convert = FALSE),
    "is not supported"
  )
  # TRUE is the default and must still be accepted
  skip_if_not_installed("mockery")
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", structure(list(), class = "Chat"))
  mockery::stub(tsc, "structured_chat_turns", turns_stub(data.frame(score = 0.5)))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  expect_s3_class(
    f("a", codebook, model = "openai/gpt-4o-mini", convert = TRUE),
    "qlm_coded"
  )
})


test_that("qlm_code forwards params and api_args to ellmer unchanged", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)

  # Which of the two a setting belongs in is ellmer's and the provider's
  # business. quallmer must not inspect or rewrite either object -- notably it
  # must not "helpfully" move top_k out of params, even though ellmer maps that
  # onto the unrelated OpenAI field top_logprobs for OpenAI-compatible
  # providers.
  user_params <- ellmer::params(temperature = 0.6, top_p = 0.95, top_k = 20)
  user_api_args <- list(top_k = 20, min_p = 0, enable_thinking = TRUE)

  seen <- NULL
  tsc <- try_structured_call
  mockery::stub(tsc, "ellmer::chat", function(...) {
    seen <<- list(...)
    structure(list(), class = "Chat")
  })
  mockery::stub(tsc, "structured_chat_turns", turns_stub(data.frame(score = 0.5)))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)

  f("a", codebook, model = "openai_compatible/qwen3-max",
    base_url = "https://example.com/v1",
    params = user_params, api_args = user_api_args)

  expect_identical(seen$params, user_params)
  expect_identical(seen$api_args, user_api_args)
  expect_equal(seen$base_url, "https://example.com/v1")
})


test_that("the JSON path forwards api_args too, adding only the response format", {
  skip_if_not_installed("mockery")

  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Prompt", type_obj)
  user_params <- ellmer::params(temperature = 0.6)

  seen <- NULL
  h <- code_handler_json
  mockery::stub(h, "ellmer::chat", function(...) {
    seen <<- list(...)
    structure(list(), class = "Chat")
  })
  mockery::stub(h, "json_chat_turns", function(chat, prompts, pc_args) {
    list(text = "{\"score\":1}", error = NA_character_, status = NA_integer_,
         usage = matrix(0, 1, 4, dimnames = list(
           NULL, c("input_tokens", "output_tokens", "cached_input_tokens", "cost"))))
  })

  h(x = "a", codebook = codebook, model = "openai_compatible/kimi-k3",
    chat_args = list(params = user_params,
                     api_args = list(reasoning_effort = "max")),
    execution_args = list())

  expect_identical(seen$params, user_params)
  expect_equal(seen$api_args$reasoning_effort, "max")
  # JSON mode is the one thing this path must set
  expect_equal(seen$api_args$response_format, list(type = "json_object"))
})


# Unit identifiers must be unique ---------------------------------------------

test_that("qlm_code rejects duplicated input names before spending a request", {
  skip_if_not_installed("mockery")
  f <- qlm_code
  mockery::stub(f, "try_structured_call", function(...) stop("a request was made"))

  expect_error(
    f(c(a = "x", a = "y"), structured_test_codebook(), model = "openai/gpt-4o-mini"),
    "must be unique"
  )
})

test_that("check_ids refuses missing identifiers before repeated ones", {
  expect_silent(check_ids(c("a", "b", "c")))
  expect_silent(check_ids(1:3))
  expect_error(check_ids(c("a", NA, "c")), "must not be missing")
  expect_error(check_ids(c("a", "", "c")), "must not be missing")
  expect_error(check_ids(c(NA, "a", "a")), "must not be missing")
  expect_error(check_ids(c("a", "b", "a")), "must be unique")
  expect_error(check_ids(c("a", "b", "a")), "\\ba\\b")

  # Factors are checked by their labels, including the empty one
  expect_silent(check_ids(factor(c("a", "b"))))
  expect_error(check_ids(factor(c("", "u1"))), "must not be missing")
  expect_error(check_ids(factor(c("a", "a"))), "must be unique")
  # Only a plain vector can be a key
  expect_error(check_ids(list("a", NULL)), "character, factor or numeric")
  expect_error(check_ids(matrix(1:4, 2)), "character, factor or numeric")
})

test_that("an empty factor label is refused as an identifier end to end", {
  a <- data.frame(.id = factor(c("", "u1")), score = c(0, 1))
  expect_error(as_qlm_coded(a, name = "A"), "must not be missing")
})

test_that("selecting .id away returns a plain tibble, not a broken coded object", {
  x <- as_qlm_coded(data.frame(.id = c("a", "b"), score = c(1, 0)), name = "A")
  projected <- x["score"]
  expect_false(inherits(projected, "qlm_coded"))
  expect_false(inherits(projected, "qlm_humancoded"))
  expect_s3_class(projected, "tbl_df")
  expect_null(attr(projected, "meta"))
  expect_equal(names(projected), "score")
  # ... whereas keeping it keeps the object
  kept <- x[c(".id", "score")]
  expect_s3_class(kept, "qlm_coded")
  expect_false(is.null(attr(kept, "meta")))
  # and a vector comes back as a vector
  expect_equal(x[, "score", drop = TRUE], c(1, 0))
  # Selecting the key twice is refused rather than kept as a classed object
  expect_error(x[c(".id", ".id")], "exactly one")
  expect_error(x[c(".id", "score", ".id")], "exactly one")
})

test_that("new_qlm_coded requires exactly one .id column", {
  codebook <- structured_test_codebook()
  two <- data.frame(id = c("a", "b"), .id = c("x", "y"), score = c(1, 2))
  expect_error(
    new_qlm_coded(
      results = two, codebook = codebook, data = c("a", "b"), input_type = "text",
      chat_args = list(name = "test/model"), execution_args = list(),
      metadata = list(timestamp = Sys.time(), n_units = 2),
      name = "run", call = quote(qlm_code(...)), parent = NULL
    ),
    "exactly one"
  )
})

test_that("new_qlm_coded rejects a table whose .id repeats", {
  codebook <- structured_test_codebook()
  expect_error(
    new_qlm_coded(
      results = data.frame(id = c("d1", "d1", "d2"), score = c(1, 2, 3)),
      codebook = codebook, data = c("a", "b", "c"), input_type = "text",
      chat_args = list(name = "test/model"), execution_args = list(),
      metadata = list(timestamp = Sys.time(), n_units = 3),
      name = "run", call = quote(qlm_code(...)), parent = NULL
    ),
    "must be unique"
  )
})


# Completing a run in the same call -------------------------------------------

test_that("qlm_code(backfill = ) hands the result to qlm_backfill", {
  skip_if_not_installed("mockery")
  seen <- new.env()

  f <- qlm_code
  mockery::stub(f, "try_structured_call", structured_stub())
  mockery::stub(f, "qlm_backfill", function(x, ..., passes) {
    seen$x <- x
    seen$passes <- passes
    x
  })
  code <- function(...) f("a", structured_test_codebook(), model = "openai/gpt-4o-mini", ...)

  # FALSE, 0 and NULL all mean none in a fresh run
  code()
  expect_null(seen$x)
  code(backfill = 0)
  expect_null(seen$x)
  code(backfill = NULL)
  expect_null(seen$x)

  result <- code(backfill = 3)
  expect_s3_class(seen$x, "qlm_coded")
  expect_identical(seen$passes, 3L)
  expect_equal(seen$x$score, result$score)

  # TRUE is the default number of passes, not one pass by coercion
  code(backfill = TRUE)
  expect_identical(seen$passes, 2L)

  for (bad in list(-1, 1.5, NA, c(1, 2), "2", c(TRUE, TRUE), Inf, -Inf, NaN,
                   .Machine$integer.max + 1)) {
    expect_error(code(backfill = bad), "single non-negative integer")
  }
})


test_that("check_qlm_coded verifies what every function relies on", {
  x <- as_qlm_coded(data.frame(.id = c("a", "b"), score = c(1, 0)), name = "A")
  expect_identical(check_qlm_coded(x), x)

  expect_error(check_qlm_coded(data.frame(.id = "a")), "must be a")

  no_meta <- x
  attr(no_meta, "meta") <- NULL
  expect_error(check_qlm_coded(no_meta), "no run metadata")

  no_id <- x
  names(no_id)[names(no_id) == ".id"] <- "id"
  expect_error(check_qlm_coded(no_id), "exactly one")

  forged <- x
  forged$.id <- c("a", "a")
  expect_error(check_qlm_coded(forged), "must be unique")
  forged$.id <- c(NA, "b")
  expect_error(check_qlm_coded(forged), "must not be missing")

  # The message names the object the caller passed
  expect_error(check_qlm_coded(forged, what = "{.arg gold}"), "gold")
})

test_that("every entry point refuses an object a row operation has left with a repeated key", {
  x <- as_qlm_coded(data.frame(.id = c("a", "b"), score = c(1, 0)), name = "A")
  # vctrs row slicing, which dplyr's verbs are built on, keeps the class and
  # attributes and does not go through `[`; so does base rbind()
  doubled <- vctrs::vec_slice(x, c(1, 1))
  expect_s3_class(doubled, "qlm_coded")
  expect_false(is.null(attr(doubled, "meta")))
  expect_s3_class(rbind(x, x), "qlm_coded")
  expect_error(qlm_failures(rbind(x, x)), "must be unique")

  expect_error(qlm_failures(doubled), "must be unique")
  expect_error(qlm_compare(doubled, x, by = "score", level = "interval"), "must be unique")
  expect_error(qlm_validate(doubled, gold = x, by = "score", level = "interval"), "must be unique")
  expect_error(qlm_trail(doubled), "must be unique")
  expect_error(qlm_replicate(doubled), "must be unique")
})


# cost that cannot be priced (#135) --------------------------------------------

# qlm_code() with the structured call stubbed to return `results`, and the
# chat built for real, off the environment and sending nothing, so that the
# diagnosis reads the provider the run would use. Callers pin
# structured = "structured": DeepSeek defaults to the JSON path.
coding_run <- function(results) {
  tsc <- try_structured_call
  mockery::stub(tsc, "structured_chat_turns", turns_stub(results))
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  f
}
offline <- function() list(Authorization = "Bearer x")

test_that("qlm_code says once, before the run, why cost will be NA (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8), cost = c(NA_real_, NA_real_))

  f <- coding_run(results)
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, include_cost = TRUE, structured = "structured"),
    "no prices for DeepSeek models"
  )
  expect_equal(qlm_meta(coded)$cost_note, "NA (ellmer has no prices for DeepSeek models)")
  expect_output(print(coded), "# Cost:     NA (ellmer has no prices for DeepSeek models)",
                fixed = TRUE)
})

test_that("qlm_code's cost message says whether token counts are recorded (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)

  # include_cost alone: no token columns come back, and the message says so
  f <- coding_run(data.frame(score = c(0.5, 0.8), cost = c(NA_real_, NA_real_)))
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, include_cost = TRUE, structured = "structured"),
    "Supply the provider's published rates as `prices`"
  )
  expect_false("input_tokens" %in% names(coded))

  # With include_tokens the counts are there, and the message says that instead
  with_tokens <- data.frame(score = c(0.5, 0.8), input_tokens = c(10, 12),
                            output_tokens = c(3, 4), cached_input_tokens = c(0, 0),
                            cost = c(NA_real_, NA_real_))
  f <- coding_run(with_tokens)
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, include_cost = TRUE, include_tokens = TRUE,
               structured = "structured"),
    "Token counts are recorded; supply"
  )
  expect_true("input_tokens" %in% names(coded))
})

test_that("qlm_code is silent about cost when it was not asked for, or is priced (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8))

  # Not asked for: the lookup is not even made
  f <- coding_run(results)
  expect_no_message(coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
                               credentials = offline, structured = "structured"))
  expect_null(qlm_meta(coded)$cost_note)
  expect_output(print(coded), "# Units:")
  expect_no_match(paste(capture.output(print(coded)), collapse = "\n"), "# Cost:")

  # Asked for and priced: nothing to say
  expect_no_message(coded <- f(c("a", "b"), codebook, model = "openai/gpt-4.1-mini",
                               credentials = offline, include_cost = TRUE,
                               structured = "structured"))
  expect_null(qlm_meta(coded)$cost_note)
})


# cost from supplied rates (#135) ----------------------------------------------

# coding_run() from above, with the execution arguments the structured call
# received recorded in `seen`.
priced_run <- function(results, seen) {
  tsc <- try_structured_call
  mockery::stub(tsc, "structured_chat_turns", function(chat, prompts, type, batch, execution_args) {
    seen$execution_args <- execution_args
    rows_as_turns(results)
  })
  f <- qlm_code
  mockery::stub(f, "try_structured_call", tsc)
  f
}

test_that("qlm_code costs an unpriced run from supplied rates (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8), input_tokens = c(1e6, 2e6),
                        output_tokens = c(1e5, 1e5), cached_input_tokens = c(0, 5e5),
                        cost = c(NA_real_, NA_real_))
  seen <- new.env()

  f <- priced_run(results, seen)
  # No "cost will be NA" message: it will not be
  expect_no_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, structured = "structured",
               prices = c(input = 1, output = 10, cached_input = 0.1))
  )

  # Costed by ellmer's sum, per million tokens
  expect_equal(coded$cost, c(2, (2e6 + 5e5 * 0.1 + 1e6) / 1e6))
  # Supplying rates asked ellmer for tokens and cost
  expect_true(isTRUE(seen$execution_args$include_tokens))
  expect_true(isTRUE(seen$execution_args$include_cost))
  # The rates travel with the object, and print says where the cost came from
  expect_equal(qlm_meta(coded)$prices, c(input = 1, output = 10, cached_input = 0.1))
  expect_equal(qlm_meta(coded)$cost_note,
               "from supplied rates: $1 input, $10 output, $0.1 cached input, per million tokens")
  expect_output(print(coded), "# Cost:     from supplied rates: $1 input", fixed = TRUE)
})

test_that("qlm_code leaves ellmer's own prices in charge (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  results <- data.frame(score = c(0.5, 0.8), input_tokens = c(1e6, 2e6),
                        output_tokens = c(1e5, 1e5), cached_input_tokens = c(0, 0),
                        cost = c(0.3, 0.6))

  f <- priced_run(results, new.env())
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "openai/gpt-4.1-mini",
               credentials = offline, structured = "structured",
               prices = c(input = 1, output = 10)),
    "prices.*is not used"
  )
  expect_equal(coded$cost, c(0.3, 0.6))
  expect_null(qlm_meta(coded)$prices)
  expect_null(qlm_meta(coded)$cost_note)
})

test_that("qlm_code says when supplied rates could not be applied (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  # Unpriced, and no counts came back to cost it from
  results <- data.frame(score = c(0.5, 0.8), input_tokens = c(NA_real_, NA_real_),
                        output_tokens = c(NA_real_, NA_real_),
                        cached_input_tokens = c(NA_real_, NA_real_),
                        cost = c(NA_real_, NA_real_))

  f <- priced_run(results, new.env())
  expect_message(
    coded <- f(c("a", "b"), codebook, model = "deepseek/deepseek-chat",
               credentials = offline, structured = "structured",
               prices = c(input = 1, output = 10)),
    "could not be applied"
  )
  expect_true(all(is.na(coded$cost)))
  # The rates are not recorded, and the note says why the cost is NA
  expect_null(qlm_meta(coded)$prices)
  expect_equal(qlm_meta(coded)$cost_note, "NA (ellmer has no prices for DeepSeek models)")
})

test_that("qlm_code rejects malformed prices before any request (#135)", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  f <- priced_run(data.frame(score = 1), new.env())
  expect_error(
    f("a", codebook, model = "openai/gpt-4.1-mini", credentials = offline,
      prices = c(input = 1), structured = "structured"),
    "Missing: output"
  )
})

test_that("qlm_code refuses tools with batch = TRUE before any call", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  expect_error(
    qlm_code("a", codebook, model = "openai/gpt-4o-mini", batch = TRUE,
             tools = ellmer::openai_tool_web_search()),
    "cannot be used with `batch = TRUE`"
  )
})

test_that("a real Chat accepts what qlm_code registers, under the names print() shows", {
  # The stubbed chats above record calls to register_tool() whether or not a
  # real Chat has that method; this pins the method and the tool names
  ch <- ellmer::chat_openai(
    model = "gpt-4o-mini",
    credentials = function() list(Authorization = "Bearer fake")
  )
  tools <- check_tools(list(
    ellmer::openai_tool_web_search(),
    ellmer::tool(function() "ok", name = "lookup", description = "Looks things up.")
  ))
  for (tl in tools) ch$register_tool(tl)
  expect_named(ch$get_tools(), c("web_search", "lookup"))
  expect_equal(format_tools(tools), "web_search (hosted), lookup (custom)")
})

test_that("print discloses tools, and a hosted tool is noted on the cost (#122)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  priced <- data.frame(id = 1:2, score = c(0.5, 0.8), input_tokens = 10L, output_tokens = 5L,
                       cached_input_tokens = 0L, cost = 0.01)
  web_search <- ellmer::openai_tool_web_search()
  lookup <- ellmer::tool(function() "ok", name = "lookup", description = "d")

  f <- tools_stub(new.env(), results = priced)
  coded <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini",
             tools = list(web_search, lookup), include_tokens = TRUE, include_cost = TRUE)
  out <- capture.output(print(coded))
  expect_true(any(grepl("^# Tools:    web_search \\(hosted\\), lookup \\(custom\\)$", out)))
  note <- attr(coded, "meta")$user$cost_note
  expect_match(note, "^from ellmer's price table; tokens only, hosted tool calls are billed separately")
  expect_true(any(grepl("^# Cost:     from ellmer's price table; tokens only", out)))

  # A custom tool runs in R and costs nothing at the provider: no note
  coded <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini",
             tools = lookup, include_tokens = TRUE, include_cost = TRUE)
  expect_null(attr(coded, "meta")$user$cost_note)

  # No cost column asked for: nothing to annotate
  coded <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini", tools = web_search)
  expect_null(attr(coded, "meta")$user$cost_note)
})

test_that("the trail keeps a run's tools by description, and none of a custom tool's code (#122)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  secret <- "t00l3cret"
  lookup <- local({
    captured <- secret
    ellmer::tool(function() captured, name = "lookup", description = "Looks things up.")
  })
  f <- tools_stub(new.env())
  coded <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini",
             tools = list(ellmer::openai_tool_web_search(), lookup))
  # The object itself keeps the tools, for a backfill or replication to reuse
  expect_true(is_ellmer_tool(attr(coded, "meta")$object$chat_args$tools[[2]]))

  stem <- tempfile()
  trail <- suppressMessages(qlm_trail(coded, path = stem))
  recorded <- attr(trail$runs[[1]]$coded, "meta")$object$chat_args$tools
  expect_true(is_tool_record(recorded))
  expect_equal(vapply(recorded, `[[`, "", "name"), c("web_search", "lookup"))

  report <- readLines(paste0(stem, ".qmd"))
  expect_true(any(grepl("^\\*\\*Tools:\\*\\* web_search \\(hosted\\), lookup \\(custom\\)$", report)))
  rds_file <- paste0(stem, ".rds")
  bytes <- memDecompress(readBin(rds_file, "raw", file.size(rds_file)), "gzip")
  expect_length(grepRaw(secret, bytes, fixed = TRUE), 0)
})

test_that("qlm_code requires batch to be a single logical, so tools cannot slip past it", {
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  web_search <- ellmer::openai_tool_web_search()
  for (bad in list(1, NA, c(TRUE, FALSE), "TRUE", 0)) {
    expect_error(qlm_code("a", codebook, model = "openai/gpt-4o-mini", batch = bad),
                 "must be `TRUE` or `FALSE`")
    expect_error(qlm_code("a", codebook, model = "openai/gpt-4o-mini", batch = bad, tools = web_search),
                 "must be `TRUE` or `FALSE`")
  }
})

test_that("a trail says tool definitions were kept, and does not call them credentials (#122)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  f <- tools_stub(new.env())
  coded <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini",
             tools = ellmer::openai_tool_web_search())
  msgs <- capture_messages(qlm_trail(coded))
  expect_true(any(grepl("Tool definitions recorded for \"run", msgs)))
  expect_false(any(grepl("Credential values", msgs)))
})

test_that("a comparison's recorded call is redacted at depth, and named for what it was (#122)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  f <- tools_stub(new.env())
  a <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini", name = "a")
  b <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini", name = "b")
  comparison <- qlm_compare(a, b, by = "score", level = "interval")
  m <- attr(comparison, "meta")
  m$object$call <- quote(qlm_compare(
    qlm_code(x, cb, tools = ellmer::tool(function() "NESTED_SECRET", name = "t", description = "d")),
    b
  ))
  attr(comparison, "meta") <- m

  stem <- tempfile()
  msgs <- capture_messages(qlm_trail(a, b, comparison, path = stem))
  expect_true(any(grepl("Tool definitions recorded", msgs)))
  expect_false(any(grepl("Credential values", msgs)))
  rds_file <- paste0(stem, ".rds")
  bytes <- memDecompress(readBin(rds_file, "raw", file.size(rds_file)), "gzip")
  expect_length(grepRaw("NESTED_SECRET", bytes, fixed = TRUE), 0)
  expect_false(any(grepl("NESTED_SECRET", readLines(paste0(stem, ".qmd")), fixed = TRUE)))
})

test_that("the trail report says what each tool could do (#122)", {
  skip_if_not_installed("mockery")
  type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
  codebook <- qlm_codebook("Test", "Test prompt", type_obj)
  f <- tools_stub(new.env())
  search <- ellmer::openai_tool_web_search(allowed_domains = "wikipedia.org")
  echo <- ellmer::tool(function(x, n) x, name = "echo", description = "Echoes the text.",
                       arguments = list(x = ellmer::type_string("The text"),
                                        n = ellmer::type_integer("Count", required = FALSE)))
  coded <- f(c("a", "b"), codebook, model = "openai/gpt-4o-mini", tools = list(search, echo))

  stem <- tempfile()
  suppressMessages(qlm_trail(coded, path = stem))
  report <- readLines(paste0(stem, ".qmd"))
  expect_true(any(grepl("^\\*\\*Tools:\\*\\* web_search \\(hosted\\), echo \\(custom\\)$", report)))
  expect_true(any(grepl('"allowed_domains":"wikipedia.org"', report, fixed = TRUE)))
  tool_line <- report[grepl("^- echo \\(custom\\):", report)]
  expect_length(tool_line, 1)
  expect_match(tool_line, '"description":"The text"')
  expect_match(tool_line, '"description":"Count"')
  expect_match(tool_line, '"required":\\["x"\\]')
  # print() stays compact
  out <- capture.output(print(coded))
  expect_true(any(grepl("^# Tools:    web_search \\(hosted\\), echo \\(custom\\)$", out)))
  expect_false(any(grepl("allowed_domains", out)))
})


test_that("print.qlm_coded reaches tibble's print method", {
  # tibble was in Imports but not in the NAMESPACE, so from an installed
  # package its namespace loaded only on the first tibble:: call. Until then
  # NextMethod() fell through to print.data.frame(), which cannot format a
  # condition among the .error cells (#173).
  expect_true("tibble" %in% names(getNamespaceImports("quallmer")))

  examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
  expect_output(print(examples$example_coded_incomplete), "# A tibble: 8", fixed = TRUE)
})

test_that("tools survive per-unit JSON fallback with structured usage retained", {
  seen <- list()
  local_mocked_bindings(
    structured_chat_turns = function(chat, prompts, type, batch, execution_args) {
      seen$structured <<- names(chat$get_tools())
      list(json_turn(list(score = "invalid")),
           json_turn(list(score = 1), finish_reason = "max_tokens"))
    },
    json_chat_turns = function(chat, prompts, pc_args) {
      seen$json <<- names(chat$get_tools())
      seen$prompts <<- prompts
      turn_records(list(text_turn('{"score": 2}')))
    }
  )
  cb <- qlm_codebook("Test", "Score", ellmer::type_object(score = ellmer::type_number()))
  expect_warning(
    result <- qlm_code(c(invalid = "retry me", cutoff = "keep me"), cb,
                       model = "openai/gpt-4o-mini", tools = ellmer::openai_tool_web_search(),
                       credentials = function() "offline", include_tokens = TRUE),
    "falling back"
  )
  expect_identical(seen$structured, "web_search")
  expect_identical(seen$json, "web_search")
  expect_length(seen$prompts, 1)
  expect_equal(result$score, c(2, NA))
  expect_match(conditionMessage(result$.error[[2]]), "max_tokens")
  expect_equal(result$input_tokens, c(20, 10))
})
