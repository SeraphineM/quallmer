web_search <- ellmer::openai_tool_web_search()
lookup <- ellmer::tool(function() "ok", name = "lookup", description = "Looks things up.")

test_that("is_ellmer_tool recognises both kinds of tool and nothing else", {
  expect_true(is_ellmer_tool(web_search))
  expect_true(is_ellmer_tool(lookup))
  expect_false(is_ellmer_tool("web_search"))
  expect_false(is_ellmer_tool(list(web_search)))
  expect_false(is_ellmer_tool(NULL))
  expect_false(is_ellmer_tool(function() "ok"))
})

test_that("check_tools wraps a bare tool, treats an empty list as none, and refuses the rest", {
  expect_null(check_tools(NULL))
  expect_null(check_tools(list()))
  expect_identical(check_tools(web_search), list(web_search))
  expect_identical(check_tools(list(web_search, lookup)), list(web_search, lookup))

  expect_error(check_tools("not a tool"), "list of.*tool objects")
  expect_error(check_tools(42), "list of.*tool objects")
  expect_error(check_tools(list(web_search, "not a tool")), "list of.*tool objects")
  expect_error(check_tools(function() "ok"), "list of.*tool objects")
})

test_that("check_tools refuses tools with batch, which cannot send them", {
  expect_error(check_tools(web_search, batch = TRUE), "cannot be used with `batch = TRUE`")
  expect_error(check_tools(list(lookup), batch = TRUE), "cannot be used with `batch = TRUE`")
  expect_null(check_tools(NULL, batch = TRUE))
  expect_null(check_tools(list(), batch = TRUE))
})

test_that("tool records describe tools without their code, and are recognised as records", {
  records <- tool_records(list(web_search, lookup))
  expect_equal(records[[1]]$name, "web_search")
  expect_equal(records[[1]]$type, "hosted")
  expect_equal(records[[2]]$name, "lookup")
  expect_equal(records[[2]]$type, "custom")
  expect_equal(records[[2]]$description, "Looks things up.")
  expect_true(is.character(records[[1]]$description))

  expect_true(is_tool_record(records))
  expect_false(is_tool_record(list(web_search)))
  expect_false(is_tool_record(list()))
  expect_false(is_tool_record(NULL))

  expect_identical(as_tool_records(list(web_search, lookup)), records)
  expect_identical(as_tool_records(records), records)
  expect_equal(as_tool_records(list()), list())
  expect_equal(as_tool_records(NULL), list())
})

test_that("has_hosted_tool and format_tools read objects and records alike", {
  records <- tool_records(list(web_search, lookup))
  expect_true(has_hosted_tool(list(web_search)))
  expect_false(has_hosted_tool(list(lookup)))
  expect_true(has_hosted_tool(records))
  expect_false(has_hosted_tool(list()))
  expect_equal(format_tools(list(web_search, lookup)), "web_search (hosted), lookup (custom)")
  expect_equal(format_tools(records), "web_search (hosted), lookup (custom)")
})

test_that("tool records keep the configuration that decides what a tool can do (#122)", {
  a <- ellmer::openai_tool_web_search(allowed_domains = "example.com")
  b <- ellmer::openai_tool_web_search(allowed_domains = "wikipedia.org")
  ra <- tool_records(list(a))[[1]]
  rb <- tool_records(list(b))[[1]]
  expect_false(identical(ra, rb))
  expect_equal(ra$config$filters$allowed_domains, "example.com")
  expect_equal(rb$config$filters$allowed_domains, "wikipedia.org")
  expect_true(is_tool_record(list(ra, rb)))

  echo <- ellmer::tool(function(x) x, name = "echo", description = "d",
                       arguments = list(x = ellmer::type_string("s")))
  rc <- tool_records(list(echo))[[1]]
  expect_null(rc$config)
  expect_s3_class(rc$arguments, "ellmer::TypeObject")
  expect_true(is.list(rc$annotations))
  expect_false(any(vapply(rc, is.function, logical(1))))
})

test_that("as_tool_records wraps a bare tool", {
  expect_true(is_tool_record(as_tool_records(web_search)))
  expect_true(is_tool_record(as_tool_records(lookup)))
  expect_equal(as_tool_records(lookup)[[1]]$name, "lookup")
})

test_that("format_tool_details gives each tool's complete configuration", {
  search <- ellmer::openai_tool_web_search(allowed_domains = "example.com")
  echo <- ellmer::tool(function(x) x, name = "echo", description = "Echoes.",
                       arguments = list(x = ellmer::type_enum(c("a", "b"), "The choice")),
                       annotations = ellmer::tool_annotations(read_only_hint = TRUE))
  lines <- format_tool_details(list(search, echo, lookup))
  expect_length(lines, 3)
  expect_match(lines[1], '^- web_search \\(hosted\\): `\\{.*"allowed_domains":"example\\.com".*\\}`$')
  expect_match(lines[2], '^- echo \\(custom\\): Echoes\\. Configuration: `\\{')
  expect_match(lines[2], '"enum":\\["a","b"\\]')
  expect_match(lines[2], '"description":"The choice"')
  expect_match(lines[2], '"read_only_hint":true')
  expect_match(lines[3], '^- lookup \\(custom\\): Looks things up\\. Configuration: `\\{')
  # Records read back from a trail render the same way
  expect_identical(format_tool_details(tool_records(list(search, echo, lookup))), lines)

  other <- ellmer::tool(function(x) x, name = "echo", description = "Echoes.",
                        arguments = list(x = ellmer::type_enum(c("c", "d"), "The choice")),
                        annotations = ellmer::tool_annotations(read_only_hint = FALSE))
  expect_false(identical(format_tool_details(echo), format_tool_details(other)))
})
