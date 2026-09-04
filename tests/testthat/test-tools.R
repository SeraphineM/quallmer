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
  expect_equal(records[[2]], list(name = "lookup", type = "custom", description = "Looks things up."))
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
