# check_prices() --------------------------------------------------------------

test_that("check_prices() accepts the documented shapes and fills the cached rate", {
  expect_null(check_prices(NULL))

  full <- check_prices(c(input = 0.435, output = 0.87, cached_input = 0.0036))
  expect_equal(full, c(input = 0.435, output = 0.87, cached_input = 0.0036))

  # A list works too, and a missing cached rate is the input rate
  from_list <- check_prices(list(input = 2, output = 8))
  expect_equal(from_list, c(input = 2, output = 8, cached_input = 2))

  # Order of the names does not matter
  expect_equal(check_prices(c(output = 8, input = 2)), c(input = 2, output = 8, cached_input = 2))
})

test_that("check_prices() names what is wrong", {
  expect_error(check_prices(0.5), "not a named numeric vector")
  expect_error(check_prices(c(0.5, 1)), "not a named numeric vector")
  expect_error(check_prices(c(input = 1)), "Missing: output")
  expect_error(check_prices(c(input = 1, output = 2, cache = 3)), "Not a rate: cache")
  expect_error(check_prices(c(input = 1, output = 2, input = 3)), "more than once")
  expect_error(check_prices(c(input = -1, output = 2)), "non-negative number: input")
  expect_error(check_prices(list(input = "1", output = 2)), "non-negative number: input")
  expect_error(check_prices(list(input = c(1, 2), output = 2)), "non-negative number: input")
  expect_error(check_prices(c(input = NA, output = 2)), "non-negative number: input")
  expect_error(check_prices(c(input = Inf, output = 2)), "non-negative number: input")
})


# price_from_tokens() ---------------------------------------------------------

prices <- c(input = 1, output = 10, cached_input = 0.1)

test_that("price_from_tokens() fills only what ellmer left NA, by ellmer's sum", {
  results <- data.frame(
    score = 1:4,
    input_tokens = c(1e6, 2e6, 1e6, NA),
    output_tokens = c(1e5, 1e5, 1e5, 1e5),
    cached_input_tokens = c(0, 5e5, NA, 0),
    cost = c(NA, NA, 0.42, NA)
  )
  out <- price_from_tokens(results, prices)

  # 1e6 * 1 + 0 * 0.1 + 1e5 * 10, per million
  expect_equal(out$cost[1], 2)
  # cached tokens are additive to input, as ellmer normalises them
  expect_equal(out$cost[2], (2e6 * 1 + 5e5 * 0.1 + 1e5 * 10) / 1e6)
  # ellmer's own figure stands
  expect_equal(out$cost[3], 0.42)
  # no token counts, no cost
  expect_true(is.na(out$cost[4]))
  # nothing else touched
  expect_equal(out$score, 1:4)
})

test_that("price_from_tokens() leaves a table without token columns alone", {
  results <- data.frame(score = 1:2, cost = c(NA, NA))
  expect_identical(price_from_tokens(results, prices), results)
})

test_that("price_from_tokens() adds the cost column when ellmer did not", {
  results <- data.frame(score = 1:2, input_tokens = c(1e6, 0), output_tokens = c(0, 1e5),
                        cached_input_tokens = c(0, 0))
  out <- price_from_tokens(results, prices)
  expect_equal(out$cost, c(1, 1))
})


# prices_note() ---------------------------------------------------------------

test_that("prices_note() states the rates without scientific notation", {
  expect_equal(
    prices_note(c(input = 0.435, output = 0.87, cached_input = 0.0036)),
    "from supplied rates: $0.435 input, $0.87 output, $0.0036 cached input, per million tokens"
  )
  expect_equal(
    prices_note(c(input = 2, output = 8, cached_input = 2)),
    "from supplied rates: $2 input, $8 output, $2 cached input, per million tokens"
  )
})
