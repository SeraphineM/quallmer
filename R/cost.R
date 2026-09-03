#' Rates supplied by the user, checked
#'
#' Rates are US dollars per million tokens, the unit ellmer's price table
#' uses, named `input`, `output` and optionally `cached_input`. A cached rate
#' that is not given is taken as the input rate: cache hits are then billed at
#' the full rate, which over-counts rather than under-counts, and no provider
#' charges more for a hit than for a miss.
#'
#' @param prices What the caller passed as `prices`.
#' @param call The calling environment, for the error message.
#'
#' @return `NULL` for `NULL`; otherwise a named numeric vector `input`,
#'   `output`, `cached_input`, in that order.
#' @keywords internal
#' @noRd
check_prices <- function(prices, call = rlang::caller_env()) {
  if (is.null(prices)) {
    return(NULL)
  }

  required <- c("input", "output")
  allowed <- c(required, "cached_input")
  problems <- character()

  if (!(is.numeric(prices) || is.list(prices)) || is.null(names(prices))) {
    problems <- "It is not a named numeric vector or list."
  } else {
    given <- names(prices)
    missing <- setdiff(required, given)
    unknown <- setdiff(given, allowed)
    if (length(missing)) {
      problems <- c(problems, paste0("Missing: ", paste(missing, collapse = ", "), "."))
    }
    if (length(unknown)) {
      problems <- c(problems, paste0("Not a rate: ", paste(unknown, collapse = ", "), "."))
    }
    if (anyDuplicated(given)) {
      problems <- c(problems, "A rate is given more than once.")
    }
    bad <- vapply(prices, function(p) {
      !is.numeric(p) || length(p) != 1L || is.na(p) || !is.finite(p) || p < 0
    }, logical(1))
    if (any(bad)) {
      problems <- c(problems, paste0(
        "Not a single non-negative number: ", paste(given[bad], collapse = ", "), "."
      ))
    }
  }

  if (length(problems)) {
    cli::cli_abort(c(
      paste0("{.arg prices} must name the rates {.code input} and {.code output}, ",
             "and optionally {.code cached_input}, in US dollars per million tokens."),
      stats::setNames(problems, rep("x", length(problems))),
      "i" = "For example {.code prices = c(input = 0.435, output = 0.87, cached_input = 0.0036)}."
    ), call = call)
  }

  c(
    input = as.numeric(prices[["input"]]),
    output = as.numeric(prices[["output"]]),
    cached_input = as.numeric(
      if ("cached_input" %in% names(prices)) prices[["cached_input"]] else prices[["input"]]
    )
  )
}


#' Cost the rows ellmer left unpriced, from their token counts
#'
#' The sum is the one ellmer applies to its own table. ellmer normalises every
#' provider's usage so that `input_tokens` counts the uncached prompt tokens and
#' `cached_input_tokens` the cache hits, whatever the provider reported, and
#' bills the three parts at their own rates; so no cache convention is needed
#' here. A row ellmer did price keeps its figure. A row without token counts,
#' which is a row the provider did not answer, stays `NA`.
#'
#' @param results The coded table, with ellmer's token and cost columns.
#' @param prices What `check_prices()` returned.
#'
#' @return `results`, with `cost` filled where it could be.
#' @keywords internal
#' @noRd
price_from_tokens <- function(results, prices) {
  needed <- c("input_tokens", "output_tokens", "cached_input_tokens")
  if (!all(needed %in% names(results))) {
    return(results)
  }
  if (!"cost" %in% names(results)) {
    results$cost <- rep(NA_real_, nrow(results))
  }

  input <- results$input_tokens
  output <- results$output_tokens
  cached <- results$cached_input_tokens
  cached[is.na(cached)] <- 0

  fill <- is.na(results$cost) & !is.na(input) & !is.na(output)
  results$cost[fill] <- (
    input[fill] * prices[["input"]] +
      cached[fill] * prices[["cached_input"]] +
      output[fill] * prices[["output"]]
  ) / 1e6

  results
}


#' One line saying where a cost came from, for the object and the trail
#'
#' @param prices What `check_prices()` returned.
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
prices_note <- function(prices) {
  rate <- function(x) format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  paste0(
    "from supplied rates: $", rate(prices[["input"]]), " input, $",
    rate(prices[["output"]]), " output, $", rate(prices[["cached_input"]]),
    " cached input, per million tokens"
  )
}
