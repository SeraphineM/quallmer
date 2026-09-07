#' Placeholder written in place of a credential value
#' @keywords internal
#' @noRd
REDACTED <- "<redacted>"

#' Arguments to `ellmer::chat()` that hold a URL
#' @keywords internal
#' @noRd
url_arg_names <- c("base_url", "endpoint")


#' Redact credentials from a run's recorded metadata
#'
#' A trail is written to be handed to someone else, so it must not carry the
#' value of any credential the run was configured with. What is kept is that a
#' credential was supplied, and where; its value becomes `"<redacted>"`.
#'
#' Three routes carry a credential into a run's metadata, and each is closed:
#'
#' - `api_key`, as a value in `chat_args` or a string literal in the call.
#' - `api_headers`, whose entries named like a credential (`Authorization`,
#'   `x-api-key`, and so on) have their values replaced, in `chat_args` and in
#'   a `c()` or `list()` literal in the call. Other headers, such as
#'   `anthropic-beta`, describe the request and are kept.
#' - A URL argument (`base_url`, `endpoint`) carrying userinfo
#'   (`https://user:token@host`) or a credential-named query parameter
#'   (`?api_key=...`), in `chat_args` and as a string literal in the call.
#'   Other query parameters, such as Azure's `api-version`, are kept.
#' - Registered `tools`, replaced in `chat_args` by their name, type,
#'   description and configuration: a custom tool is an R function, and a
#'   closure serialised to the `.rds` carries its environment, as a
#'   credentials callback does. In the call, a `tools` argument written as
#'   anything but a variable name is replaced, since an inline
#'   `ellmer::tool()` definition carries its function's source.
#'
#' A credential argument in the call is kept only when it names a source that
#' cannot itself contain the value: a variable, a namespace-qualified name,
#' or an exact one-argument `Sys.getenv("KEY")` lookup. A computed expression
#' can contain a literal credential even though its result was not evaluated
#' when the call was captured, so any other expression is replaced wholesale.
#'
#' A `credentials` callback is a literal of its own kind: ellmer calls it for
#' the secret, so `function() "sk-..."` holds the value as surely as
#' `api_key = "sk-..."` does, and a closure serialised to the `.rds` carries
#' its enclosing environment with whatever that captured. The one form that
#' is kept is a zero-argument function whose body is a single
#' `Sys.getenv("NAME")`, which names a variable rather than holding a value;
#' it is rebuilt in the base environment so that nothing captured travels
#' with it. Any other callback becomes `"<redacted>"`, in `chat_args` and in
#' the call.
#'
#' A backfilled run also records, per pass, the overrides that pass was
#' given, which take the same credential arguments; each pass's `overrides`
#' is redacted like `chat_args`.
#'
#' @param meta A `meta` attribute as built by `new_qlm_coded()` and its
#'   counterparts for comparisons and validations.
#'
#' @return `meta` with `object$call`, `object$chat_args` and the `overrides`
#'   of every `object$backfill` pass redacted.
#' @keywords internal
#' @noRd
redact_meta <- function(meta) {
  if (!is.null(meta$object[["call"]])) {
    meta$object[["call"]] <- redact_call(meta$object[["call"]])
  }
  if (!is.null(meta$object[["chat_args"]])) {
    meta$object[["chat_args"]] <- redact_chat_args(meta$object[["chat_args"]])
  }
  if (length(meta$object[["backfill"]])) {
    meta$object[["backfill"]] <- lapply(meta$object[["backfill"]], function(pass) {
      if (!is.null(pass[["overrides"]])) {
        pass[["overrides"]] <- redact_chat_args(pass[["overrides"]])
      }
      pass
    })
  }
  meta
}


#' Drop the values a trail redacted before arguments are sent again
#'
#' An object read back from a trail carries `"<redacted>"` where a
#' credential was. Sending that literal would fail at the provider with a
#' message about a bad key, or worse, reach a proxy that accepts anything.
#' So before recorded arguments are reused, by `restore_run_args()` for a
#' replication or backfill and by `replay_backfill()` for a recorded pass,
#' the redacted values are removed: `api_key` and `credentials` go
#' altogether, so ellmer falls back to its own defaults; a redacted header
#' entry goes, the others stay; a redacted query parameter leaves the URL.
#'
#' @param args A named list of arguments to [qlm_code()].
#'
#' @return A list: `args`, with the redacted values removed, and `dropped`,
#'   the names of the arguments that carried one.
#' @keywords internal
#' @noRd
drop_redacted_args <- function(args) {
  dropped <- character()
  if (!is.list(args) || is.null(names(args))) {
    return(list(args = args, dropped = dropped))
  }

  for (nm in c("api_key", "credentials")) {
    if (identical(args[[nm]], REDACTED)) {
      args[[nm]] <- NULL
      dropped <- c(dropped, nm)
    }
  }

  headers <- args[["api_headers"]]
  if (!is.null(headers)) {
    hit <- vapply(headers, identical, logical(1), REDACTED)
    if (any(hit)) {
      headers <- headers[!hit]
      args[["api_headers"]] <- if (length(headers)) headers else NULL
      dropped <- c(dropped, "api_headers")
    }
  }

  for (arg in intersect(names(args), url_arg_names)) {
    url <- args[[arg]]
    if (is.character(url) && length(url) == 1L && !is.na(url) &&
        grepl(REDACTED, url, fixed = TRUE)) {
      args[[arg]] <- drop_redacted_query(url)
      dropped <- c(dropped, arg)
    }
  }
  # Tools read back from a trail are descriptions, not objects to register
  if (is_tool_record(args[["tools"]])) {
    args[["tools"]] <- NULL
    dropped <- c(dropped, "tools")
  }

  list(args = args, dropped = dropped)
}


#' Remove query parameters whose value was redacted
#'
#' @param url A single string.
#'
#' @return The URL without those parameters, and without a `?` when none
#'   remain.
#' @keywords internal
#' @noRd
drop_redacted_query <- function(url) {
  query <- regexpr("\\?[^#]*", url)
  if (query < 0L) {
    return(url)
  }
  params <- substring(url, query + 1L, query + attr(query, "match.length") - 1L)
  params <- strsplit(params, "&", fixed = TRUE)[[1]]
  params <- params[!endsWith(params, paste0("=", REDACTED))]
  regmatches(url, query) <- if (length(params)) paste0("?", paste(params, collapse = "&")) else ""
  url
}


#' The message that says which redacted arguments are not being sent
#'
#' @param dropped Names from `drop_redacted_args()`, minus any the caller
#'   overrode.
#'
#' @return A single string.
#' @keywords internal
#' @noRd
redacted_args_note <- function(dropped) {
  one <- length(dropped) == 1L
  paste0(
    paste0("`", dropped, "`", collapse = ", "),
    if (one) " carries a value" else " carry values",
    " redacted when this run was saved to a trail; ",
    if (one) "it is" else "they are",
    " not sent. Supply ", if (one) "it" else "them",
    " in `...` if the endpoint needs ", if (one) "it." else "them."
  )
}


#' Redact credential values in a list of `ellmer::chat()` arguments
#'
#' @param chat_args A named list, as recorded in `meta$object$chat_args`.
#'
#' @return The list with credential values replaced.
#' @keywords internal
#' @noRd
redact_chat_args <- function(chat_args) {
  if (!is.list(chat_args) || is.null(names(chat_args))) {
    return(chat_args)
  }

  if (!is.null(chat_args[["api_key"]])) {
    chat_args[["api_key"]] <- REDACTED
  }
  if (!is.null(chat_args[["api_headers"]])) {
    chat_args[["api_headers"]] <- redact_headers(chat_args[["api_headers"]])
  }
  if (!is.null(chat_args[["credentials"]])) {
    chat_args[["credentials"]] <- redact_credentials(chat_args[["credentials"]])
  }
  for (arg in intersect(names(chat_args), url_arg_names)) {
    if (is.character(chat_args[[arg]])) {
      chat_args[[arg]] <- redact_url(chat_args[[arg]])
    }
  }
  # A custom tool wraps an R function, whose environment would go into the
  # file with it; the trail keeps what the tools were, not their code. A
  # backfill's overrides hold `...` as given, so this may be one bare tool.
  if (!is.null(chat_args[["tools"]])) {
    chat_args[["tools"]] <- as_tool_records(chat_args[["tools"]])
  }

  chat_args
}


#' Redact credential values and expressions in a recorded call
#'
#' Applied at every depth: a comparison or validation records the coding
#' calls it was given as its arguments, and a credential in one of those is
#' no safer for being nested.
#'
#' @param call A call, as recorded by `match.call()` in `qlm_code()` and the
#'   functions that take its results.
#'
#' @return The call with credential-bearing values and expressions replaced.
#' @keywords internal
#' @noRd
redact_call <- function(call) {
  if (!is.call(call)) {
    return(call)
  }
  nms <- names(call) %||% rep("", length(call))

  for (i in seq_along(call)[-1]) {
    nm <- nms[i]
    value <- call[[i]]

    if (identical(nm, "api_key")) {
      if (!is_safe_credential_source(value)) {
        call[[i]] <- REDACTED
      }
    } else if (identical(nm, "api_headers")) {
      call[[i]] <- redact_header_expr(value)
    } else if (identical(nm, "credentials")) {
      call[[i]] <- redact_credentials_expr(value)
    } else if (identical(nm, "tools") && !is.symbol(value)) {
      # An inline tool definition can hold an R function and anything the
      # function holds; only a name that points elsewhere is safe to keep
      call[[i]] <- REDACTED
    } else if (nm %in% url_arg_names) {
      if (is.character(value)) {
        call[[i]] <- redact_url(value)
      } else if (!is_safe_credential_source(value)) {
        call[[i]] <- REDACTED
      }
    } else if (is.call(value)) {
      # A qlm_compare() or qlm_validate() call records the qlm_code() calls
      # it was given, with their arguments; the same rules apply at depth
      call[[i]] <- redact_call(value)
    }
  }

  call
}


#' Redact the values of credential-named headers
#'
#' @param headers A named character vector or list, as passed to
#'   `api_headers`.
#'
#' @return `headers` with matching values replaced.
#' @keywords internal
#' @noRd
redact_headers <- function(headers) {
  nms <- names(headers)
  if (is.null(nms)) {
    return(headers)
  }
  hit <- nzchar(nms) & is_credential_name(nms)
  headers[hit] <- REDACTED
  headers
}


#' Redact credential-bearing values in a header expression
#'
#' Handles the literal form, `c(Authorization = "Bearer ...")` or the same
#' with `list()`, replacing a credential-named entry regardless of whether its
#' value is a string or a computed expression. A source reference is safe to
#' keep. Any other expression is replaced wholesale because it can contain a
#' credential in its unevaluated language object.
#'
#' @param expr The `api_headers` argument of a recorded call.
#'
#' @return `expr` with matching entries replaced, or `"<redacted>"`.
#' @keywords internal
#' @noRd
redact_header_expr <- function(expr) {
  if (is_safe_credential_source(expr)) {
    return(expr)
  }
  if (!is.call(expr) ||
      !identical(expr[[1]], quote(c)) && !identical(expr[[1]], quote(list))) {
    return(REDACTED)
  }
  nms <- names(expr)
  if (is.null(nms) || any(!nzchar(nms[-1]))) {
    return(REDACTED)
  }

  for (i in seq_along(expr)[-1]) {
    if (is_credential_name(nms[i])) {
      expr[[i]] <- REDACTED
    }
  }

  expr
}


#' Redact a `credentials` callback held in `chat_args`
#'
#' A recognised safe callback is rebuilt from its body alone, in the base
#' environment, so that the function serialised into the `.rds` carries no
#' captured environment. Anything else, a function with another body or
#' with arguments (a default can hold a secret), or a value that is not a
#' function at all, becomes `"<redacted>"`.
#'
#' @param f The recorded `credentials` value.
#'
#' @return A rebuilt function, or `"<redacted>"`.
#' @keywords internal
#' @noRd
redact_credentials <- function(f) {
  if (!is.function(f) || is.primitive(f) || !is.null(formals(f)) ||
      !is_safe_credentials_body(body(f))) {
    return(REDACTED)
  }
  as.function(list(body(f)), envir = baseenv())
}


#' Redact a `credentials` argument in a recorded call
#'
#' A `function` literal is kept only in the recognised safe form, and then
#' rebuilt from its parts so that no source reference, which can carry text
#' beyond the code, travels with it. A safe source reference stays as written;
#' any other expression is replaced because it may contain a credential.
#'
#' @param expr The `credentials` argument of a recorded call.
#'
#' @return `expr`, a rebuilt function expression, or `"<redacted>"`.
#' @keywords internal
#' @noRd
redact_credentials_expr <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name("function"))) {
    if (!is.null(expr[[2]]) || !is_safe_credentials_body(expr[[3]])) {
      return(REDACTED)
    }
    return(as.call(list(as.name("function"), NULL, expr[[3]], NULL)))
  }
  if (is_safe_credential_source(expr)) {
    return(expr)
  }
  REDACTED
}


#' Is this expression only a reference to a credential source?
#'
#' Symbols and namespace-qualified names cannot contain a credential value in
#' the recorded call. An exact one-argument `Sys.getenv()` lookup contains only
#' the environment-variable name. Calls that compute a value are not safe:
#' their unevaluated arguments may contain the credential itself.
#'
#' @param expr An expression from a recorded call.
#'
#' @return `TRUE` when the expression cannot itself contain a credential value.
#' @keywords internal
#' @noRd
is_safe_credential_source <- function(expr) {
  if (is.null(expr) || is.symbol(expr) || is_safe_credentials_body(expr)) {
    return(TRUE)
  }
  is.call(expr) && length(expr) == 3L &&
    (identical(expr[[1]], quote(`::`)) || identical(expr[[1]], quote(`:::`))) &&
    is.symbol(expr[[2]]) && is.symbol(expr[[3]])
}


#' Is this function body a bare `Sys.getenv("NAME")`?
#'
#' Exactly one call, optionally wrapped in braces, to `Sys.getenv` or
#' `base::Sys.getenv`, with a single string argument. A second argument is
#' refused because `Sys.getenv("NAME", unset = "sk-...")` would hold the
#' secret as its fallback.
#'
#' @param body A function body or expression.
#'
#' @return `TRUE` for the safe form only.
#' @keywords internal
#' @noRd
is_safe_credentials_body <- function(body) {
  if (is.call(body) && identical(body[[1]], as.name("{"))) {
    if (length(body) != 2L) {
      return(FALSE)
    }
    body <- body[[2]]
  }
  if (!is.call(body) || length(body) != 2L) {
    return(FALSE)
  }

  fn <- body[[1]]
  is_getenv <- identical(fn, quote(Sys.getenv)) || identical(fn, quote(base::Sys.getenv))
  arg_name <- names(body)[2] %||% ""
  is_getenv && is.character(body[[2]]) && length(body[[2]]) == 1L &&
    arg_name %in% c("", "x")
}


#' Redact credentials embedded in a URL
#'
#' Userinfo is dropped altogether: it is not part of the endpoint's identity
#' and the fact that it was present is recorded by nothing being there. A
#' credential-named query parameter keeps its name and loses its value, so
#' the record still shows that the endpoint was addressed with, say, an
#' `api_key` parameter.
#'
#' @param url A single string. Anything else is returned as is.
#'
#' @return The URL with credentials replaced.
#' @keywords internal
#' @noRd
redact_url <- function(url) {
  if (length(url) != 1L || !is.character(url) || is.na(url)) {
    return(url)
  }

  url <- sub("^(\\w+://)[^/@?#]*@", "\\1", url)

  query <- regexpr("\\?[^#]*", url)
  if (query > 0L) {
    params <- substring(url, query + 1L, query + attr(query, "match.length") - 1L)
    params <- strsplit(params, "&", fixed = TRUE)[[1]]
    param_names <- sub("=.*$", "", params)
    hit <- grepl("=", params, fixed = TRUE) & is_credential_name(param_names)
    params[hit] <- paste0(param_names[hit], "=", REDACTED)
    regmatches(url, query) <- paste0("?", paste(params, collapse = "&"))
  }

  url
}


#' Does a header or query parameter name look like it carries a credential?
#'
#' Matched by substring, case-insensitively, so `Authorization`,
#' `Proxy-Authorization`, `x-api-key`, `Ocp-Apim-Subscription-Key`,
#' `X-Auth-Token`, Azure's SAS `sig` and a `?token=` parameter are all caught.
#' Erring towards redaction is the right side to err on: a value wrongly
#' redacted costs a reader one lookup, a value wrongly kept is a leak.
#'
#' @param x A character vector of names.
#'
#' @return A logical vector.
#' @keywords internal
#' @noRd
is_credential_name <- function(x) {
  grepl("auth|key|token|secret|passw|credential|cookie|session|sig", x,
        ignore.case = TRUE)
}
