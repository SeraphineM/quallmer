#' Code qualitative data with an LLM
#'
#' Applies a codebook to input data using a large language model, returning
#' a rich object that includes the codebook, execution settings, results, and
#' metadata for reproducibility.
#'
#' Arguments in `...` are dynamically routed to either [ellmer::chat()],
#' [ellmer::parallel_chat_structured()], or [ellmer::batch_chat_structured()]
#' based on their names.
#'
#' @param x Input data: a character vector of texts (for text codebooks) or
#'   file paths to images (for image codebooks). Named vectors will use names
#'   as identifiers in the output; unnamed vectors will use sequential integers.
#'   The identifiers become the `.id` column, on which every later operation
#'   keys, so names must be unique.
#' @param codebook A codebook object created with [qlm_codebook()]. Also accepts
#'   deprecated [task()] objects for backward compatibility.
#' @param model Provider (and optionally model) name in the form
#'   `"provider/model"` or `"provider"` (which will use the default model for
#'   that provider). Passed to the `name` argument of [ellmer::chat()].
#'   Examples: `"openai/gpt-4o-mini"`, `"anthropic/claude-3-5-sonnet-20241022"`,
#'   `"ollama/llama3.2"`, `"openai"` (uses default OpenAI model).
#' @param structured How the output schema is obtained. `"structured"` uses
#'   [ellmer::parallel_chat_structured()] and trusts the provider to enforce
#'   the schema. `"json"` asks for JSON, puts the schema in the system prompt,
#'   and validates every response against the codebook locally. `"auto"` (the
#'   default) attempts the structured call and falls back to `"json"` if it
#'   fails. See Details for which to use.
#' @param max_retries Number of additional attempts made for a response that
#'   arrives intact but does not conform to the codebook schema. Applies on the
#'   JSON path only, so setting it alongside `structured = "structured"` is an
#'   error. Default is 2, giving at most three attempts per unit. This is
#'   separate from ellmer's transport-level retries for rate limits and server
#'   errors, which apply to every provider and are set with
#'   `options(ellmer_max_tries = )`.
#' @param batch Logical. If `TRUE`, uses [ellmer::batch_chat_structured()]
#'   instead of [ellmer::parallel_chat_structured()]. Batch processing is more
#'   cost-effective for large jobs but may have longer turnaround times.
#'   Default is `FALSE`. See [ellmer::batch_chat_structured()] for details.
#' @param ... Additional arguments passed to [ellmer::chat()],
#'   [ellmer::parallel_chat_structured()], or [ellmer::batch_chat_structured()].
#'   Arguments recognized by [ellmer::parallel_chat_structured()] or
#'   [ellmer::batch_chat_structured()] are routed there; all other arguments
#'   (including provider-specific arguments like `base_url`, `credentials`, or
#'   `api_args` for OpenAI-compatible endpoints) are passed to [ellmer::chat()].
#' @param name Character string identifying this coding run. Default is `NULL`.
#' @param notes Optional character string with descriptive notes about this
#'   coding run. Useful for documenting the purpose or rationale when viewing
#'   results in [qlm_trail()]. Default is `NULL`.
#'
#' @details
#' Progress indicators and error handling are provided by the underlying
#' [ellmer::parallel_chat_structured()] or [ellmer::batch_chat_structured()]
#' function. Set `verbose = TRUE` to see progress messages during coding.
#' Retry logic for API failures should be configured through ellmer's options.
#'
#' @section Provider-specific parameters:
#'
#' `params` and `api_args` are forwarded to [ellmer::chat()] unchanged.
#' quallmer does not inspect or rewrite either, so which of the two a setting
#' belongs in is determined by ellmer and the provider, not here.
#'
#' The distinction matters. [ellmer::params()] carries provider-agnostic
#' settings that ellmer translates per provider; `api_args` goes into the raw
#' request body untouched. A setting placed in the wrong one is not
#' necessarily rejected. For OpenAI-compatible providers ellmer maps `top_k`
#' onto the OpenAI field `top_logprobs`, which asks for log-probabilities per
#' token and has nothing to do with top-k sampling — so
#' `params(top_k = 20)` is rejected by Alibaba Model Studio
#' (`Range of top_logprobs should be [0, 5]`), while `params(top_k = 3)` is
#' accepted and silently applies no sampling setting at all. Non-OpenAI
#' sampling controls therefore belong in `api_args`:
#'
#' ```r
#' # Qwen through Alibaba Model Studio
#' qlm_code(
#'   x, codebook,
#'   model = "openai_compatible/qwen3-max",
#'   base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1",
#'   credentials = function() {
#'     list(Authorization = paste("Bearer", Sys.getenv("DASHSCOPE_API_KEY")))
#'   },
#'   params = ellmer::params(temperature = 0.6, top_p = 0.95),
#'   api_args = list(top_k = 20, min_p = 0, enable_thinking = TRUE)
#' )
#'
#' # Kimi K3 through Moonshot, whose temperature and top_p are fixed by the
#' # provider and documented as needing to be omitted rather than set
#' qlm_code(
#'   x, codebook,
#'   model = "openai_compatible/kimi-k3",
#'   base_url = "https://api.moonshot.ai/v1",
#'   credentials = function() {
#'     list(Authorization = paste("Bearer", Sys.getenv("MOONSHOT_API_KEY")))
#'   },
#'   api_args = list(reasoning_effort = "max")
#' )
#' ```
#'
#' Passing a model parameter such as `temperature` or `max_tokens` at the top
#' level does not work: those reach [ellmer::chat()], which has no such
#' argument. Use `params`.
#'
#' @section Schema enforcement:
#'
#' Some providers accept a JSON Schema without enforcing it, so a response can
#' come back parseable but non-conforming — and the non-conformance then arrives
#' silently as `NA`, indistinguishable from missing data. Providers reached
#' through ellmer's generic OpenAI-compatible request path are all in this
#' position: `strict = TRUE` is sent and may simply be ignored. `qlm_code()`
#' emits a one-time note when it detects one, which
#' `options(quallmer.quiet_schema_note = TRUE)` silences.
#'
#' `structured` chooses what to do about it:
#'
#' \describe{
#'   \item{`"structured"`}{Trust the provider. Fails loudly if the call fails.}
#'   \item{`"json"`}{Never trust it: ask for JSON, put the schema in the system
#'     prompt, and validate each response against `codebook$schema` locally,
#'     re-prompting with the specific validation error when one does not
#'     conform. The reliable choice for an endpoint known not to enforce.}
#'   \item{`"auto"`}{Attempt the structured call; fall back to `"json"` if it
#'     errors, or if it returns a result in which every required field is `NA`
#'     in every row, which is what an endpoint that ignored the schema
#'     produces. Rows whose request failed, or whose response was cut off
#'     at `max_tokens`, are left out of that judgement, since neither says
#'     anything about the schema; a row from which ellmer could extract no
#'     structured data counts, since that is what an endpoint that ignored
#'     the schema produces.}
#' }
#'
#' `"auto"` catches wholesale non-enforcement, not the intermittent kind: a
#' single non-conforming row among many leaves that check silent. Only
#' `"json"` catches that, at the cost of putting the schema in the prompt.
#'
#' That check also needs something to look at. It reads required scalar
#' properties, because those become ordinary columns; required arrays and
#' nested objects become list-columns in which a missing value and a
#' schema-valid empty one are the same zero-length cell, and cannot be told
#' apart. So for a codebook whose required properties are all arrays or nested
#' objects, `"auto"` on an unverified endpoint validates locally from the
#' start rather than making a call it could not check, and says so. Use
#' `"structured"` to rely on the provider regardless.
#'
#' On the JSON path, units that never validate have `NA` coded values and a
#' `.error` list-column recording the reason, and `max_retries` controls how
#' many repair attempts each unit gets. On the structured path, a response
#' from which ellmer could extract no structured data, which it reports only
#' by warning, is likewise given an `.error`. On either path, [qlm_failures()]
#' lists the units that produced no usable coding, with the reason for each,
#' and `print()` reports how many there were. Batch processing and image
#' codebooks
#' are not supported there, so `"auto"` will not fall back under
#' `batch = TRUE`. The path actually taken is recorded in the run metadata as
#' `backend`.
#'
#' @section Truncated responses:
#'
#' A response that runs into the provider's output limit (`max_tokens`) is cut
#' off mid-JSON, and the request is billed in full. The affected units are
#' systematically the longest and richest ones, and for a codebook where an
#' empty answer is a legitimate outcome, a cut-off answer that reads as empty
#' is the worst kind of silent failure. What arrives depends on the path.
#'
#' On the JSON path the finish reason travels with the response, so a unit cut
#' off this way is recorded in `.error` with the token count, is listed by
#' [qlm_failures()], and is not retried: a repair prompt cannot supply what the
#' limit withheld, and would only press the model into a shorter answer.
#'
#' On the structured path, [ellmer::parallel_chat_structured()] returns the
#' converted table and discards the turns, and the finish reason with them. A
#' truncated response therefore arrives as a row of `NA` scalars and
#' zero-length arrays. Where the cut leaves unparsable JSON behind, ellmer
#' warns that it could extract no data, and that warning becomes an `.error`
#' naming a parse error rather than the cut. Where it leaves nothing behind,
#' as on Anthropic's forced-tool path, there is no warning, and nothing in the
#' table distinguishes the row from a unit to which nothing applied. What the
#' table can carry is the output token count, so when `params(max_tokens = )`
#' is set, a row that used the whole budget and has nothing in it is recorded
#' in `.error` as cut off, with that as the reason in either case. Without a
#' declared limit the cap is not known here (ellmer supplies a provider
#' default, 4096 for Anthropic, inside its request builder), and the silent
#' case stays silent. So set `max_tokens` explicitly, high enough for the
#' longest answer the codebook can produce: the limit is then both less likely
#' to be hit and detectable when it is.
#'
#' When `batch = TRUE`, the function uses [ellmer::batch_chat_structured()]
#' which submits jobs to the provider's batch API. This is typically more
#' cost-effective but has longer turnaround times. The `path` argument specifies
#' where batch results are cached, `wait` controls whether to wait for completion,
#' and `ignore_hash` can force reprocessing of cached results.
#'
#' @return A `qlm_coded` object (a tibble with additional attributes):
#'   \describe{
#'     \item{Data columns}{The coded results with a `.id` column for identifiers.}
#'     \item{Attributes}{`data`, `input_type`, and `run` (list containing name, batch, call, codebook, chat_args, execution_args, metadata, parent).}
#'   }
#'   The object prints as a tibble and can be used directly in data manipulation workflows.
#'   The `batch` flag in the `run` attribute indicates whether batch processing was used.
#'   The `execution_args` contains all non-chat execution arguments (for either parallel or batch processing).
#'
#' @seealso
#' [qlm_codebook()] for creating codebooks, [qlm_replicate()] for replicating
#' coding runs, [qlm_compare()] and [qlm_validate()] for assessing reliability.
#'
#' @examples
#' # Requires API credentials and internet access; not run in package checks.
#' \dontrun{
#' # Basic sentiment analysis
#' texts <- c("I love this product!", "Terrible experience.", "It's okay.")
#' coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded
#'
#' # With named inputs (names become IDs in output)
#' texts_named <- c(review1 = "Great service!", review2 = "Very disappointing.")
#' coded2 <- qlm_code(texts_named, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded2
#' }
#'
#' @export
qlm_code <- function(x, codebook, model, ...,
                     batch = FALSE,
                     structured = c("auto", "structured", "json"),
                     max_retries = 2L, name = NULL, notes = NULL) {
  # Distinguishes a value the user chose from the default, so that the default
  # never errors but an explicit setting is never silently ignored.
  explicit_retries <- !missing(max_retries)
  explicit_structured <- !missing(structured)
  structured <- match.arg(structured)

  # Accept both qlm_codebook and task objects, converting if needed
  if (inherits(codebook, "task") && !inherits(codebook, "qlm_codebook")) {
    codebook <- as_qlm_codebook(codebook)
  }

  if (!inherits(codebook, "qlm_codebook")) {
    cli::cli_abort(c(
      "{.arg codebook} must be created using {.fn qlm_codebook}.",
      "i" = "Use {.fn qlm_codebook} or one of the predefined codebook functions."
    ))
  }

  # Input validation
  if (codebook$input_type == "text" && !is.character(x)) {
    cli::cli_abort("This codebook expects text input (a character vector).")
  }
  if (codebook$input_type == "image" && !is.character(x)) {
    cli::cli_abort("This codebook expects image file paths (a character vector).")
  }
  # Names become .id, the key every later operation relies on. Checked here,
  # before any request is spent, rather than in the constructor afterwards.
  if (!is.null(names(x))) {
    check_ids(names(x), what = "{.code names(x)}")
  }

  # Get valid argument names from ellmer functions
  pcs_arg_names <- names(formals(ellmer::parallel_chat_structured))
  batch_arg_names <- names(formals(ellmer::batch_chat_structured))

  # Route ... arguments
  # execution_args go to parallel_chat_structured or batch_chat_structured
  # Everything else (including provider-specific args like base_url) goes to chat()
  dots <- list(...)
  dot_names <- names(dots)

  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    cli::cli_abort(c(
      "{.arg model} must be a single string.",
      "i" = "Use the form {.val provider/model} or {.val provider}, for example {.val openai/gpt-4o-mini}."
    ))
  }

  # A prefix ellmer cannot dispatch on is knowable without asking the provider
  # anything, so say so here rather than letting ellmer::chat() abort with
  # "Can't find provider ellmer::chat_qwen()".
  check_model_provider(model)

  # Checked after the model itself, so a bad model and a stray parameter report
  # the model first. `dot_names` is already in hand from the capture above.
  check_model_params(dot_names, model)

  # Providers whose API rejects the schema-constrained request skip straight to
  # JSON mode rather than spending a wasted round trip; see
  # default_structured_mode().
  if (!explicit_structured) {
    structured <- default_structured_mode(model)
  }

  # Repairing a response requires validating it, which only the JSON-mode path
  # does. Under `structured = "structured"` that path is never reached, so say
  # so rather than accepting a value that will not be applied.
  if (explicit_retries && identical(structured, "structured")) {
    cli::cli_abort(c(
      "{.arg max_retries} is not supported with {.code structured = \"structured\"}.",
      "i" = "It applies to the JSON-mode path; use {.code structured = \"auto\"} or {.code \"json\"}.",
      "i" = "For transport-level retries on any provider, set {.code options(ellmer_max_tries = )}."
    ))
  }

  # execution_args contains arguments for parallel_chat_structured or batch_chat_structured
  execution_arg_names <- unique(c(pcs_arg_names, batch_arg_names))
  execution_args <- dots[dot_names %in% execution_arg_names]

  # chat_args gets everything NOT destined for execution functions

  # This allows provider-specific args (base_url, credentials, api_args, etc.)
  # to pass through to ellmer::chat() which forwards them to the provider
  chat_args <- dots[!dot_names %in% execution_arg_names]

  # ellmer returns a bare list under convert = FALSE, which has no rows to
  # merge an .id into and no columns for new_qlm_coded() to reorder. It has
  # never worked here; say so rather than failing later with "incorrect number
  # of dimensions".
  if (identical(execution_args$convert, FALSE)) {
    cli::cli_abort(c(
      "{.code convert = FALSE} is not supported by {.fn qlm_code}.",
      "i" = "A {.cls qlm_coded} object is built from the converted table, one row per unit.",
      "i" = "For the unconverted list, call {.fn ellmer::parallel_chat_structured} directly."
    ))
  }

  # Metadata contributed by the coding path
  backend_meta <- list()
  results <- NULL
  fallback_reason <- NULL

  # ---- schema-constrained structured output -------------------------------
  if (structured %in% c("auto", "structured")) {
    attempt <- try_structured_call(
      x = x, codebook = codebook, model = model,
      chat_args = chat_args, execution_args = execution_args, batch = batch,
      allow_skip = identical(structured, "auto") && !batch
    )

    if (isTRUE(attempt$ok)) {
      results <- attempt$value
      backend_meta <- list(backend = "structured")
      incomplete <- n_incomplete(results, codebook$schema)
      if (incomplete) {
        cli::cli_warn(
          "{incomplete} row{?s} from the structured call {?is/are} missing at least one required field."
        )
      }
    } else if (isTRUE(attempt$undetectable)) {
      # Not a failure: no request was made. Informational, because the choice
      # is a consequence of the codebook and the endpoint, not of anything
      # going wrong.
      fallback_reason <- attempt$error
      cli::cli_inform(c(
        "i" = "Coding {.val {model}} in JSON mode with local validation.",
        "i" = attempt$error,
        "i" = "Use {.code structured = \"structured\"} to rely on the provider instead."
      ))
    } else if (identical(structured, "structured")) {
      cli::cli_abort(c(
        "Structured output failed for model {.val {model}}.",
        set_bullets(attempt$error),
        "i" = "Use {.code structured = \"auto\"} to fall back to JSON mode with local validation."
      ))
    } else if (batch) {
      # JSON mode drives its own parallel requests and has no batch API path,
      # so under batch there is nothing to fall back to. Say that, rather than
      # letting the handler abort later with a message about `batch`.
      cli::cli_abort(c(
        "Structured output failed for model {.val {model}}.",
        set_bullets(attempt$error),
        "i" = "JSON-mode coding has no batch path, so {.code structured = \"auto\"} cannot fall back here.",
        "i" = "Re-run with {.code batch = FALSE} to use local validation instead."
      ))
    } else {
      fallback_reason <- attempt$error
      cli::cli_warn(c(
        "Structured output failed; falling back to JSON mode with local validation.",
        set_bullets(attempt$error)
      ))
    }
  }

  # ---- JSON mode with local validation ------------------------------------
  if (is.null(results)) {
    results <- code_handler_json(
      x = x,
      codebook = codebook,
      model = model,
      chat_args = chat_args,
      execution_args = execution_args,
      batch = batch,
      max_retries = max_retries
    )
    backend_meta <- attr(results, "qlm_backend_meta") %||% list()
    attr(results, "qlm_backend_meta") <- NULL
  }

  backend_meta$structured <- structured
  if (!is.null(fallback_reason)) {
    backend_meta$fallback_reason <- fallback_reason
  }

  # Add ID column from input names or sequence
  results$id <- names(x) %||% seq_along(x)

  # Build metadata list
  metadata <- list(
    timestamp = Sys.time(),
    n_units = length(x),
    notes = notes,
    ellmer_version = tryCatch(
      as.character(utils::packageVersion("ellmer")),
      error = function(e) NA_character_
    ),
    quallmer_version = tryCatch(
      as.character(utils::packageVersion("quallmer")),
      error = function(e) NA_character_
    ),
    R_version = paste(R.version$major, R.version$minor, sep = ".")
  )

  # Fields contributed by a provider-specific handler (backend, max_retries, ...)
  metadata <- c(metadata, backend_meta)

  # Add model to chat_args for easy access
  chat_args$name <- model

  # Create and return qlm_coded object
  new_qlm_coded(
    results = results,
    codebook = codebook,
    data = x,
    input_type = codebook$input_type,
    chat_args = chat_args,
    execution_args = execution_args,
    batch = batch,
    metadata = metadata,
    name = name,
    call = match.call(),
    parent = NULL
  )
}

#' Default structured-output mode for a model
#'
#' Providers whose API rejects the schema-constrained request outright should
#' not spend a guaranteed-wasted round trip discovering that. DeepSeek is the
#' known case: its endpoint answers `response_format` of type `json_schema`
#' with HTTP 400, "This response_format type is unavailable now". Everything
#' else defaults to attempting the structured call.
#'
#' Only a *default*: an explicit `structured =` always wins.
#'
#' @param model Provider (and optionally model) name, as passed to [qlm_code()].
#'
#' @return One of `"auto"`, `"structured"` or `"json"`.
#' @keywords internal
#' @noRd
default_structured_mode <- function(model) {
  provider <- model_provider(model)

  switch(provider,
    deepseek = "json",
    "auto"
  )
}


#' Attempt schema-constrained structured output
#'
#' Wraps the [ellmer::parallel_chat_structured()] path so that [qlm_code()] can
#' decide what to do when it does not work. Two things count as failure: the
#' call throwing, and the call returning a table in which every required field
#' is `NA` in every row, which is what an endpoint that accepted the schema and
#' ignored it produces.
#'
#' @param x,codebook,model,chat_args,execution_args,batch As in [qlm_code()].
#'
#' @return A list with `ok`, and either `value` (the results) or `error`.
#' @keywords internal
#' @noRd
try_structured_call <- function(x, codebook, model, chat_args, execution_args, batch,
                                allow_skip = FALSE) {
  system_prompt <- if (!is.null(codebook$role)) {
    paste(codebook$role, codebook$instructions, sep = "\n\n")
  } else {
    codebook$instructions
  }

  if (codebook$input_type == "image") {
    prompts <- lapply(x, ellmer::content_image_file)
  } else {
    prompts <- as.list(x)
  }

  build_chat <- function(prompt) {
    do.call(ellmer::chat, c(list(name = model, system_prompt = prompt), chat_args))
  }
  chat <- build_chat(system_prompt)

  # Whether a failed structured call would even be visible depends on the
  # codebook. Failure is detected from required scalar fields coming back all
  # NA; a codebook whose required properties are all arrays or nested objects
  # offers no such signal, because ellmer renders those as list-columns where a
  # missing value and a schema-valid empty one are the same zero-length cell.
  #
  # So on an endpoint whose enforcement cannot be verified, a structured call
  # over such a codebook would be trusted with no way to check it. Validate
  # locally instead, and say why. `structured = "structured"` remains the way
  # to ask for provider enforcement regardless.
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  undetectable <- allow_skip &&
    !is.null(provider) &&
    !provider_enforces_schema(provider) &&
    !length(required_scalar_fields(codebook$schema))

  if (undetectable) {
    return(list(
      ok = FALSE,
      undetectable = TRUE,
      error = paste0(
        "this endpoint's schema enforcement cannot be verified, and the ",
        "codebook has no required scalar field whose absence would reveal a ",
        "failed structured call"
      )
    ))
  }

  warn_unenforced_schema(chat, model)

  # Whether a response ran into a declared output limit is knowable only from
  # the token counts, which ellmer attaches on request; the finish reason
  # itself does not survive parallel_chat_structured(). See
  # mark_truncated_rows() for what is inferred from them, and why.
  cap <- declared_max_tokens(chat_args)
  keep_tokens <- isTRUE(execution_args$include_tokens)
  if (!is.null(cap)) {
    execution_args$include_tokens <- TRUE
  }

  run <- function(chat) {
    if (batch) {
      with_extraction_errors(do.call(ellmer::batch_chat_structured, c(
        list(chat = chat, prompts = prompts, type = codebook$schema),
        execution_args
      )))
    } else {
      with_extraction_errors(do.call(ellmer::parallel_chat_structured, c(
        list(chat = chat, prompts = prompts, type = codebook$schema),
        execution_args
      )))
    }
  }

  attempt <- tryCatch(
    list(ok = TRUE, value = run(chat)),
    error = function(e) list(ok = FALSE, error = strip_ansi(conditionMessage(e)))
  )

  # Alibaba Model Studio refuses `response_format` unless the word "json"
  # appears in the messages. That is a request-shape requirement, not a coding
  # one, and the endpoint does enforce the schema once the request is accepted
  # -- so satisfy it and retry rather than falling back and losing enforcement.
  # The added sentence concerns output format only and names no coding
  # criterion, so the substantive instrument is unchanged.
  #
  # Defensive: not reachable against ellmer's current request shape, which
  # sends json_schema rather than json_object. See is_json_word_error().
  if (!isTRUE(attempt$ok) && is_json_word_error(attempt$error)) {
    retry_prompt <- paste(
      system_prompt,
      "Return your answer as a single JSON object.",
      sep = "\n\n"
    )
    attempt <- tryCatch(
      list(ok = TRUE, value = run(build_chat(retry_prompt))),
      error = function(e) list(ok = FALSE, error = strip_ansi(conditionMessage(e)))
    )
  }

  if (isTRUE(attempt$ok)) {
    attempt$value <- mark_truncated_rows(
      attempt$value, codebook$schema, cap, keep_tokens = keep_tokens
    )
  }

  if (isTRUE(attempt$ok) && all_required_missing(attempt$value, codebook$schema)) {
    attempt <- list(
      ok = FALSE,
      error = paste0(
        "the structured call returned no usable values (every required field ",
        "was NA in all ", nrow(attempt$value),
        if (nrow(attempt$value) == 1L) " row" else " rows",
        "); the endpoint appears ",
        "not to honour the JSON schema"
      )
    )
  }

  attempt
}


#' Run an ellmer structured call, keeping the extraction failures it reports
#'
#' ellmer's `multi_convert()` attaches `.error` only to turns whose request
#' failed. A turn that came back but yielded no structured data -- a refusal
#' in prose, say -- is reported in a warning listing the affected rows and
#' then dropped: scalar fields become `NA` and an array becomes a zero-length
#' cell, with no `.error`. For an array-only schema that is indistinguishable
#' from a valid empty answer, so the failure would be invisible to
#' [qlm_failures()] (#132). Capture the warning as it passes and record an
#' `.error` for each listed row that has none. The warning still reaches the
#' user; nothing is muffled.
#'
#' This depends on the format of ellmer's warning, one `* <row>: <message>`
#' line per failure. `test-qlm_failures.R` pins that against ellmer's actual
#' `multi_convert()`, so an upstream change fails the suite rather than
#' silently restoring the blind spot.
#'
#' @param expr A call returning what [ellmer::parallel_chat_structured()] or
#'   [ellmer::batch_chat_structured()] returns.
#'
#' @return The value of `expr`, with `.error` set for reported rows when it is
#'   a data frame; unchanged otherwise.
#' @keywords internal
#' @noRd
with_extraction_errors <- function(expr) {
  failures <- NULL
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      found <- parse_extraction_warning(conditionMessage(w))
      if (!is.null(found)) {
        failures <<- rbind(failures, found)
      }
    }
  )
  attach_extraction_errors(value, failures)
}


#' Read the rows named in ellmer's extraction warning
#'
#' @param msg The warning's condition message.
#'
#' @return A data frame with `index` and `message`, or `NULL` when `msg` is
#'   not that warning or names no rows.
#' @keywords internal
#' @noRd
parse_extraction_warning <- function(msg) {
  msg <- strip_ansi(msg)
  if (!grepl("^Failed to extract data from", msg)) {
    return(NULL)
  }
  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
  parts <- regmatches(lines, regexec("^\\s*[*]\\s*([0-9]+):\\s*(.*)$", lines))
  hits <- Filter(function(m) length(m) == 3L, parts)
  if (!length(hits)) {
    return(NULL)
  }
  data.frame(
    index = as.integer(vapply(hits, `[`, character(1), 2L)),
    message = vapply(hits, `[`, character(1), 3L),
    stringsAsFactors = FALSE
  )
}


#' Record extraction failures in .error
#'
#' Only rows with no `.error` already are touched; a request failure ellmer
#' recorded takes precedence. `.error` is placed where ellmer puts it, before
#' any token and cost columns, so both origins yield the same column order.
#'
#' @param results What the structured call returned.
#' @param failures The data frame from `parse_extraction_warning()`, or `NULL`.
#'
#' @return `results`, with `.error` set where needed.
#' @keywords internal
#' @noRd
attach_extraction_errors <- function(results, failures) {
  if (is.null(failures) || !is.data.frame(results)) {
    return(results)
  }
  failures <- failures[failures$index >= 1L & failures$index <= nrow(results), ]
  if (!nrow(failures)) {
    return(results)
  }

  had_error <- ".error" %in% names(results)
  errors <- if (had_error) {
    as.list(results$.error)
  } else {
    vector("list", nrow(results))
  }
  for (k in seq_len(nrow(failures))) {
    i <- failures$index[k]
    if (is.null(errors[[i]])) {
      errors[[i]] <- extraction_error(failures$message[k])
    }
  }
  results$.error <- errors

  if (!had_error) {
    usage <- intersect(
      c("input_tokens", "output_tokens", "cached_input_tokens", "cost"),
      names(results)
    )
    others <- setdiff(names(results), c(".error", usage))
    results <- results[, c(others, ".error", usage)]
  }
  results
}


#' The output limit the caller declared, if any
#'
#' Read from `params(max_tokens = )` and nowhere else. ellmer fills in a
#' provider default when none is set (4096 for Anthropic, at the time of
#' writing), but that default lives in its request builder and is not visible
#' from here, so an undeclared limit is treated as unknown rather than guessed
#' at. `api_args` is not consulted either: quallmer forwards it unchanged and
#' does not inspect it.
#'
#' @param chat_args List of arguments destined for [ellmer::chat()].
#'
#' @return A positive number, or `NULL`.
#' @keywords internal
#' @noRd
declared_max_tokens <- function(chat_args) {
  params <- chat_args$params
  if (!is.list(params)) {
    return(NULL)
  }
  cap <- params$max_tokens
  if (is.numeric(cap) && length(cap) == 1L && is.finite(cap) && cap > 0) {
    as.numeric(cap)
  } else {
    NULL
  }
}


#' Flag structured rows that used the whole output budget and returned nothing
#'
#' [ellmer::parallel_chat_structured()] returns the converted table and
#' discards the turns, and with them the finish reason. A response the provider
#' cut off at `max_tokens` therefore arrives as an ordinary row with nothing in
#' it -- `NA` scalars, zero-length arrays -- at full cost. When the cut leaves
#' unparsable JSON, ellmer warns and `with_extraction_errors()` records the
#' parse error; when it leaves nothing, as on Anthropic's forced-tool path,
#' there is no `.error` at all and the row is indistinguishable from a unit to
#' which nothing applied. ellmer's sequential `chat_structured()` raises an
#' error for the very same response.
#'
#' What the table does carry, when asked, is the output token count. A row
#' that spent every token of a declared limit *and* has nothing in it was, to
#' a near certainty, cut off: a complete answer that happened to end exactly at
#' the limit would carry data, and an empty answer does not need the whole
#' budget. Both conditions are required, so "empty is not missing" still
#' holds: an empty row below the limit is left alone, since a required array
#' may validly be `[]`.
#'
#' Only a limit the caller declared is known here (see `declared_max_tokens()`).
#' The finish reason is the right signal and is read directly on the JSON
#' path; using it here too needs ellmer to return it from the parallel call.
#'
#' @param results The result of [ellmer::parallel_chat_structured()], with
#'   token columns when `cap` is not `NULL`.
#' @param schema The codebook schema.
#' @param cap The declared output limit, or `NULL` when none is known.
#' @param keep_tokens Whether the caller asked for the token columns. If not,
#'   they were requested only for this check and are removed again.
#'
#' @return `results`, with `.error` set for the flagged rows.
#' @keywords internal
#' @noRd
mark_truncated_rows <- function(results, schema, cap, keep_tokens = TRUE) {
  if (is.null(cap) || !is.data.frame(results) || !nrow(results) ||
      !"output_tokens" %in% names(results)) {
    return(results)
  }
  spent <- results$output_tokens
  # A request that failed outright spent no output tokens, so it can never
  # land here and keeps ellmer's reason. A row that did spend the whole limit
  # may already carry an extraction error from with_extraction_errors()
  # ("premature EOF"): that is the symptom, and the cut is the cause, so the
  # reason recorded here replaces it.
  hit <- !is.na(spent) & spent >= cap & blank_rows(results, schema)

  if (!keep_tokens) {
    token_columns <- c("input_tokens", "output_tokens", "cached_input_tokens")
    results[intersect(token_columns, names(results))] <- NULL
  }
  if (!any(hit)) {
    return(results)
  }

  errors <- recorded_errors(results)
  for (i in which(hit)) {
    errors[i] <- list(simpleError(paste0(
      "The response used the whole max_tokens limit of ",
      format(cap, big.mark = ","), " and returned nothing, so it was most ",
      "likely cut off; raise the limit with params(max_tokens = )."
    )))
  }
  results$.error <- errors

  # ellmer's column order: coded values, then .error, then usage
  trailing <- intersect(
    c("input_tokens", "output_tokens", "cached_input_tokens", "cost"),
    names(results)
  )
  results <- results[c(setdiff(names(results), c(".error", trailing)), ".error", trailing)]

  n <- sum(hit)
  cli::cli_warn(c(
    "{n} response{?s} from the structured call used the whole {.code max_tokens} limit of {cap} and returned nothing.",
    "i" = "{cli::qty(n)}{?It was/They were} most likely cut off; raise the limit with {.code params(max_tokens = )}.",
    "i" = "{.fn qlm_failures} lists the affected units."
  ))
  results
}


#' Which rows of a converted result carry no coded value at all?
#'
#' Reads the columns the schema defines, ignoring `.error` and the usage
#' columns. A scalar is blank when `NA`; an array, which converts to a
#' list-column, when its cell is empty; a nested object, which converts to a
#' data-frame column, when every one of its columns is blank.
#'
#' @param results A data frame as returned by ellmer's converter.
#' @param schema The codebook schema.
#'
#' @return A logical vector, one element per row.
#' @keywords internal
#' @noRd
blank_rows <- function(results, schema) {
  meta <- c(".error", "input_tokens", "output_tokens", "cached_input_tokens", "cost")
  cols <- if (inherits(schema, "ellmer::TypeObject")) {
    intersect(names(schema@properties), names(results))
  } else {
    setdiff(names(results), meta)
  }
  if (!length(cols)) {
    return(rep(FALSE, nrow(results)))
  }
  Reduce(`&`, lapply(results[cols], blank_column))
}

blank_column <- function(v) {
  if (is.data.frame(v)) {
    if (!ncol(v)) {
      return(rep(TRUE, nrow(v)))
    }
    return(Reduce(`&`, lapply(v, blank_column)))
  }
  if (is.list(v)) {
    return(vapply(v, blank_cell, logical(1)))
  }
  is.na(v)
}

blank_cell <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (is.data.frame(x)) {
    return(nrow(x) == 0L)
  }
  length(x) == 0L
}


#' An error recorded for a response ellmer could extract nothing from
#'
#' Carries a class of its own so that the schema-enforcement check in
#' `all_required_missing()` can tell it from a request failure or a cut-off
#' response. The distinction matters: here the endpoint did answer, and the
#' answer was not the schema, which is precisely the evidence that check
#' looks for; a request that never got an answer, or an answer the provider
#' cut short, is not.
#'
#' @param message The message ellmer gave for the row.
#'
#' @return A condition inheriting from `simpleError`.
#' @keywords internal
#' @noRd
extraction_error <- function(message) {
  structure(
    simpleError(message),
    class = c("quallmer_extraction_error", "simpleError", "error", "condition")
  )
}

is_extraction_error <- function(e) {
  inherits(e, "quallmer_extraction_error")
}


#' Note when the provider does not guarantee schema enforcement
#'
#' Emitted once per session. The structured path is being trusted to return
#' conforming output, and for anything on ellmer's generic OpenAI-compatible
#' request path that trust is unverified: `strict = TRUE` is sent and may
#' simply be ignored. Informational rather than a warning, because it describes
#' a property of the endpoint, not a problem with this run.
#'
#' @param chat An [ellmer::Chat] object.
#' @param model The model string, for the message.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
warn_unenforced_schema <- function(chat, model) {
  if (isTRUE(getOption("quallmer.quiet_schema_note", FALSE))) {
    return(invisible(NULL))
  }
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  if (is.null(provider) || provider_enforces_schema(provider)) {
    return(invisible(NULL))
  }

  cli::cli_inform(
    c(
      "!" = "{.val {model}} is reached through an OpenAI-compatible endpoint, which may accept the output schema without enforcing it.",
      "i" = "Non-conforming values arrive as {.val NA}, indistinguishable from missing data.",
      "i" = "Use {.code structured = \"json\"} to validate each response against the codebook locally.",
      "i" = "Silence this with {.code options(quallmer.quiet_schema_note = TRUE)}."
    ),
    .frequency = "once",
    .frequency_id = "quallmer_schema_enforcement"
  )
  invisible(NULL)
}


#' Create a qlm_coded object (internal)
#'
#' Low-level constructor for qlm_coded objects. This function is not exported
#' and is intended for internal use by [qlm_code()] and [qlm_replicate()].
#'
#' The object is a tibble with additional qlm_coded class and attributes.
#'
#' @param results Data frame of coded results with id column.
#' @param codebook A qlm_codebook object.
#' @param data The original input data (x from qlm_code).
#' @param input_type Type of input ("text" or "image").
#' @param chat_args List of arguments passed to ellmer::chat().
#' @param execution_args List of arguments passed to ellmer::parallel_chat_structured()
#'   or ellmer::batch_chat_structured(). For backward compatibility, also accepts
#'   pcs_args as an alias.
#' @param batch Logical indicating whether batch processing was used.
#' @param metadata List of metadata (timestamp, versions, etc.).
#' @param name Character string identifying this run.
#' @param call The call that created this object.
#' @param parent Character string identifying parent run (NULL for originals).
#' @param pcs_args Deprecated. Use execution_args instead.
#'
#' @return A qlm_coded object (tibble with attributes).
#' @importFrom utils head
#' @keywords internal
#' @noRd
new_qlm_coded <- function(results, codebook, data, input_type, chat_args,
                           execution_args = NULL, batch = FALSE, metadata,
                           name, call, parent = NULL, pcs_args = NULL) {
  # Backward compatibility: if pcs_args is provided but execution_args is not
  if (is.null(execution_args) && !is.null(pcs_args)) {
    execution_args <- pcs_args
  }
  # Rename id column to .id
  names(results)[names(results) == "id"] <- ".id"

  # Exactly one identifier column, holding a key. Every merge downstream --
  # comparison, validation, backfill -- keys on .id and would silently pair
  # the wrong rows (#156).
  if (sum(names(results) == ".id") != 1L) {
    cli::cli_abort(c(
      "{.arg results} must have exactly one {.field .id} column; found {sum(names(results) == '.id')}.",
      "i" = "An {.code id} column is renamed to {.field .id}, so the two must not both be present."
    ))
  }
  check_ids(results$.id, what = "{.field .id}")

  # Reorder columns to put .id first
  results <- results[, c(".id", setdiff(names(results), ".id"))]

  # Convert to tibble (always available via ellmer)
  results <- tibble::as_tibble(results)

  # Add qlm_coded class and attributes with new metadata structure
  # Build object metadata - include source and is_gold if present
  object_meta <- list(
    batch = batch,
    call = call,
    chat_args = chat_args,
    execution_args = execution_args,
    parent = parent,
    n_units = metadata$n_units,
    input_type = input_type
  )

  # Add source and is_gold from metadata if present (for human-coded data)
  if (!is.null(metadata$source)) {
    object_meta$source <- metadata$source
  }
  if (!is.null(metadata$is_gold)) {
    object_meta$is_gold <- metadata$is_gold
  }
  if (!is.null(metadata$backend)) {
    object_meta$backend <- metadata$backend
  }
  if (!is.null(metadata$structured)) {
    object_meta$structured <- metadata$structured
  }

  # Build user metadata: start with name and notes, then add custom metadata
  user_meta <- list(
    name = name,
    notes = metadata$notes
  )

  # Add any custom metadata fields (exclude system, object, and user-handled fields)
  system_fields <- c("timestamp", "ellmer_version", "quallmer_version", "R_version")
  object_fields <- c("n_units", "source", "is_gold", "backend", "structured")
  user_handled <- c("name", "notes")
  exclude_fields <- c(system_fields, object_fields, user_handled)

  custom_metadata <- metadata[!names(metadata) %in% exclude_fields]
  if (length(custom_metadata) > 0) {
    user_meta <- c(user_meta, custom_metadata)
  }

  structure(
    results,
    class = c("qlm_coded", class(results)),
    data = data,
    codebook = codebook,
    meta = list(
      user = user_meta,
      object = object_meta,
      system = list(
        timestamp = metadata$timestamp,
        ellmer_version = metadata$ellmer_version,
        quallmer_version = metadata$quallmer_version,
        R_version = metadata$R_version
      )
    )
  )
}


#' Print a qlm_coded object
#'
#' @param x A qlm_coded object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects (printing to console).
#' @keywords internal
#' @export
print.qlm_coded <- function(x, ...) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  codebook_attr <- attr(x, "codebook")

  # Print header
  cat("# quallmer coded object\n")
  cat("# Run:      ", meta_attr$user$name, "\n", sep = "")

  # Distinguish human vs LLM coding
  if (!is.null(meta_attr$object$source) && meta_attr$object$source == "human") {
    cat("# Source:   Human coder\n")
    if (!is.null(codebook_attr$name) && codebook_attr$name != "Human-coded data") {
      cat("# Codebook: ", codebook_attr$name, "\n", sep = "")
    }
  } else {
    cat("# Codebook: ", codebook_attr$name, "\n", sep = "")
    cat("# Model:    ", meta_attr$object$chat_args$name %||% "unknown", "\n", sep = "")
  }

  # Show if this is a gold standard
  if (!is.null(meta_attr$object$is_gold) && isTRUE(meta_attr$object$is_gold)) {
    cat("# Gold:     Yes\n")
  }

  # Units attempted, and how many came back with nothing usable, so that a
  # partly failed run cannot look complete (#132). Failed rows beyond the
  # printed head of the tibble would otherwise go unseen. Row subsetting keeps
  # the class and the original count, so when the rows present differ from
  # the units attempted, say so, and count over the rows present.
  n_units <- meta_attr$object$n_units
  n_rows <- nrow(x)
  n_failed <- sum(failed_units(x))
  units <- if (!is.null(n_units) && n_units != n_rows) {
    paste0(n_units, " attempted, ", n_rows, " present")
  } else {
    as.character(n_units %||% n_rows)
  }
  breakdown <- if (n_failed > 0) {
    paste0(" (", n_rows - n_failed, " scored, ", n_failed, " failed)")
  } else {
    ""
  }
  cat("# Units:    ", units, breakdown, "\n", sep = "")

  if (!is.null(meta_attr$object$parent)) {
    cat("# Parent:   ", meta_attr$object$parent, "\n", sep = "")
  }

  # Show notes if present
  if (!is.null(meta_attr$user$notes)) {
    cat("# Notes:    ", meta_attr$user$notes, "\n", sep = "")
  }

  cat("\n")

  # Print data using parent class method
  NextMethod()
}

