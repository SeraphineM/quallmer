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
#'     produces.}
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
#' many repair attempts each unit gets. Batch processing and image codebooks
#' are not supported there, so `"auto"` will not fall back under
#' `batch = TRUE`. The path actually taken is recorded in the run metadata as
#' `backend`.
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

  run <- function(chat) {
    if (batch) {
      do.call(ellmer::batch_chat_structured, c(
        list(chat = chat, prompts = prompts, type = codebook$schema),
        execution_args
      ))
    } else {
      do.call(ellmer::parallel_chat_structured, c(
        list(chat = chat, prompts = prompts, type = codebook$schema),
        execution_args
      ))
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

  cat("# Units:    ", meta_attr$object$n_units, "\n", sep = "")

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

