#' Segment texts using an LLM
#'
#' Applies a codebook to input texts to segment them into thematic or conceptual
#' units, returning a [quanteda::corpus()] where each segment is a document.
#' This is the LLM-powered analogue of [quanteda::corpus_segment()].
#'
#' The codebook schema defines additional document-level variables (docvars)
#' for each segment. A `text` field (the verbatim segment text) is always added
#' automatically and must not appear in the schema. Measurement `levels` defined
#' in the codebook are not applicable to segmentation and are silently ignored.
#'
#' @inheritParams qlm_code
#' @param x A character vector of texts or a [quanteda::corpus()] object.
#'   Named character vectors use names as document identifiers; unnamed vectors
#'   use sequential labels (`text1`, `text2`, ...).
#' @param codebook A codebook object created with [qlm_codebook()]. The schema
#'   should be a [type_object()] whose fields become docvars in the output
#'   corpus. Do not include a field named `text`; it is reserved for the
#'   verbatim segment text and is added automatically.
#' @param ... Additional arguments passed to [ellmer::chat()] or
#'   [ellmer::parallel_chat_structured()]. Arguments recognized by
#'   [ellmer::parallel_chat_structured()] are routed there; all other arguments
#'   (including provider-specific arguments like `base_url`, `credentials`, or
#'   `api_args` for OpenAI-compatible endpoints) are passed to [ellmer::chat()].
#' @param notes Optional character string with descriptive notes about this
#'   segmentation run. Default is `NULL`.
#'
#' @return A [quanteda::corpus()] where each segment is a document. Document
#'   names follow the `{source}.{i}` convention of [quanteda::corpus_segment()].
#'   Docvars include:
#'   \describe{
#'     \item{`docid`}{Name of the source document.}
#'     \item{`segid`}{Integer segment index within the source document.}
#'     \item{...}{Any fields defined in the codebook schema.}
#'     \item{...}{Original docvars inherited from the input (if `x` is a
#'       corpus).}
#'   }
#'
#' @seealso [qlm_code()] for document-level coding, [qlm_codebook()] for
#'   creating codebooks, [quanteda::corpus_segment()] for pattern-based
#'   segmentation.
#'
#' @examples
#' \dontrun{
#' # Aspect-based segmentation of a hotel review (character vector input
#' # returns a data.frame).
#' review <- paste(
#'   "The room was clean and tidy, despite being rather basic in its furnishings.",
#'   "The location of the hotel was really great, however.",
#'   "We loved the proximity to both public transport and to the city's main attractions."
#' )
#'
#' cb_absa <- qlm_codebook(
#'   name = "Aspect-based segmentation",
#'   instructions = paste(
#'     "Segment the text according to the distinct aspects (topics or features).",
#'     "Each segment will continue as long as it is part of the same aspect.",
#'     "An aspect-based segment may be more than one sentence or may be just a",
#'     "part of a sentence.",
#'     "",
#'     "Aspects in hotel reviews include: cleanliness, features, location, service,",
#'     "and value. Return each aspect segment with its verbatim text and a short",
#'     "aspect label."
#'   ),
#'   schema = type_object(
#'     aspect    = type_string("Short aspect label"),
#'     sentiment = type_enum(c("negative", "neutral", "positive"),
#'                           "Sentiment toward this aspect")
#'   )
#' )
#'
#' segs <- qlm_segment(review, cb_absa, model = "anthropic")
#' quanteda::docvars(segs)
#' #   docid segid      aspect sentiment
#' # 1 text1     1 cleanliness  positive
#' # 2 text1     2    features  negative
#' # 3 text1     3    location  positive
#'
#' # Corpus input preserves existing docvars
#' reviews_corp <- quanteda::corpus(
#'   c(hotel_a = review),
#'   docvars = data.frame(city = "London", stars = 4L)
#' )
#' segs_corp <- qlm_segment(reviews_corp, cb_absa, model = "anthropic")
#' quanteda::docvars(segs_corp)
#' }
#'
#' @export
qlm_segment <- function(x, codebook, model, ..., name = NULL, notes = NULL) {
  rlang::check_installed(
    "quanteda",
    reason = "to return a segmented corpus from {.fn qlm_segment}"
  )

  if (inherits(x, "corpus")) {
    doc_texts    <- as.character(x)
    doc_names    <- quanteda::docnames(x)
    parent_dvars <- quanteda::docvars(x)
  } else if (is.character(x)) {
    doc_texts    <- unname(x)
    doc_names    <- if (!is.null(names(x))) names(x) else paste0("text", seq_along(x))
    parent_dvars <- NULL
  } else {
    cli::cli_abort(
      "{.arg x} must be a character vector or quanteda corpus, not {.cls {class(x)[1]}}."
    )
  }

  # Validate codebook class
  if (!inherits(codebook, "qlm_codebook")) {
    cli::cli_abort(c(
      "{.arg codebook} must be a {.cls qlm_codebook} object.",
      "i" = "Create one with {.fn qlm_codebook}."
    ))
  }

  # Segmentation only makes sense for text input
  if (codebook$input_type != "text") {
    cli::cli_abort(c(
      "{.fn qlm_segment} only supports text input.",
      "x" = "The codebook has {.arg input_type} = {.val {codebook$input_type}}."
    ))
  }

  # Schema must be a type_object
  if (!inherits(codebook$schema, "ellmer::TypeObject")) {
    cli::cli_abort(c(
      "The {.arg schema} in {.arg codebook} must be a {.fn type_object}.",
      "i" = "Each field in the schema becomes a column in the output."
    ))
  }

  # "text" is reserved for the segment text
  if ("text" %in% names(codebook$schema@properties)) {
    cli::cli_abort(c(
      "The schema must not include a field named {.val text}.",
      "i" = "The {.val text} field is reserved and added automatically by {.fn qlm_segment}."
    ))
  }

  # Build internal schema: text field prepended to user-defined fields
  user_props <- codebook$schema@properties
  items_schema <- do.call(
    ellmer::type_object,
    c(
      list(text = ellmer::type_string(
        "Verbatim text of this segment, copied exactly from the input"
      )),
      user_props
    )
  )
  internal_schema <- ellmer::type_array(items = items_schema)

  # Build system prompt from role and instructions
  system_prompt <- if (!is.null(codebook$role)) {
    paste(codebook$role, codebook$instructions, sep = "\n\n")
  } else {
    codebook$instructions
  }

  # Route ... arguments to chat() or parallel_chat_structured()
  # execution_args go to parallel_chat_structured
  # Everything else (including provider-specific args like base_url) goes to chat()
  pcs_arg_names  <- names(formals(ellmer::parallel_chat_structured))
  dots       <- list(...)
  dot_names  <- names(dots)
  execution_args <- dots[dot_names %in% pcs_arg_names]
  # chat_args gets everything NOT destined for execution functions
  # This allows provider-specific args (base_url, credentials, api_args, etc.)
  # to pass through to ellmer::chat() which forwards them to the provider
  chat_args      <- dots[!dot_names %in% pcs_arg_names]

  # Build chat object
  chat <- do.call(ellmer::chat, c(
    list(name = model, system_prompt = system_prompt),
    chat_args
  ))

  # Execute: returns a list of tibbles (one per document) for array types
  results_list <- do.call(ellmer::parallel_chat_structured, c(
    list(chat = chat, prompts = as.list(doc_texts), type = internal_schema),
    execution_args
  ))

  # Collect per-segment data across all documents
  all_texts    <- character(0)
  all_docnames <- character(0)
  dvar_rows    <- vector("list", length(doc_texts))

  for (i in seq_along(doc_texts)) {
    segs <- results_list[[i]]

    if (is.null(segs) || !is.data.frame(segs) || nrow(segs) == 0L) {
      cli::cli_warn("Document {.val {doc_names[i]}} produced no segments.")
      next
    }

    n_segs       <- nrow(segs)
    all_texts    <- c(all_texts, segs$text)
    all_docnames <- c(all_docnames, paste0(doc_names[i], ".", seq_len(n_segs)))

    # Compute character-level positions by aligning segments to source text
    positions <- tryCatch(
      align_segments(doc_texts[i], segs$text),
      error = function(e) NULL
    )

    dv <- data.frame(
      docid      = rep(doc_names[i], n_segs),
      segid      = seq_len(n_segs),
      char_start = if (!is.null(positions)) positions$start else NA_integer_,
      char_end   = if (!is.null(positions)) positions$end   else NA_integer_,
      stringsAsFactors = FALSE
    )

    for (field in names(user_props)) {
      dv[[field]] <- segs[[field]]
    }

    # Inherit parent docvars when input is a corpus
    if (!is.null(parent_dvars) && ncol(parent_dvars) > 0L) {
      for (col in names(parent_dvars)) {
        dv[[col]] <- parent_dvars[i, col]
      }
    }

    dvar_rows[[i]] <- dv
  }

  all_dvars <- do.call(rbind, Filter(Negate(is.null), dvar_rows))

  out <- quanteda::corpus(all_texts, docnames = all_docnames)
  if (!is.null(all_dvars)) {
    quanteda::docvars(out) <- all_dvars
  }

  # Mark as a segmented corpus and store metadata
  quanteda::meta(out, "qlm_segment") <- TRUE
  quanteda::meta(out, "name") <- name
  continuum_lengths <- stats::setNames(
    nchar(doc_texts),
    doc_names
  )
  quanteda::meta(out, "continuum_lengths") <- continuum_lengths

  out
}
