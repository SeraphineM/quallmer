#' Trail record: reproducible quallmer annotation
#'
#' Run a quallmer task on a data frame with a specified LLM setting,
#' capturing metadata for reproducibility and optionally caching the
#' full result on disk.
#'
#' @param data A data frame containing the text to be annotated.
#' @param text_col Character scalar. Name of the text column.
#' @param task A quallmer task object.
#' @param setting A \code{trail_setting} object describing the LLM configuration.
#' @param id_col Optional character scalar identifying units.
#' @param cache_dir Optional directory in which to cache Trails. If \code{NULL}, caching disabled.
#' @param overwrite Whether to overwrite existing cache.
#' @param annotate_fun Function used to perform the annotation (default \code{annotate()}).
#'
#' @return An object of class \code{"trail_record"}.
#' @export
trail_record <- function(
    data,
    text_col,
    task,
    setting,
    id_col       = NULL,
    cache_dir    = "trail_cache",
    overwrite    = FALSE,
    annotate_fun = annotate
) {
  stopifnot(inherits(setting, "trail_setting"))
  stopifnot(is.data.frame(data))

  if (!text_col %in% names(data)) {
    stop("text_col '", text_col, "' not found in data.")
  }

  # Ensure unique ID column
  if (is.null(id_col)) {
    id_col <- ".trail_unit_id"
    data[[id_col]] <- seq_len(nrow(data))
  } else if (!id_col %in% names(data)) {
    stop("id_col '", id_col, "' not found in data.")
  }

  # Cache directory initialization
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Cache key
  cache_key <- digest::digest(list(data[[id_col]], data[[text_col]], task, setting))
  cache_path <- if (!is.null(cache_dir)) {
    file.path(cache_dir, paste0("trail_", cache_key, ".rds"))
  } else {
    NA_character_
  }

  # Read cache if exists
  if (!is.null(cache_dir) && file.exists(cache_path) && !overwrite) {
    cached <- readRDS(cache_path)
    if (inherits(cached, "trail_record")) return(cached)
  }

  # ---- resolve backend chat function from provider ----
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for trail_record().")
  }

  chat_fn <- switch(
    setting$provider,
    "openai" = ellmer::chat_openai,
    "ollama" = ellmer::chat_ollama,
    # extend here if you use other providers:
    # "azure"  = ellmer::chat_azure,
    stop("Unsupported provider in trail_setting: '", setting$provider, "'")
  )

  # ---- annotation call: task expects text input (character) ----
  text_vec <- as.character(data[[text_col]])

  # Build api_args: temperature + extras (extras can override)
  api_args <- c(
    list(temperature = setting$temperature),
    setting$extra
  )

  args <- list(
    .data    = text_vec,
    task     = task,
    chat_fn  = chat_fn,
    model    = setting$model,
    api_args = api_args
  )

  annotations <- do.call(annotate_fun, args)

  # Attach ID if not present; align by row position
  if (!id_col %in% names(annotations)) {
    annotations[[id_col]] <- data[[id_col]]
  }

  meta <- list(
    timestamp    = Sys.time(),
    n_rows       = nrow(data),
    provider     = setting$provider,
    model        = setting$model,
    temperature  = setting$temperature,
    api_extra    = setting$extra,
    cache_dir    = cache_dir,
    cache_path   = cache_path,
    id_col       = id_col,
    text_col     = text_col,
    task_class   = class(task),
    quallmer_ver = tryCatch(as.character(utils::packageVersion("quallmer")), error = function(e) NA_character_),
    ellmer_ver   = tryCatch(as.character(utils::packageVersion("ellmer")),  error = function(e) NA_character_),
    R_ver        = paste(R.version$major, R.version$minor, sep = ".")
  )

  rec <- structure(
    list(
      annotations = annotations,
      meta        = meta,
      setting     = setting,
      task        = task
    ),
    class = "trail_record"
  )

  if (!is.null(cache_dir)) saveRDS(rec, cache_path)

  rec
}
