#' Trail record: reproducible quallmer annotation (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `trail_record()` is deprecated. Use [qlm_code()] instead, which automatically
#' captures metadata for reproducibility. For systematic comparisons across
#' different models or settings, see [qlm_replicate()].
#'
#' @param data A data frame containing the text to be annotated.
#' @param text_col Character scalar. Name of the text column.
#' @param task A quallmer task object.
#' @param setting A \code{trail_setting} object describing the LLM configuration.
#' @param id_col Optional character scalar identifying units.
#' @param cache_dir Optional directory in which to cache Trails. If \code{NULL}, caching disabled.
#'   For examples and tests, use \code{tempdir()} to comply with CRAN policies.
#' @param overwrite Whether to overwrite existing cache.
#' @param annotate_fun Function used to perform the annotation.
#'
#' @return An object of class \code{"trail_record"}.
#' @keywords internal
#' @export
trail_record <- function(
    data,
    text_col,
    task,
    setting,
    id_col       = NULL,
    cache_dir    = NULL,
    overwrite    = FALSE,
    annotate_fun = annotate
) {
  lifecycle::deprecate_warn("0.2.0", "trail_record()", "qlm_code()")
  if (!inherits(setting, "trail_setting")) {
    cli::cli_abort("{.arg setting} must be a {.cls trail_setting} object.")
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!text_col %in% names(data)) {
    cli::cli_abort("{.arg text_col} {.val {text_col}} not found in {.arg data}.")
  }

  # Ensure unique ID column
  if (is.null(id_col)) {
    id_col <- ".trail_unit_id"
    data[[id_col]] <- seq_len(nrow(data))
  } else if (!id_col %in% names(data)) {
    cli::cli_abort("{.arg id_col} {.val {id_col}} not found in {.arg data}.")
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

  # ---- annotation call: task expects text input (character) ----
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ellmer} is required for {.fn trail_record}.",
      "i" = "Install it with {.code install.packages(\"ellmer\")}"
    ))
  }

  text_vec <- as.character(data[[text_col]])

  # Construct model_name from provider and model
  model_name <- paste0(setting$provider, "/", setting$model)

  # Build params: temperature + extras (extras can override)
  params <- c(
    list(temperature = setting$temperature),
    setting$extra
  )

  args <- list(
    .data      = text_vec,
    task       = task,
    model_name = model_name,
    params     = params
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
    cache_dir    = if (is.null(cache_dir)) NA_character_ else cache_dir,
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

#' Print a trail_record object
#'
#' @param x A trail_record object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects (printing to console).
#' @keywords internal
#' @export
print.trail_record <- function(x, ...) {
  cat("Trail record\n")
  cat("  Provider:    ", x$meta$provider, "\n", sep = "")
  cat("  Model:       ", x$meta$model, "\n", sep = "")
  cat("  Temperature: ", x$meta$temperature, "\n", sep = "")
  cat("  Units:       ", x$meta$n_rows, "\n", sep = "")
  cat("  Task:        ", paste(x$meta$task_class, collapse = ", "), "\n", sep = "")
  cat("  Timestamp:   ", format(x$meta$timestamp, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
  if (!is.na(x$meta$cache_dir)) {
    cat("  Cached:      ", x$meta$cache_path, "\n", sep = "")
  }
  invisible(x)
}
