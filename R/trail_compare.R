# ===============================================
# trail_matrix: records -> coder-style wide data
# ===============================================

#' Convert Trail records to coder-style wide data
#'
#' Treat each setting/record in a `trail_compare` object as a separate
#' coder and convert the annotations into a wide data frame suitable for
#' intercoder reliability analysis or other comparisons.
#'
#' @param x Either a \code{trail_compare} object or a named list of
#'   \code{trail_record} objects.
#' @param id_col Character scalar. Name of the column that identifies
#'   units (documents, paragraphs, etc.). Must be present in each
#'   record's \code{annotations} data.
#' @param label_col Character scalar. Name of the column in each
#'   record's \code{annotations} data containing the code or label of
#'   interest.
#'
#' @return A data frame with one row per unit and one column per
#'   setting/record. The unit ID column is retained under the name
#'   \code{id_col}.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
#' @export
trail_matrix <- function(x,
                         id_col    = "id",
                         label_col = "label") {

  records <- NULL
  if (inherits(x, "trail_compare")) {
    records <- x$records
  } else if (is.list(x) && length(x) > 0L &&
             all(vapply(x, inherits, logical(1), "trail_record"))) {
    records <- x
  } else {
    stop("x must be a 'trail_compare' or a list of 'trail_record' objects.")
  }

  if (is.null(names(records)) || any(!nzchar(names(records)))) {
    stop("Records must be named; names are used as setting/coder IDs.")
  }

  df_list <- lapply(names(records), function(name) {
    ann <- records[[name]]$annotations
    if (!id_col %in% names(ann)) {
      stop("id_col '", id_col, "' not found in annotations for record '", name, "'.")
    }
    if (!label_col %in% names(ann)) {
      stop("label_col '", label_col, "' not found in annotations for record '", name, "'.")
    }
    data.frame(
      unit_id = ann[[id_col]],
      coder   = name,
      code    = ann[[label_col]],
      stringsAsFactors = FALSE
    )
  })

  long_all <- dplyr::bind_rows(df_list)

  wide <- tidyr::pivot_wider(
    long_all,
    names_from  = coder,
    values_from = code
  )

  # Expose the ID column under the requested name
  names(wide)[names(wide) == "unit_id"] <- id_col
  wide
}

# =====================================================
# trail_icr: compute intercoder reliability from matrix
# =====================================================

#' Compute intercoder reliability across Trail settings
#'
#' Convenience helper to compute intercoder reliability across multiple
#' Trail records or a `trail_compare` object by treating each setting as
#' a coder and calling \code{validate()} in \code{mode = "icr"}.
#'
#' @param x A \code{trail_compare} object or a list of \code{trail_record}
#'   objects.
#' @param id_col Character scalar. Name of the unit identifier column in
#'   the resulting wide data (defaults to "id").
#' @param label_col Character scalar. Name of the label column in each
#'   record's annotations (defaults to "label").
#' @param min_coders Integer. Minimum number of non-missing coders per
#'   unit required for inclusion.
#' @param icr_fun Function used to compute intercoder reliability.
#'   Defaults to \code{validate()}, which is expected to accept
#'   \code{data}, \code{id}, \code{coder_cols}, \code{min_coders},
#'   and \code{mode = "icr"}. It should also understand
#'   \code{output = "list"} to return a named list of statistics.
#' @param ... Additional arguments passed on to \code{icr_fun}.
#'
#' @return The result of calling \code{icr_fun()} on the wide data.
#'   With the default \code{validate()}, this is a named list of
#'   intercoder reliability statistics.
#'
#' @seealso
#' * `trail_compare()` – run the same task across multiple settings
#' * `trail_matrix()` – underlying wide data used here
#' * `validate()` – core validation / ICR engine
#'
#' @export
trail_icr <- function(
    x,
    id_col     = "id",
    label_col  = "label",
    min_coders = 2L,
    icr_fun    = validate,
    ...) {

  # Validate min_coders
  if (!is.numeric(min_coders) || length(min_coders) != 1L ||
      is.na(min_coders) || min_coders < 2L || min_coders != as.integer(min_coders)) {
    stop("`min_coders` must be an integer >= 2.")
  }
  min_coders <- as.integer(min_coders)

  wide <- trail_matrix(x, id_col = id_col, label_col = label_col)
  coder_cols <- setdiff(names(wide), id_col)

  if (length(coder_cols) < 2L) {
    stop("Need at least two setting/coder columns to compute intercoder reliability.")
  }

  args <- list(
    data       = wide,
    id         = id_col,
    coder_cols = coder_cols,
    min_coders = min_coders,
    mode       = "icr",
    output     = "list"
  )

  do.call(icr_fun, c(args, list(...)))
}

# ============================================================
# trail_compare: run settings AND return matrix + ICR together
# ============================================================

#' trail_compare: run a task across multiple settings and compute reliability
#'
#' Apply a quallmer task to the same text data under multiple settings,
#' producing one `trail_record` per setting, and directly compute a
#' coder-style wide matrix plus intercoder reliability scores.
#'
#' @param data A data frame containing the text to be annotated.
#' @param text_col Character scalar. Name of the text column containing
#'   text units to annotate.
#' @param task A quallmer task object describing what to extract or label.
#' @param settings A named list of `trail_setting` objects. The list
#'   names serve as identifiers for each setting (similar to coder IDs).
#' @param id_col Optional character scalar identifying the unit column.
#'   If `NULL`, a consistent temporary ID (`".trail_unit_id"`) is created
#'   and added to the input data so annotations from all settings can be
#'   aligned.
#' @param label_col Character scalar. Name of the label column in each
#'   record's `annotations` data that should be used as the code for
#'   comparison (e.g. `"label"`, `"score"`, `"category"`).
#' @param cache_dir Optional character scalar specifying a directory to
#'   cache LLM outputs. Passed to `trail_record()`. Defaults to
#'   `"trail_cache"`.
#' @param overwrite Logical. If `TRUE`, ignore all cached results and
#'   recompute annotations for every setting.
#' @param annotate_fun Annotation backend function used by
#'   `trail_record()` (default = `annotate()`).
#' @param min_coders Minimum number of non-missing coders per unit
#'   required for inclusion in the intercoder reliability calculation.
#'
#' @return A `trail_compare` object with components:
#'   \describe{
#'     \item{records}{Named list of `trail_record` objects (one per setting)}
#'     \item{matrix}{Wide coder-style annotation matrix (settings = columns)}
#'     \item{icr}{Named list of intercoder reliability statistics}
#'     \item{meta}{Metadata on settings, identifiers, task, timestamp, etc.}
#'   }
#'
#' @details
#' All settings are applied to the same text units. Because the ID
#' column is shared across settings, their annotation outputs can be
#' directly compared via the `matrix` component, and summarized using
#' intercoder reliability statistics in `icr`.
#'
#' @seealso
#' * `trail_record()` – run a task for a single setting
#' * `trail_matrix()` – align records into coder-style wide format
#' * `trail_icr()` – compute intercoder reliability across settings
#'
#' @export
trail_compare <- function(
    data,
    text_col,
    task,
    settings,
    id_col       = NULL,
    label_col    = "label",
    cache_dir    = "trail_cache",
    overwrite    = FALSE,
    annotate_fun = annotate,
    min_coders   = 2L
) {
  # Validate min_coders
  if (!is.numeric(min_coders) || length(min_coders) != 1L ||
      is.na(min_coders) || min_coders < 2L || min_coders != as.integer(min_coders)) {
    stop("`min_coders` must be an integer >= 2.")
  }
  min_coders <- as.integer(min_coders)

  stopifnot(is.list(settings), length(settings) > 0L)
  if (is.null(names(settings)) || any(!nzchar(names(settings)))) {
    stop("settings must be a named list; names will be used as setting IDs.")
  }
  stopifnot(all(vapply(settings, inherits, logical(1), "trail_setting")))

  # Shared ID column for all records
  if (is.null(id_col)) {
    id_col <- ".trail_unit_id"
    data[[id_col]] <- seq_len(nrow(data))
  } else if (!id_col %in% names(data)) {
    stop("id_col '", id_col, "' not found in data.")
  }

  # Run all settings (LLM variants)
  records <- lapply(settings, function(s) {
    trail_record(
      data         = data,
      text_col     = text_col,
      task         = task,
      setting      = s,
      id_col       = id_col,
      cache_dir    = cache_dir,
      overwrite    = overwrite,
      annotate_fun = annotate_fun
    )
  })

  # Compute wide coder matrix + ICR using the chosen label_col
  wide <- trail_matrix(records, id_col = id_col, label_col = label_col)
  icr  <- trail_icr(records,
                    id_col     = id_col,
                    label_col  = label_col,
                    min_coders = min_coders)

  structure(
    list(
      records = records,
      matrix  = wide,
      icr     = icr,
      meta    = list(
        timestamp   = Sys.time(),
        id_col      = id_col,
        label_col   = label_col,
        text_col    = text_col,
        n_settings  = length(settings),
        setting_ids = names(settings),
        n_rows      = nrow(data),
        task_class  = class(task)
      )
    ),
    class = "trail_compare"
  )
}

# --------------------------------
# Pretty print for trail_compare
# --------------------------------

#' @export
print.trail_compare <- function(x, ...) {
  cat("Trail compare\n")
  cat("  Settings: ", paste(x$meta$setting_ids, collapse = ", "), "\n", sep = "")
  cat("  Units:    ", x$meta$n_rows, "\n", sep = "")
  cat("  Label:    ", x$meta$label_col, "\n", sep = "")
  cat("  ICR stats:\n")
  print(x$icr)
  invisible(x)
}
