#' Create an audit trail from quallmer objects
#'
#' Creates a complete audit trail documenting your qualitative coding workflow.
#' Following Lincoln and Guba's (1985) concept of the audit trail for
#' establishing trustworthiness in qualitative research, this function captures
#' the full decision history of your AI-assisted coding process.
#'
#' @param ... One or more quallmer objects (`qlm_coded`, `qlm_comparison`, or
#'   `qlm_validation`). When multiple objects are provided, they will be used
#'   to reconstruct the complete workflow chain.
#' @param path Optional base path for saving the audit trail. When provided,
#'   creates `{path}.rds` (complete archive) and `{path}.qmd` (human-readable
#'   report). If `NULL` (default), the trail is only returned without saving.
#'
#' @return A `qlm_trail` object containing:
#'
#'   \describe{
#'     \item{runs}{List of run information with coded data, ordered from oldest to newest}
#'     \item{complete}{Logical indicating whether all parent references were resolved}
#'   }
#'
#' @details
#' Lincoln and Guba (1985, pp. 319-320) describe six categories of audit trail
#' materials for establishing trustworthiness in qualitative research.
#' The quallmer package operationalizes these for LLM-assisted text analysis:
#'
#' \describe{
#'   \item{Raw data}{Original texts stored in coded objects}
#'   \item{Data reduction products}{Coded results from each run}
#'   \item{Data reconstruction products}{Comparisons and validations}
#'   \item{Process notes}{Model parameters, timestamps, decision history}
#'   \item{Materials relating to intentions}{Function calls documenting intent}
#'   \item{Instrument development information}{Codebook with instructions and schema}
#' }
#'
#' When `path` is provided, the function creates:
#' \itemize{
#'   \item `{path}.rds`: Complete trail object for R (reloadable with `readRDS()`)
#'   \item `{path}.qmd`: Quarto document with full audit trail documentation
#' }
#'
#' \subsection{Credentials}{
#' Both files are written to be shared, so neither carries the value of a
#' credential a run was configured with. An `api_key`, the values of
#' `api_headers` entries named like a credential, and any userinfo or
#' credential-named query parameter in a `base_url` are replaced by
#' `"<redacted>"` in each run's recorded call and chat arguments. The
#' returned object is redacted in the same way, so the trail in memory and
#' the two files agree. The trail records that a credential was supplied, not
#' what it was; a `qlm_coded` object loaded from the `.rds` therefore needs a
#' credential of its own before it can be replicated.
#'
#' Only literal values are redacted from a call. A credential read where it
#' is needed, through an environment variable ellmer reads by default or a
#' `credentials = function() Sys.getenv("MY_KEY")` argument, never enters the
#' record and is the recommended form. That is also the only `credentials`
#' callback the trail keeps, rebuilt without its environment; a callback of
#' any other shape is replaced by `"<redacted>"`, since it may hold or
#' capture the secret it returns.
#' }
#'
#' @references
#' Lincoln, Y. S., & Guba, E. G. (1985). *Naturalistic Inquiry*. Sage.
#'
#' @examples
#' # Load example coded objects
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#'
#' # View audit trail from two coding runs
#' trail <- qlm_trail(
#'   examples$example_coded_sentiment,
#'   examples$example_coded_mini
#' )
#' print(trail)
#'
#' \donttest{
#' # Save complete audit trail (creates .rds and .qmd files)
#' qlm_trail(
#'   examples$example_coded_sentiment,
#'   examples$example_coded_mini,
#'   path = tempfile("my_analysis")
#' )
#' }
#'
#' @seealso [qlm_code()], [qlm_replicate()], [qlm_compare()], [qlm_validate()]
#' @export
qlm_trail <- function(..., path = NULL) {
  objects <- list(...)

  if (length(objects) == 0) {
    cli::cli_abort("At least one object must be provided.")
  }

  # Extract runs from all objects
  runs <- list()
  redacted <- integer()
  for (i in seq_along(objects)) {
    obj <- objects[[i]]

    # Check if it's a quallmer object with run attribute
    if (!inherits(obj, c("qlm_coded", "qlm_comparison", "qlm_validation"))) {
      cli::cli_abort(c(
        "All objects must be quallmer objects.",
        "x" = "Object {i} has class {.cls {class(obj)}}.",
        "i" = "Expected {.cls qlm_coded}, {.cls qlm_comparison}, or {.cls qlm_validation}."
      ))
    }

    # Auto-upgrade old structure if needed
    obj <- upgrade_meta(obj)

    meta_attr <- attr(obj, "meta")
    if (is.null(meta_attr)) {
      cli::cli_abort(c(
        "Object {i} does not have metadata.",
        "i" = "This object may have been created with an older version of quallmer."
      ))
    }

    # A trail is made to be shared. Credential values leave the record here,
    # before anything is copied out of the metadata, so the returned object,
    # the .rds and the report agree; see redact_meta().
    redacted_meta <- redact_meta(meta_attr)
    if (!identical(redacted_meta, meta_attr)) {
      redacted <- c(redacted, i)
      meta_attr <- redacted_meta
      attr(obj, "meta") <- meta_attr
    }

    # Build a "run" structure for compatibility with trail code
    run <- list(
      name = meta_attr$user$name,
      notes = meta_attr$user$notes,
      call = meta_attr$object$call,
      parent = meta_attr$object$parent,
      metadata = list(
        timestamp = meta_attr$system$timestamp,
        quallmer_version = meta_attr$system$quallmer_version,
        R_version = meta_attr$system$R_version
      )
    )

    # Preserve the full object, minus credential values, so downstream
    # consumers can replicate without any lossy conversion. Type-specific
    # mirrors (codebook, chat_args, etc.) are kept only as convenience
    # pointers for print/report code.
    if (inherits(obj, "qlm_coded")) {
      obj <- check_qlm_coded(obj, what = sprintf("object %d", i))
      run$coded <- obj
      run$codebook <- attr(obj, "codebook")
      run$batch <- meta_attr$object$batch
      run$chat_args <- meta_attr$object$chat_args
      run$provider_resolution <- meta_attr$object$provider_resolution
      run$execution_args <- meta_attr$object$execution_args
      run$prices <- meta_attr$user$prices
      run$cost_note <- meta_attr$user$cost_note
      # The files a file-input run coded, by hash: what a reader needs to
      # reproduce the run, so it belongs in the report
      run$input_type <- meta_attr$object$input_type
      run$input_files <- meta_attr$user$input_files
      run$metadata$n_units <- meta_attr$object$n_units
      run$metadata$ellmer_version <- meta_attr$system$ellmer_version
      # Passes that completed the run, and any other model they used: the
      # audit trail is where a composite most needs to be disclosed (#136)
      run$backfill <- meta_attr$object$backfill
    } else if (inherits(obj, "qlm_comparison")) {
      run$comparison <- obj
      run$metadata$n_raters <- meta_attr$object$n_raters
      run$metadata$variables <- meta_attr$object$variables
    } else if (inherits(obj, "qlm_validation")) {
      run$validation <- obj
      run$metadata$variables <- meta_attr$object$variables
      run$metadata$average <- meta_attr$object$average
    }

    # Generate fallback name if missing
    if (is.null(run$name) || run$name == "" || length(run$name) == 0) {
      run$name <- paste0("run_", i)
    }

    # Store run with its index for ordering
    run$object_index <- i
    runs[[run$name]] <- run
  }

  # Build complete chain by following parent relationships
  chain <- list()
  complete <- TRUE

  # Find all runs and their parents
  all_names <- names(runs)
  all_parents <- unique(unlist(lapply(runs, function(r) r$parent)))
  all_parents <- all_parents[!sapply(all_parents, is.null)]

  # Check if all parents are resolved
  missing_parents <- setdiff(all_parents, all_names)
  if (length(missing_parents) > 0) {
    complete <- FALSE
  }

  # Order runs by following parent chain
  roots <- runs[sapply(runs, function(r) {
    is.null(r$parent) || !any(r$parent %in% all_names)
  })]

  # Build chain from roots
  visited <- character(0)
  queue <- names(roots)

  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]

    if (current %in% visited) next

    visited <- c(visited, current)
    chain[[current]] <- runs[[current]]

    # Find children
    children <- names(runs)[sapply(runs, function(r) {
      !is.null(r$parent) && current %in% r$parent
    })]

    queue <- c(queue, children)
  }

  trail <- structure(
    list(
      runs = chain,
      complete = complete
    ),
    class = "qlm_trail"
  )

  if (length(redacted) > 0) {
    indices <- vapply(runs, function(r) r$object_index, integer(1))
    redacted_runs <- names(runs)[indices %in% redacted]
    cli::cli_alert_info(paste(
      "Credential values recorded for {.val {redacted_runs}} are replaced by",
      "{.val {REDACTED}} in the trail."
    ))
  }

  # Save if path is provided

if (!is.null(path)) {
    rds_file <- paste0(path, ".rds")
    qmd_file <- paste0(path, ".qmd")

    # Save RDS
    saveRDS(trail, rds_file)
    cli::cli_alert_success("Trail saved to {.path {rds_file}}")

    # Generate report
    generate_trail_report(trail, qmd_file)
    cli::cli_alert_success("Report saved to {.path {qmd_file}}")
    cli::cli_alert_info("Render with {.code quarto::quarto_render(\"{qmd_file}\")}")
  }

  invisible(trail)
}


#' Print a quallmer trail
#'
#' @param x A qlm_trail object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects (printing to console).
#' @keywords internal
#' @export
print.qlm_trail <- function(x, ...) {
  n_runs <- length(x$runs)

  if (n_runs == 0) {
    cat("Empty trail\n")
    return(invisible(x))
  }

  # Header
  if (n_runs == 1) {
    cat("# quallmer audit trail\n")
    run <- x$runs[[1]]
    cat("Run:     ", run$name, "\n", sep = "")
    if (!is.null(run$parent)) {
      if (length(run$parent) == 1) {
        cat("Parent:  ", run$parent, "\n", sep = "")
      } else {
        cat("Parents: ", paste(run$parent, collapse = ", "), "\n", sep = "")
      }
    }
    if (!is.null(run$metadata$timestamp)) {
      cat("Created: ", format(run$metadata$timestamp, '%Y-%m-%d %H:%M:%S'), "\n", sep = "")
    }
    if (!is.null(run$chat_args$name)) {
      cat("Model:   ", run$chat_args$name, "\n", sep = "")
    }
    if (!is.null(run$provider_resolution)) {
      cat("Requested model: ", provider_request_label(run$provider_resolution), "\n", sep = "")
    }
    if (!is.null(backfill_summary(run$backfill))) {
      cat("Backfill:", backfill_summary(run$backfill), "\n")
    }
    if (!is.null(run$metadata$notes)) {
      cat("Notes:   ", run$metadata$notes, "\n", sep = "")
    }

    # Show comparison info if available
    if (!is.null(run$comparison)) {
      comp <- run$comparison
      levels <- unique(comp$level)
      level_str <- if (length(levels)) paste(levels, collapse = "/") else "unknown"
      cat("\nComparison (", level_str, " level):\n", sep = "")
      cat("  Subjects: ", attr(comp, "n") %||% "?", "\n", sep = "")
      cat("  Raters:   ", attr(comp, "raters") %||% "?", "\n", sep = "")
    }

    # Show validation info if available
    if (!is.null(run$validation)) {
      val <- run$validation
      levels <- unique(val$level)
      level_str <- if (length(levels)) paste(levels, collapse = "/") else "unknown"
      cat("\nValidation (", level_str, " level):\n", sep = "")
      cat("  N:        ", attr(val, "n") %||% "?", "\n", sep = "")
      if (!is.null(run$metadata$average)) {
        cat("  Average:  ", run$metadata$average, "\n", sep = "")
      }
    }

    cat("\n")
    if (!x$complete) {
      cat("To see full chain, provide ancestor objects.\n")
    }
  } else {
    runs_text <- if (n_runs == 1) "run" else "runs"
    cat("# quallmer audit trail (", n_runs, " ", runs_text, ")\n\n", sep = "")

    for (i in seq_along(x$runs)) {
      run <- x$runs[[i]]

      ts <- if (!is.null(run$metadata$timestamp)) {
        format(run$metadata$timestamp, "%Y-%m-%d %H:%M")
      } else {
        "unknown"
      }

      model <- if (!is.null(run$chat_args$name)) {
        run$chat_args$name
      } else {
        "unknown"
      }

      parent_str <- if (!is.null(run$parent)) {
        if (length(run$parent) == 1) {
          paste0(" (parent: ", run$parent, ")")
        } else {
          paste0(" (parents: ", paste(run$parent, collapse = ", "), ")")
        }
      } else {
        " (original)"
      }

      cat(i, ". ", run$name, parent_str, "\n", sep = "")
      cat("   ", ts, " | ", model, "\n", sep = "")
      if (!is.null(backfill_summary(run$backfill))) {
        cat("   Backfill: ", backfill_summary(run$backfill), "\n", sep = "")
      }

      if (!is.null(run$codebook$name)) {
        cat("   Codebook: ", run$codebook$name, "\n", sep = "")
      }

      if (!is.null(run$metadata$notes)) {
        cat("   Notes: ", run$metadata$notes, "\n", sep = "")
      }

      if (!is.null(run$comparison)) {
        comp <- run$comparison
        levels <- unique(comp$level)
        level_str <- if (length(levels)) paste(levels, collapse = "/") else "unknown"
        cat("   Comparison: ", level_str, " level | ",
            attr(comp, "n") %||% "?", " subjects | ",
            attr(comp, "raters") %||% "?", " raters\n", sep = "")
      }

      if (!is.null(run$validation)) {
        val <- run$validation
        levels <- unique(val$level)
        level_str <- if (length(levels)) paste(levels, collapse = "/") else "unknown"
        cat("   Validation: ", level_str, " level | ",
            "n=", attr(val, "n") %||% "?", sep = "")
        if (!is.null(run$metadata$average)) {
          cat(" | ", run$metadata$average, " avg", sep = "")
        }
        cat("\n")
      }

      if (i < length(x$runs)) {
        cat("\n")
      }
    }

    if (!x$complete) {
      cat("\n")
      cat("! Trail is incomplete. Some parent runs are missing.\n")
    }
  }

  invisible(x)
}


#' Generate audit trail report (internal)
#'
#' Creates a Quarto document with all audit trail components following
#' Lincoln and Guba's (1985) framework.
#'
#' @param trail A qlm_trail object.
#' @param file Path to save the .qmd file.
#'
#' @return Invisibly returns the file path.
#' @keywords internal
#' @noRd
generate_trail_report <- function(trail, file) {
  lines <- character()

  # YAML header
  lines <- c(lines, "---")
  lines <- c(lines, "title: \"quallmer audit trail\"")
  lines <- c(lines, paste0("date: \"", format(Sys.time(), "%Y-%m-%d"), "\""))
  lines <- c(lines, "format:")
  lines <- c(lines, "  html:")
  lines <- c(lines, "    toc: true")
  lines <- c(lines, "    toc-depth: 3")
  lines <- c(lines, "---")
  lines <- c(lines, "")

  # Introduction
  lines <- c(lines, "This audit trail documents the complete workflow following Lincoln and Guba's (1985)")
  lines <- c(lines, "framework for establishing trustworthiness in qualitative research.")
  lines <- c(lines, "")

  # Trail summary
  lines <- c(lines, "## Trail summary")
  lines <- c(lines, "")
  lines <- c(lines, paste("- **Number of runs:**", length(trail$runs)))
  lines <- c(lines, paste("- **Complete:**", if (trail$complete) "Yes" else "No (missing parent runs)"))
  lines <- c(lines, "")

  # Get metadata from most recent run
  if (length(trail$runs) > 0) {
    last_run <- trail$runs[[length(trail$runs)]]
    if (!is.null(last_run$metadata)) {
      lines <- c(lines, "### System information")
      lines <- c(lines, "")
      lines <- c(lines, paste("- **quallmer version:**", last_run$metadata$quallmer_version %||% "unknown"))
      lines <- c(lines, paste("- **ellmer version:**", last_run$metadata$ellmer_version %||% "unknown"))
      lines <- c(lines, paste("- **R version:**", last_run$metadata$R_version %||% "unknown"))
      lines <- c(lines, "")
    }
  }

  # Instrument development information (Codebooks)
  lines <- c(lines, "## Instrument development")
  lines <- c(lines, "")
  lines <- c(lines, "Codebooks used in this analysis:")
  lines <- c(lines, "")

  # One entry per distinct codebook, not per name: two runs may share a name
  # and differ in a setting, such as the resolution images were coded at, and
  # the second must not vanish behind the first (#177)
  codebooks_seen <- list()
  variants_of <- list()
  for (run in trail$runs) {
    if (!is.null(run$codebook) && !is.null(run$codebook$name)) {
      cb_name <- run$codebook$name
      cb_key <- digest::digest(fill_codebook_fields(run$codebook))
      if (is.null(codebooks_seen[[cb_key]])) {
        codebooks_seen[[cb_key]] <- run$codebook
        variants_of[[cb_name]] <- (variants_of[[cb_name]] %||% 0L) + 1L
        heading <- if (variants_of[[cb_name]] > 1L) {
          paste0(cb_name, " (variant ", variants_of[[cb_name]], ")")
        } else {
          cb_name
        }

        lines <- c(lines, paste0("### ", heading))
        lines <- c(lines, "")

        input_line <- codebook_input_line(run$codebook)
        if (!is.null(input_line)) {
          lines <- c(lines, input_line)
          lines <- c(lines, "")
        }

        if (!is.null(run$codebook$instructions)) {
          lines <- c(lines, "**Instructions:**")
          lines <- c(lines, "")
          lines <- c(lines, paste(">", run$codebook$instructions))
          lines <- c(lines, "")
        }

        if (!is.null(run$codebook$schema)) {
          lines <- c(lines, "**Schema:**")
          lines <- c(lines, "")
          lines <- c(lines, "```")
          schema_str <- utils::capture.output(print(run$codebook$schema))
          lines <- c(lines, schema_str)
          lines <- c(lines, "```")
          lines <- c(lines, "")
        }
      }
    }
  }

  # Process notes (Timeline with all details)
  lines <- c(lines, "## Process notes")
  lines <- c(lines, "")
  lines <- c(lines, "Chronological record of all coding runs:")
  lines <- c(lines, "")

  for (i in seq_along(trail$runs)) {
    run <- trail$runs[[i]]

    lines <- c(lines, paste0("### ", i, ". ", run$name))
    lines <- c(lines, "")

    # Parent relationship
    if (!is.null(run$parent)) {
      if (length(run$parent) == 1) {
        lines <- c(lines, paste("**Parent:**", run$parent))
      } else {
        lines <- c(lines, paste("**Parents:**", paste(run$parent, collapse = ", ")))
      }
    } else {
      lines <- c(lines, "**Parent:** None (original run)")
    }

    # Timestamp
    if (!is.null(run$metadata$timestamp)) {
      lines <- c(lines, paste("**Timestamp:**", format(run$metadata$timestamp, "%Y-%m-%d %H:%M:%S")))
    }

    # Model and parameters
    if (!is.null(run$chat_args$name)) {
      lines <- c(lines, paste("**Model:**", run$chat_args$name))
    }
    if (!is.null(run$provider_resolution)) {
      lines <- c(lines, paste("**Requested model:**",
                              provider_request_label(run$provider_resolution)))
    }
    if (!is.null(backfill_summary(run$backfill))) {
      lines <- c(lines, paste("**Backfill:**", backfill_summary(run$backfill)))
    }

    params <- run_params(run)
    if (length(params) > 0) {
      lines <- c(lines, paste(
        "**Parameters:**",
        paste(Map(format_param, names(params), params), collapse = ", ")
      ))
    }

    # Where the cost column came from, when ellmer could not fill it: a
    # figure resting on entered rates must say so in the artefact (#135)
    if (!is.null(run$cost_note)) {
      lines <- c(lines, paste("**Cost:**", run$cost_note))
    }
    pass_notes <- backfill_cost_notes(run$backfill, run$cost_note)
    for (i in seq_along(pass_notes)) {
      lines <- c(lines, paste0("**Cost (", names(pass_notes)[i], "):** ", pass_notes[[i]]))
    }

    # Codebook reference, with the image settings this run coded at: they
    # are per run, since a codebook of the same name may differ between runs
    if (!is.null(run$codebook$name)) {
      lines <- c(lines, paste("**Codebook:**", run$codebook$name))
      input_line <- codebook_input_line(run$codebook)
      if (!is.null(input_line)) {
        lines <- c(lines, input_line)
      }
    }

    # Units coded
    if (!is.null(run$metadata$n_units)) {
      lines <- c(lines, paste("**Units coded:**", run$metadata$n_units))
    }

    # Batch processing
    if (!is.null(run$batch) && run$batch) {
      lines <- c(lines, "**Processing:** Batch")
    }

    for (k in seq_along(run$backfill)) {
      resolution <- run$backfill[[k]]$provider_resolution
      if (!is.null(resolution)) {
        lines <- c(lines, paste0("**Backfill pass ", k, " requested model:** ",
                                provider_request_label(resolution)))
      }
    }

    lines <- c(lines, "")

    # The files behind each unit, with the hash recorded at coding time
    if (is.data.frame(run$input_files) && nrow(run$input_files)) {
      lines <- c(lines, paste0(
        "**Input files (", run$input_type %||% "file", "):** ",
        nrow(run$input_files), " file", if (nrow(run$input_files) == 1L) "" else "s",
        ", SHA-256 recorded at coding time"
      ))
      lines <- c(lines, "")
      lines <- c(lines, "| .id | File | Bytes | SHA-256 |")
      lines <- c(lines, "|---|---|---|---|")
      for (k in seq_len(nrow(run$input_files))) {
        f <- run$input_files[k, ]
        lines <- c(lines, paste0(
          "| ", f$.id, " | ", f$file, " | ",
          if (is.na(f$size)) "" else format(f$size, big.mark = ",", scientific = FALSE),
          " | ",
          # A hash is recorded for anything that passed through the coding
          # machine, a downloaded URL included; a URL without one was
          # fetched by the provider (#177, #179)
          if (!is.na(f$sha256)) paste0("`", f$sha256, "`")
          else if (is_input_url(f$file, data = TRUE)) "fetched by the provider"
          else "not recorded",
          " |"
        ))
      }
      lines <- c(lines, "")
    }

    # Call (materials relating to intentions)
    if (!is.null(run$call)) {
      lines <- c(lines, "**Call:**")
      lines <- c(lines, "")
      lines <- c(lines, "```{r eval=FALSE}")
      lines <- c(lines, deparse(run$call))
      lines <- c(lines, "```")
      lines <- c(lines, "")
    }
  }

  # Data reconstruction products (Comparisons and Validations)
  comparisons <- list()
  validations <- list()

  for (run in trail$runs) {
    if (!is.null(run$comparison)) {
      comparisons[[length(comparisons) + 1]] <- list(
        name = run$name, comparison = run$comparison, parent = run$parent
      )
    }
    if (!is.null(run$validation)) {
      validations[[length(validations) + 1]] <- list(
        name = run$name, validation = run$validation, parent = run$parent
      )
    }
  }

  if (length(comparisons) > 0 || length(validations) > 0) {
    lines <- c(lines, "## Data reconstruction")
    lines <- c(lines, "")
    lines <- c(lines, "Assessment of coding quality and reliability:")
    lines <- c(lines, "")
  }

  if (length(comparisons) > 0) {
    lines <- c(lines, "### Comparisons")
    lines <- c(lines, "")

    for (entry in comparisons) {
      lines <- c(lines, paste0("#### ", entry$name))
      lines <- c(lines, "")

      if (!is.null(entry$parent)) {
        lines <- c(lines, paste("**Compared runs:**", paste(entry$parent, collapse = ", ")))
      }

      comp <- entry$comparison
      levels <- unique(comp$level)
      level_str <- if (length(levels)) paste(levels, collapse = ", ") else "unknown"
      lines <- c(lines, paste("**Level:**", level_str))
      lines <- c(lines, paste("**Subjects:**", attr(comp, "n") %||% "?"))
      lines <- c(lines, paste("**Raters:**", attr(comp, "raters") %||% "?"))
      lines <- c(lines, "")

      lines <- c(lines, "**Measures:**")
      lines <- c(lines, "")
      lines <- c(lines, format_metric_rows(comp, format_measure_name))
      lines <- c(lines, "")
    }
  }

  if (length(validations) > 0) {
    lines <- c(lines, "### Validations")
    lines <- c(lines, "")

    for (entry in validations) {
      lines <- c(lines, paste0("#### ", entry$name))
      lines <- c(lines, "")

      if (!is.null(entry$parent)) {
        lines <- c(lines, paste("**Validated run:**", paste(entry$parent, collapse = ", ")))
      }

      val <- entry$validation
      levels <- unique(val$level)
      level_str <- if (length(levels)) paste(levels, collapse = ", ") else "unknown"
      lines <- c(lines, paste("**Level:**", level_str))
      lines <- c(lines, paste("**N:**", attr(val, "n") %||% "?"))
      meta_v <- attr(val, "meta")
      if (!is.null(meta_v$object$average)) {
        lines <- c(lines, paste("**Averaging:**", meta_v$object$average))
      }
      lines <- c(lines, "")

      lines <- c(lines, "**Metrics:**")
      lines <- c(lines, "")
      lines <- c(lines, format_metric_rows(val, format_validation_measure_name,
                                            extra_label_col = "class"))
      lines <- c(lines, "")
    }
  }

  # Raw data and data reduction products (coded results)
  lines <- c(lines, "## Raw data and coded results")
  lines <- c(lines, "")
  lines <- c(lines, "The complete coded data for each run is stored in the RDS file.")
  lines <- c(lines, "Load with `readRDS()` to access:")
  lines <- c(lines, "")
  lines <- c(lines, "```r")
  lines <- c(lines, "trail <- readRDS(\"path/to/trail.rds\")")
  lines <- c(lines, "trail$runs$run_name$data")
  lines <- c(lines, "# Coded results for a specific run")
  lines <- c(lines, "```")
  lines <- c(lines, "")

  # Summary of data in each run
  for (run in trail$runs) {
    if (!is.null(run$coded)) {
      lines <- c(lines, paste0("**", run$name, ":** ", nrow(run$coded), " units, ",
                               ncol(run$coded) - 1, " variables"))
    }
  }
  lines <- c(lines, "")

  # Replication instructions
  lines <- c(lines, "## Replication")
  lines <- c(lines, "")
  lines <- c(lines, "This section provides instructions and code to replicate the analysis.")
  lines <- c(lines, "")

  # Get RDS filename from the qmd path
 rds_filename <- sub("\\.qmd$", ".rds", basename(file))

  # Environment setup
  lines <- c(lines, "### Environment setup")
  lines <- c(lines, "")
  lines <- c(lines, "Install required packages:")
  lines <- c(lines, "")
  lines <- c(lines, "```r")
  lines <- c(lines, "# Install quallmer (if not already installed)")
  lines <- c(lines, "# install.packages(\"pak\")")
  lines <- c(lines, "# pak::pak(\"quallmer/quallmer\")")
  lines <- c(lines, "")
  lines <- c(lines, "library(quallmer)")
  lines <- c(lines, "```")
  lines <- c(lines, "")

  # One entry per distinct endpoint actually used. Identity is the provider
  # prefix *and* the base_url, matching qlm_replicate(): everything ellmer has
  # no chat_*() for arrives as `openai_compatible`, so the prefix alone would
  # collapse unrelated services into one line.
  endpoints <- lapply(trail$runs, function(r) {
    if (is.null(r$chat_args$name)) {
      return(NULL)
    }
    identity <- endpoint_identity(r$chat_args$base_url)
    list(
      provider = model_provider(r$chat_args$name),
      identity = identity,
      key = paste0(model_provider(r$chat_args$name), "\r", identity)
    )
  })
  endpoints <- endpoints[!vapply(endpoints, is.null, logical(1))]
  endpoints <- endpoints[!duplicated(vapply(endpoints, function(e) e$key, character(1)))]

  if (length(endpoints) > 0) {
    known <- ellmer_providers()

    lines <- c(lines, "### Provider and endpoint setup")
    lines <- c(lines, "")
    lines <- c(lines, paste(
      "Credential requirements differ by provider, from an API key to platform",
      "IAM to none at all. Follow the corresponding ellmer help page to",
      "configure access."
    ))
    lines <- c(lines, "")
    lines <- c(lines, "```r")

    for (endpoint in endpoints) {
      label <- endpoint_label(endpoint$identity)
      heading <- if (is.na(label)) {
        paste0("# ", endpoint$provider)
      } else {
        paste0("# ", endpoint$provider, " at ", label)
      }
      lines <- c(lines, heading)

      if (endpoint$provider %in% known) {
        lines <- c(lines, paste0("#   see ?ellmer::chat_", endpoint$provider))
      } else {
        # Recorded before qlm_code() began rejecting prefixes ellmer cannot
        # dispatch on, so there is no help topic to send the reader to.
        lines <- c(lines, "#   this provider is not one the installed ellmer offers")
        lines <- c(lines, "#   see https://ellmer.tidyverse.org/reference/index.html")
      }

      # Ollama needs no key on a local endpoint, but ellmer reads
      # OLLAMA_API_KEY when one is served behind a proxy, so this is said only
      # where it is true.
      if (identical(endpoint$provider, "ollama") && is_local_endpoint(endpoint$identity)) {
        lines <- c(lines, "#   no API key for a local endpoint; ensure Ollama is running")
        lines <- c(lines, "#   https://ollama.com")
      }
    }

    lines <- c(lines, "```")
    lines <- c(lines, "")
  }

  # Loading the trail
  lines <- c(lines, "### Loading the trail")
  lines <- c(lines, "")
  lines <- c(lines, "Load the saved trail to access codebooks, data, and metadata:")
  lines <- c(lines, "")
  lines <- c(lines, "```r")
  lines <- c(lines, paste0("trail <- readRDS(\"", rds_filename, "\")"))
  lines <- c(lines, "```")
  lines <- c(lines, "")

  # Replication code for each run
  lines <- c(lines, "### Replication code")
  lines <- c(lines, "")
  lines <- c(lines, "The following code replicates each coding run using the stored parameters.")
  lines <- c(lines, "")

  # Find coding runs (those with codebooks, excluding comparisons/validations)
  coding_runs <- trail$runs[sapply(trail$runs, function(r) {
    !is.null(r$coded)
  })]

  if (length(coding_runs) > 0) {
    for (run in coding_runs) {
      lines <- c(lines, paste0("#### Replicate: ", run$name))
      lines <- c(lines, "")

      # Build replication code
      lines <- c(lines, "```r")

      # Extract codebook from trail
      lines <- c(lines, paste0("# Get codebook from trail"))
      lines <- c(lines, paste0("codebook_", run$name, " <- trail$runs$", run$name, "$codebook"))
      lines <- c(lines, "")

      # Get original data (user needs to provide this)
      lines <- c(lines, "# Load your input data (same structure as original)")
      lines <- c(lines, "# texts <- your_data$text_column")
      lines <- c(lines, "")

      # Build qlm_code call
      model <- run$chat_args$name %||% "openai/gpt-4o"
      params <- run_params(run)

      code_call <- paste0("coded_", run$name, " <- qlm_code(")
      code_call <- paste0(code_call, "\n  texts,")
      code_call <- paste0(code_call, "\n  codebook_", run$name, ",")
      code_call <- paste0(code_call, "\n  model = \"", model, "\"")

      # Sampling settings belong in `params`, not at the top level, where they
      # would reach ellmer::chat() and error. A generated script that cannot
      # run is worse than one that omits the settings.
      if (length(params) > 0) {
        code_call <- paste0(
          code_call, ",\n  params = ellmer::params(",
          paste(Map(format_param, names(params), params), collapse = ", "), ")"
        )
      }

      if (!is.null(run$batch) && run$batch) {
        code_call <- paste0(code_call, ",\n  batch = TRUE")
      }

      if (!is.null(run$prices)) {
        code_call <- paste0(
          code_call, ",\n  prices = c(",
          paste(Map(format_param, names(run$prices), run$prices), collapse = ", "), ")"
        )
      }

      code_call <- paste0(code_call, ",\n  name = \"", run$name, "\"")
      code_call <- paste0(code_call, "\n)")

      lines <- c(lines, code_call)
      lines <- c(lines, "```")
      lines <- c(lines, "")
    }
  }

  # Replication code for comparisons
  if (length(comparisons) > 0) {
    lines <- c(lines, "#### Replicate comparisons")
    lines <- c(lines, "")
    lines <- c(lines, "After replicating coding runs, compare results:")
    lines <- c(lines, "")
    lines <- c(lines, "```r")

    for (entry in comparisons) {
      comp <- entry$comparison
      if (!is.null(entry$parent) && length(entry$parent) >= 2) {
        parent_vars <- paste0("coded_", entry$parent, collapse = ", ")
        meta_c <- attr(comp, "meta")
        vars <- meta_c$object$variables %||% unique(comp$variable)
        by_arg <- if (length(vars) == 1L) vars else paste0("c(", paste(shQuote(vars), collapse = ", "), ")")
        levels <- unique(comp$level)
        lines <- c(lines, paste0("comparison <- qlm_compare(", parent_vars, ","))
        lines <- c(lines, paste0("  by = \"", by_arg, "\","))
        if (length(levels) == 1L) {
          lines <- c(lines, paste0("  level = \"", levels, "\""))
        } else {
          lines <- c(lines, "  level = NULL  # auto-detect from codebook")
        }
        lines <- c(lines, ")")
      }
    }

    lines <- c(lines, "```")
    lines <- c(lines, "")
  }

  # Replication code for validations
  if (length(validations) > 0) {
    lines <- c(lines, "#### Replicate validations")
    lines <- c(lines, "")
    lines <- c(lines, "After replicating coding runs, validate against gold standard:")
    lines <- c(lines, "")
    lines <- c(lines, "```r")

    for (entry in validations) {
      val <- entry$validation
      if (!is.null(entry$parent) && length(entry$parent) >= 1) {
        meta_v <- attr(val, "meta")
        vars <- meta_v$object$variables %||% unique(val$variable)
        by_arg <- if (length(vars) == 1L) vars else paste0("c(", paste(shQuote(vars), collapse = ", "), ")")
        levels <- unique(val$level)
        lines <- c(lines, paste0("validation <- qlm_validate(coded_", entry$parent[1], ","))
        lines <- c(lines, "  gold = gold_standard,  # Your gold standard data")
        lines <- c(lines, paste0("  by = \"", by_arg, "\","))
        if (length(levels) == 1L) {
          lines <- c(lines, paste0("  level = \"", levels, "\""))
        } else {
          lines <- c(lines, "  level = NULL  # auto-detect from codebook")
        }
        lines <- c(lines, ")")
      }
    }

    lines <- c(lines, "```")
    lines <- c(lines, "")
  }

  # Note about reproducibility
  lines <- c(lines, "### Note on reproducibility")
  lines <- c(lines, "")
  lines <- c(lines, "LLM outputs are inherently stochastic. To improve reproducibility:")
  lines <- c(lines, "")
  lines <- c(lines, "- Use `params = ellmer::params(temperature = 0)` for more deterministic outputs")
  lines <- c(lines, "- Set a random seed where supported by the provider")
  lines <- c(lines, "- Document the exact model version (models are updated over time)")
  lines <- c(lines, "- Compare results across multiple runs using `qlm_replicate()`")
  lines <- c(lines, "")

  # Reference
  lines <- c(lines, "## Reference")
  lines <- c(lines, "")
  lines <- c(lines, "Lincoln, Y. S., & Guba, E. G. (1985). *Naturalistic Inquiry*. Sage.")
  lines <- c(lines, "")

  # Write file
  writeLines(lines, file)

  invisible(file)
}

#' The image settings a codebook codes at, as one report line
#'
#' The resolution an image was coded at is part of the instrument; a
#' codebook saved before the fields existed was coded at "low" with the
#' provider choosing the URL detail (#177).
#'
#' @param codebook A codebook, as stored on a run.
#' @return A string, or `NULL` for a codebook that takes no images.
#' @keywords internal
#' @noRd
codebook_input_line <- function(codebook) {
  filled <- fill_codebook_fields(codebook)
  if (is.null(filled$image_file_resize)) {
    return(NULL)
  }
  paste0(
    "**Input:** image files, resized with `image_file_resize = \"",
    filled$image_file_resize, "\"`; image URLs with `image_url_detail = \"",
    filled$image_url_detail, "\"`"
  )
}


#' Format the rows of a comparison/validation tibble as markdown bullets
#'
#' Iterates the long-format `qlm_comparison` / `qlm_validation` tibble and
#' produces one bullet per row, grouped by `variable`. Honours optional
#' `docid`/`class` qualifiers and `ci_lower`/`ci_upper` columns when present.
#'
#' @param df A `qlm_comparison` or `qlm_validation` tibble.
#' @param label_fn Function mapping a measure name to its display label
#'   (`format_measure_name` or `format_validation_measure_name`).
#' @param extra_label_col Optional name of an extra column whose value is
#'   appended to the measure label in parentheses (e.g., `"class"` for
#'   validation per-class breakdowns).
#'
#' @return A character vector of markdown lines.
#' @keywords internal
#' @noRd
format_metric_rows <- function(df, label_fn, extra_label_col = NULL) {
  out <- character()
  if (is.null(df) || nrow(df) == 0L || !"variable" %in% names(df)) {
    return(out)
  }

  has_ci    <- all(c("ci_lower", "ci_upper") %in% names(df))
  has_docid <- "docid" %in% names(df) && !all(is.na(df$docid))
  has_extra <- !is.null(extra_label_col) &&
               extra_label_col %in% names(df) &&
               !all(is.na(df[[extra_label_col]]))

  variables <- unique(df$variable)
  for (i in seq_along(variables)) {
    var <- variables[i]
    rows <- df[df$variable == var, , drop = FALSE]
    if (length(variables) > 1L) {
      out <- c(out, paste0("*", var, ":*"))
    }
    for (j in seq_len(nrow(rows))) {
      label <- label_fn(rows$measure[j])
      if (has_extra && !is.na(rows[[extra_label_col]][j])) {
        label <- paste0(label, " (", rows[[extra_label_col]][j], ")")
      }
      if (has_docid && !is.na(rows$docid[j])) {
        label <- paste0(label, " [", rows$docid[j], "]")
      }
      line <- sprintf("- %s: %.4f", label, rows$value[j])
      if (has_ci && !is.na(rows$ci_lower[j])) {
        line <- sprintf("%s [%.4f, %.4f]", line, rows$ci_lower[j], rows$ci_upper[j])
      }
      out <- c(out, line)
    }
    if (i < length(variables)) out <- c(out, "")
  }
  out
}


#' Sampling parameters recorded for a run
#'
#' `qlm_code()` takes sampling settings as `params = ellmer::params(...)` and
#' stores them in `chat_args$params`. Older runs recorded a bare
#' `chat_args$temperature` instead, from a routing that was never implemented:
#' a top-level `temperature` reaches [ellmer::chat()], which has no such
#' argument, so the call errored. That shape is read here and folded into
#' `params` so an old trail still describes and reproduces itself in the form
#' that works, but it is never displayed or emitted. `params$temperature` is
#' canonical and wins when both are present.
#'
#' @param run One element of `trail$runs`.
#'
#' @return A named list of sampling parameters, possibly empty.
#' @keywords internal
#' @noRd
run_params <- function(run) {
  params <- run$chat_args$params %||% list()

  if (is.null(params$temperature) && !is.null(run$chat_args$temperature)) {
    params$temperature <- run$chat_args$temperature
  }

  params
}


#' Deparse one value onto a single line
#'
#' Values are serialised one at a time rather than through `unlist()`, which
#' would flatten a vector-valued parameter such as `stop = c("END", "STOP")`
#' into separate entries and lose the distinction between `0` and `"0"`. This
#' is `deparse1()`, which cannot be used directly because the package supports
#' R (>= 3.5.0) and `deparse1()` arrived in R 4.0.0.
#'
#' @param value Any value short enough to belong in a parameter list.
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
deparse_value <- function(value) {
  paste(deparse(value, width.cutoff = 500L), collapse = " ")
}


#' Format one named parameter as `name = value`
#'
#' Serves both the metadata readout and the generated replication call, so the
#' report describes exactly what the script will run.
#'
#' @param name Parameter name.
#' @param value Parameter value.
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
format_param <- function(name, value) {
  paste0(name, " = ", deparse_value(value))
}


#' Normalised endpoint identity for a run
#'
#' Two runs are against the same endpoint when the provider prefix *and* the
#' `base_url` match, which is the rule [qlm_replicate()] already applies: every
#' provider ellmer has no `chat_*()` for is reached as
#' `openai_compatible/<model>`, so the prefix alone cannot tell Qwen through
#' Alibaba from Kimi through Moonshot, nor one local server from another.
#'
#' The host alone is not enough either. `localhost:8000` and `localhost:1234`
#' are different servers, `/team-a/v1` and `/team-b/v1` on one gateway are
#' different deployments, and `http` and `https` are different endpoints. So
#' the whole URL is kept, minus any userinfo -- a credential embedded as
#' `https://user:token@host` is not part of the endpoint's identity and must
#' never be recorded as though it were.
#'
#' @param base_url A recorded `chat_args$base_url`, possibly `NULL` or `NA`.
#'
#' @return A single string, or `NA_character_` when no usable URL was recorded.
#' @keywords internal
#' @noRd
endpoint_identity <- function(base_url) {
  if (length(base_url) != 1L || !is.character(base_url) || is.na(base_url) ||
      !nzchar(base_url)) {
    return(NA_character_)
  }

  url <- sub("^(\\w+://)[^/@]*@", "\\1", base_url)  # drop userinfo
  sub("/+$", "", url)
}


#' Endpoint label for the setup section
#'
#' The identity minus query and fragment. A credential is as likely to arrive
#' as `?api_key=` as in userinfo, and a trail is written to be handed to
#' someone else, so neither is printed here.
#'
#' This is display only. The recorded call and `chat_args` are redacted when
#' the trail is built, by `redact_meta()`, so the Call section and the `.rds`
#' are covered there; this keeps the query out of a heading where it would
#' only be noise.
#'
#' Known limitation: an endpoint distinguished only by a query parameter --
#' Azure's `api-version=`, say -- prints the same label as its sibling, though
#' the two remain distinct identities and so appear as separate entries.
#'
#' @param identity A value from `endpoint_identity()`.
#'
#' @return A single string, or `NA_character_`.
#' @keywords internal
#' @noRd
endpoint_label <- function(identity) {
  if (is.na(identity)) {
    return(NA_character_)
  }

  sub("[?#].*$", "", identity)
}


#' Is this endpoint on the machine running the report?
#'
#' Used only to decide whether "no API key needed" is safe to say, so an
#' unknown endpoint must not count as local. An absent `base_url` is unknown
#' rather than default: ellmer resolves Ollama's to
#' `Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")`, so a run made
#' against a remote `OLLAMA_BASE_URL` records no URL, and calling that local
#' would reproduce the stale guidance this replaced.
#'
#' @param identity A value from `endpoint_identity()`.
#'
#' @return `TRUE` only when the endpoint is recorded and loopback.
#' @keywords internal
#' @noRd
is_local_endpoint <- function(identity) {
  if (is.na(identity)) {
    return(FALSE)
  }

  host <- sub("^\\w+://", "", identity)
  host <- sub("[/?#].*$", "", host)

  # Brackets and port together: `[::1]` and `[::1]:11434` both have to reduce
  # to `::1`. Unbracketing first and stripping the port after would take the
  # trailing `:1` off `::1` and leave a bare colon.
  host <- if (grepl("^\\[", host)) {
    sub("^\\[([^]]*)\\].*$", "\\1", host)
  } else {
    sub(":\\d+$", "", host)
  }

  host %in% c("localhost", "127.0.0.1", "::1", "0.0.0.0")
}
