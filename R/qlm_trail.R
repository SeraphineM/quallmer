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

    # Add object-specific metadata
    if (inherits(obj, "qlm_coded")) {
      run$batch <- meta_attr$object$batch
      run$chat_args <- meta_attr$object$chat_args
      run$execution_args <- meta_attr$object$execution_args
      run$codebook <- attr(obj, "codebook")
      run$metadata$n_units <- meta_attr$object$n_units
      run$metadata$ellmer_version <- meta_attr$system$ellmer_version
    } else if (inherits(obj, "qlm_comparison")) {
      run$metadata$n_raters <- meta_attr$object$n_raters
      run$metadata$variables <- meta_attr$object$variables
    } else if (inherits(obj, "qlm_validation")) {
      run$metadata$variables <- meta_attr$object$variables
      run$metadata$average <- meta_attr$object$average
    }

    # Store comparison/validation data if this is a comparison or validation object
    if (inherits(obj, "qlm_comparison")) {
      run$comparison_data <- list(
        level = obj$level,
        subjects = obj$subjects,
        raters = obj$raters,
        alpha_nominal = obj$alpha_nominal,
        kappa = obj$kappa,
        kappa_type = obj$kappa_type,
        alpha_ordinal = obj$alpha_ordinal,
        kappa_weighted = obj$kappa_weighted,
        w = obj$w,
        rho = obj$rho,
        alpha_interval = obj$alpha_interval,
        icc = obj$icc,
        r = obj$r,
        percent_agreement = obj$percent_agreement
      )
    } else if (inherits(obj, "qlm_validation")) {
      run$validation_data <- list(
        level = obj$level,
        n = obj$n,
        classes = obj$classes,
        average = obj$average,
        accuracy = obj$accuracy,
        precision = obj$precision,
        recall = obj$recall,
        f1 = obj$f1,
        kappa = obj$kappa,
        rho = obj$rho,
        tau = obj$tau,
        r = obj$r,
        icc = obj$icc,
        mae = obj$mae,
        rmse = obj$rmse
      )
    }

    # Always store coded data for complete audit trail
    if (inherits(obj, "qlm_coded")) {
      run$data <- as.data.frame(obj)
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
    if (!is.null(run$metadata$notes)) {
      cat("Notes:   ", run$metadata$notes, "\n", sep = "")
    }

    # Show comparison info if available
    if (!is.null(run$comparison_data)) {
      comp <- run$comparison_data
      cat("\nComparison (", comp$level %||% "unknown", " level):\n", sep = "")
      cat("  Subjects: ", comp$subjects %||% "?", "\n", sep = "")
      cat("  Raters:   ", comp$raters %||% "?", "\n", sep = "")
    }

    # Show validation info if available
    if (!is.null(run$validation_data)) {
      val <- run$validation_data
      cat("\nValidation (", val$level %||% "unknown", " level):\n", sep = "")
      cat("  N:        ", val$n %||% "?", "\n", sep = "")
      if (!is.null(val$average)) {
        cat("  Average:  ", val$average, "\n", sep = "")
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

      if (!is.null(run$codebook$name)) {
        cat("   Codebook: ", run$codebook$name, "\n", sep = "")
      }

      if (!is.null(run$metadata$notes)) {
        cat("   Notes: ", run$metadata$notes, "\n", sep = "")
      }

      if (!is.null(run$comparison_data)) {
        comp <- run$comparison_data
        cat("   Comparison: ", comp$level %||% "unknown", " level | ",
            comp$subjects %||% "?", " subjects | ",
            comp$raters %||% "?", " raters\n", sep = "")
      }

      if (!is.null(run$validation_data)) {
        val <- run$validation_data
        cat("   Validation: ", val$level %||% "unknown", " level | ",
            "n=", val$n %||% "?", sep = "")
        if (!is.null(val$average)) {
          cat(" | ", val$average, " avg", sep = "")
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

  codebooks_seen <- list()
  for (run in trail$runs) {
    if (!is.null(run$codebook) && !is.null(run$codebook$name)) {
      cb_name <- run$codebook$name
      if (is.null(codebooks_seen[[cb_name]])) {
        codebooks_seen[[cb_name]] <- run$codebook

        lines <- c(lines, paste0("### ", cb_name))
        lines <- c(lines, "")

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

    if (!is.null(run$chat_args$temperature)) {
      lines <- c(lines, paste("**Temperature:**", run$chat_args$temperature))
    }

    # Codebook reference
    if (!is.null(run$codebook$name)) {
      lines <- c(lines, paste("**Codebook:**", run$codebook$name))
    }

    # Units coded
    if (!is.null(run$metadata$n_units)) {
      lines <- c(lines, paste("**Units coded:**", run$metadata$n_units))
    }

    # Batch processing
    if (!is.null(run$batch) && run$batch) {
      lines <- c(lines, "**Processing:** Batch")
    }

    lines <- c(lines, "")

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
    if (!is.null(run$comparison_data)) {
      comparisons[[length(comparisons) + 1]] <- list(name = run$name, data = run$comparison_data, parent = run$parent)
    }
    if (!is.null(run$validation_data)) {
      validations[[length(validations) + 1]] <- list(name = run$name, data = run$validation_data, parent = run$parent)
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

    for (comp in comparisons) {
      lines <- c(lines, paste0("#### ", comp$name))
      lines <- c(lines, "")

      if (!is.null(comp$parent)) {
        lines <- c(lines, paste("**Compared runs:**", paste(comp$parent, collapse = ", ")))
      }

      cd <- comp$data
      lines <- c(lines, paste("**Level:**", cd$level %||% "unknown"))
      lines <- c(lines, paste("**Subjects:**", cd$subjects %||% "?"))
      lines <- c(lines, paste("**Raters:**", cd$raters %||% "?"))
      lines <- c(lines, "")

      # Display relevant measures
      lines <- c(lines, "**Measures:**")
      lines <- c(lines, "")

      if (!is.null(cd$level)) {
        if (cd$level == "nominal") {
          if (!is.null(cd$alpha_nominal)) lines <- c(lines, sprintf("- Krippendorff's alpha: %.4f", cd$alpha_nominal))
          if (!is.null(cd$kappa)) lines <- c(lines, sprintf("- %s kappa: %.4f", cd$kappa_type %||% "Cohen's", cd$kappa))
        } else if (cd$level == "ordinal") {
          if (!is.null(cd$alpha_ordinal)) lines <- c(lines, sprintf("- Krippendorff's alpha (ordinal): %.4f", cd$alpha_ordinal))
          if (!is.null(cd$kappa_weighted)) lines <- c(lines, sprintf("- Weighted kappa: %.4f", cd$kappa_weighted))
          if (!is.null(cd$rho)) lines <- c(lines, sprintf("- Spearman's rho: %.4f", cd$rho))
        } else if (cd$level %in% c("interval", "ratio")) {
          if (!is.null(cd$alpha_interval)) lines <- c(lines, sprintf("- Krippendorff's alpha (interval): %.4f", cd$alpha_interval))
          if (!is.null(cd$icc)) lines <- c(lines, sprintf("- ICC: %.4f", cd$icc))
          if (!is.null(cd$r)) lines <- c(lines, sprintf("- Pearson's r: %.4f", cd$r))
        }
        if (!is.null(cd$percent_agreement)) lines <- c(lines, sprintf("- Percent agreement: %.2f%%", cd$percent_agreement * 100))
      }
      lines <- c(lines, "")
    }
  }

  if (length(validations) > 0) {
    lines <- c(lines, "### Validations")
    lines <- c(lines, "")

    for (val in validations) {
      lines <- c(lines, paste0("#### ", val$name))
      lines <- c(lines, "")

      if (!is.null(val$parent)) {
        lines <- c(lines, paste("**Validated run:**", paste(val$parent, collapse = ", ")))
      }

      vd <- val$data
      lines <- c(lines, paste("**Level:**", vd$level %||% "unknown"))
      lines <- c(lines, paste("**N:**", vd$n %||% "?"))
      if (!is.null(vd$average)) {
        lines <- c(lines, paste("**Averaging:**", vd$average))
      }
      lines <- c(lines, "")

      lines <- c(lines, "**Metrics:**")
      lines <- c(lines, "")

      if (!is.null(vd$level)) {
        if (vd$level == "nominal") {
          if (!is.null(vd$accuracy)) lines <- c(lines, sprintf("- Accuracy: %.4f", vd$accuracy))
          if (!is.null(vd$precision)) lines <- c(lines, sprintf("- Precision: %.4f", vd$precision))
          if (!is.null(vd$recall)) lines <- c(lines, sprintf("- Recall: %.4f", vd$recall))
          if (!is.null(vd$f1)) lines <- c(lines, sprintf("- F1-score: %.4f", vd$f1))
          if (!is.null(vd$kappa)) lines <- c(lines, sprintf("- Cohen's kappa: %.4f", vd$kappa))
        } else if (vd$level == "ordinal") {
          if (!is.null(vd$rho)) lines <- c(lines, sprintf("- Spearman's rho: %.4f", vd$rho))
          if (!is.null(vd$tau)) lines <- c(lines, sprintf("- Kendall's tau: %.4f", vd$tau))
          if (!is.null(vd$mae)) lines <- c(lines, sprintf("- MAE: %.4f", vd$mae))
        } else if (vd$level == "interval") {
          if (!is.null(vd$r)) lines <- c(lines, sprintf("- Pearson's r: %.4f", vd$r))
          if (!is.null(vd$icc)) lines <- c(lines, sprintf("- ICC: %.4f", vd$icc))
          if (!is.null(vd$mae)) lines <- c(lines, sprintf("- MAE: %.4f", vd$mae))
          if (!is.null(vd$rmse)) lines <- c(lines, sprintf("- RMSE: %.4f", vd$rmse))
        }
      }
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
    if (!is.null(run$data)) {
      lines <- c(lines, paste0("**", run$name, ":** ", nrow(run$data), " units, ",
                               ncol(run$data) - 1, " variables"))
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

  # Collect unique providers from runs
  providers <- unique(unlist(lapply(trail$runs, function(r) {
    if (!is.null(r$chat_args$name)) {
      strsplit(r$chat_args$name, "/")[[1]][1]
    }
  })))
  providers <- providers[!is.na(providers)]

  if (length(providers) > 0) {
    lines <- c(lines, "Configure API credentials for the models used:")
    lines <- c(lines, "")
    lines <- c(lines, "```r")
    for (provider in providers) {
      if (provider == "openai") {
        lines <- c(lines, "# OpenAI: Set OPENAI_API_KEY environment variable")
        lines <- c(lines, "# Sys.setenv(OPENAI_API_KEY = \"your-api-key\")")
      } else if (provider == "anthropic") {
        lines <- c(lines, "# Anthropic: Set ANTHROPIC_API_KEY environment variable")
        lines <- c(lines, "# Sys.setenv(ANTHROPIC_API_KEY = \"your-api-key\")")
      } else if (provider == "google") {
        lines <- c(lines, "# Google: Set GOOGLE_API_KEY environment variable")
        lines <- c(lines, "# Sys.setenv(GOOGLE_API_KEY = \"your-api-key\")")
      } else if (provider == "ollama") {
        lines <- c(lines, "# Ollama: Ensure Ollama is running locally")
        lines <- c(lines, "# See: https://ollama.ai/")
      } else {
        lines <- c(lines, paste0("# ", provider, ": Configure API credentials as needed"))
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
    !is.null(r$codebook) && is.null(r$comparison_data) && is.null(r$validation_data)
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
      temp <- run$chat_args$temperature

      code_call <- paste0("coded_", run$name, " <- qlm_code(")
      code_call <- paste0(code_call, "\n  texts,")
      code_call <- paste0(code_call, "\n  codebook_", run$name, ",")
      code_call <- paste0(code_call, "\n  model = \"", model, "\"")

      if (!is.null(temp)) {
        code_call <- paste0(code_call, ",\n  temperature = ", temp)
      }

      if (!is.null(run$batch) && run$batch) {
        code_call <- paste0(code_call, ",\n  batch = TRUE")
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

    for (comp in comparisons) {
      cd <- comp$data
      if (!is.null(comp$parent) && length(comp$parent) >= 2) {
        parent_vars <- paste0("coded_", comp$parent, collapse = ", ")
        lines <- c(lines, paste0("comparison <- qlm_compare(", parent_vars, ","))
        lines <- c(lines, paste0("  by = variable_name,  # Replace with actual variable"))
        lines <- c(lines, paste0("  level = \"", cd$level %||% "nominal", "\""))
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

    for (val in validations) {
      vd <- val$data
      if (!is.null(val$parent) && length(val$parent) >= 1) {
        lines <- c(lines, paste0("validation <- qlm_validate(coded_", val$parent[1], ","))
        lines <- c(lines, "  gold = gold_standard,  # Your gold standard data")
        lines <- c(lines, "  by = variable_name,    # Replace with actual variable")
        lines <- c(lines, paste0("  level = \"", vd$level %||% "nominal", "\""))
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
  lines <- c(lines, "- Use `temperature = 0` for more deterministic outputs")
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
