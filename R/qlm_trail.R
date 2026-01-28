#' Extract audit trail from quallmer objects
#'
#' Creates a complete audit trail documenting your qualitative coding workflow.
#' Following Lincoln and Guba's (1985) concept of the audit trail for
#' establishing trustworthiness in qualitative research, this function captures
#' the full decision history of your AI-assisted coding process.
#'
#' @param ... One or more quallmer objects (`qlm_coded`, `qlm_comparison`, or
#'   `qlm_validation`). When multiple objects are provided, they will be used
#'   to reconstruct the complete workflow chain.
#'
#' @return A `qlm_trail` object containing:
#'
#'   \describe{
#'     \item{runs}{List of run information with coded data, ordered from oldest to newest}
#'     \item{complete}{Logical indicating whether all parent references were resolved}
#'   }
#'
#' @details
#' The audit trail captures the complete history of your coding workflow:
#' - Run names and parent-child relationships
#' - Models and parameters used at each step
#' - Timestamps documenting when each step occurred
#' - The actual coded results from each run
#' - Comparison and validation metrics (when applicable)
#'
#' This supports the confirmability and dependability criteria described by
#' Lincoln and Guba, allowing others to trace the logic of your analytical
#' decisions and verify the consistency of your coding process.
#'
#' When a single object is provided, only its immediate information is shown.
#' To see the full chain, provide all ancestor objects.
#'
#' For branching workflows (e.g., when multiple coded objects are compared),
#' the trail captures all input runs as parents of the comparison.
#'
#' @references
#' Lincoln, Y. S., & Guba, E. G. (1985). *Naturalistic Inquiry*. Sage.
#'
#' @examples
#' \dontrun{
#' # Code movie reviews with sentiment codebook
#'
#' library(quanteda)
#'
#' test_corpus <- data_corpus_LMRDsample %>%
#'   corpus_sample(size = 10, by = polarity)
#'
#' coded1 <- qlm_code(
#'   test_corpus,
#'   data_codebook_sentiment,
#'   model = "openai/gpt-4o",
#'   name = "gpt4o_run"
#' )
#'
#' # Replicate with different model
#' coded2 <- qlm_replicate(coded1, model = "openai/gpt-4o-mini", name = "mini_run")
#'
#' # Extract and view the audit trail
#' trail <- qlm_trail(coded1, coded2)
#' print(trail)
#'
#' # Use helper functions for saving/exporting
#' qlm_trail_save(trail, "trail.rds")
#' qlm_trail_export(trail, "trail.json")
#' qlm_trail_report(trail, "trail.qmd")
#'
#' # Or use qlm_archive() for one-call documentation
#' qlm_archive(coded1, coded2, path = "workflow")
#' }
#'
#' @seealso [qlm_replicate()], [qlm_code()], [qlm_compare()], [qlm_validate()],
#'   [qlm_trail_save()], [qlm_trail_export()], [qlm_trail_report()], [qlm_archive()]
#' @export
qlm_trail <- function(...) {
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

    run <- attr(obj, "run")
    if (is.null(run)) {
      cli::cli_abort(c(
        "Object {i} does not have a {.field run} attribute.",
        "i" = "This object may have been created with an older version of quallmer."
      ))
    }

    # Store comparison/validation data if this is a comparison or validation object
    if (inherits(obj, "qlm_comparison")) {
      # Extract all measures from the comparison object
      # Different measures exist depending on the level (nominal/ordinal/interval/ratio)
      run$comparison_data <- list(
        level = obj$level,
        subjects = obj$subjects,
        raters = obj$raters,
        # Nominal measures
        alpha_nominal = obj$alpha_nominal,
        kappa = obj$kappa,
        kappa_type = obj$kappa_type,
        # Ordinal measures
        alpha_ordinal = obj$alpha_ordinal,
        kappa_weighted = obj$kappa_weighted,
        w = obj$w,
        rho = obj$rho,
        # Interval/ratio measures
        alpha_interval = obj$alpha_interval,
        icc = obj$icc,
        r = obj$r,
        # Shared across all levels
        percent_agreement = obj$percent_agreement
      )
    } else if (inherits(obj, "qlm_validation")) {
      # Extract all validation metrics across all measurement levels
      run$validation_data <- list(
        level = obj$level,
        n = obj$n,
        classes = obj$classes,
        average = obj$average,
        # Nominal metrics
        accuracy = obj$accuracy,
        precision = obj$precision,
        recall = obj$recall,
        f1 = obj$f1,
        kappa = obj$kappa,
        # Ordinal metrics
        rho = obj$rho,
        tau = obj$tau,
        # Interval metrics
        r = obj$r,
        icc = obj$icc,
        # Shared metrics (ordinal/interval)
        mae = obj$mae,
        rmse = obj$rmse
      )
    }

    # Always store coded data for complete audit trail
    if (inherits(obj, "qlm_coded")) {
      run$data <- as.data.frame(obj)
    }
    # For comparisons and validations, we already stored the relevant summary data
    # The actual underlying coded data would be in their parent runs

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
  # Start with runs that have no parent or parent not in set
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

  structure(
    list(
      runs = chain,
      complete = complete
    ),
    class = "qlm_trail"
  )
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
    # Plural handling
    runs_text <- if (n_runs == 1) "run" else "runs"
    cat("# quallmer audit trail (", n_runs, " ", runs_text, ")\n\n", sep = "")

    for (i in seq_along(x$runs)) {
      run <- x$runs[[i]]

      # Format timestamp
      ts <- if (!is.null(run$metadata$timestamp)) {
        format(run$metadata$timestamp, "%Y-%m-%d %H:%M")
      } else {
        "unknown"
      }

      # Format model
      model <- if (!is.null(run$chat_args$name)) {
        run$chat_args$name
      } else {
        "unknown"
      }

      # Format parent
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

      # Show codebook name if available
      if (!is.null(run$codebook$name)) {
        cat("   Codebook: ", run$codebook$name, "\n", sep = "")
      }

      # Show comparison summary if available
      if (!is.null(run$comparison_data)) {
        comp <- run$comparison_data
        cat("   Comparison: ", comp$level %||% "unknown", " level | ",
            comp$subjects %||% "?", " subjects | ",
            comp$raters %||% "?", " raters\n", sep = "")
      }

      # Show validation summary if available
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


#' Save trail to RDS file
#'
#' Saves an audit trail to an RDS file for archival purposes. The trail includes
#' all coded data, creating a complete archive of your analysis.
#'
#' @param trail A `qlm_trail` object from [qlm_trail()].
#' @param file Path to save the RDS file.
#'
#' @return Invisibly returns the file path.
#'
#' @examples
#' \dontrun{
#' # Code movie reviews and create replication
#' coded1 <- qlm_code(data_corpus_LMRDsample, data_codebook_sentiment,
#'                    model = "openai/gpt-4o", name = "gpt4o_run")
#' coded2 <- qlm_replicate(coded1, model = "openai/gpt-4o-mini", name = "mini_run")
#'
#' # Extract trail and save
#' trail <- qlm_trail(coded1, coded2)
#' qlm_trail_save(trail, "analysis_trail.rds")
#' }
#'
#' @seealso [qlm_trail()], [qlm_archive()]
#' @export
qlm_trail_save <- function(trail, file) {
  if (!inherits(trail, "qlm_trail")) {
    cli::cli_abort(c(
      "{.arg trail} must be a {.cls qlm_trail} object.",
      "i" = "Create one with {.fn qlm_trail}."
    ))
  }

  saveRDS(trail, file)
  cli::cli_alert_success("Trail saved to {.path {file}}")

  invisible(file)
}


#' Export trail to JSON
#'
#' Exports an audit trail to JSON format for portability and archival.
#'
#' @param trail A `qlm_trail` object from [qlm_trail()].
#' @param file Path to save the JSON file.
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' The JSON export includes:
#' - Run names and parent relationships
#' - Timestamps
#' - Model names and parameters
#' - Codebook names
#' - Call information (as text)
#'
#' Large objects like the full codebook schema and coded data are stored in
#' the RDS format (via [qlm_trail_save()]) rather than JSON.
#'
#' @examples
#' \dontrun{
#' # Code movie reviews and create replication
#' coded1 <- qlm_code(data_corpus_LMRDsample, data_codebook_sentiment,
#'                    model = "openai/gpt-4o", name = "gpt4o_run")
#' coded2 <- qlm_replicate(coded1, model = "openai/gpt-4o-mini", name = "mini_run")
#'
#' # Extract trail and export to JSON
#' trail <- qlm_trail(coded1, coded2)
#' qlm_trail_export(trail, "analysis_trail.json")
#' }
#'
#' @seealso [qlm_trail()], [qlm_archive()]
#' @export
qlm_trail_export <- function(trail, file) {
  if (!inherits(trail, "qlm_trail")) {
    cli::cli_abort(c(
      "{.arg trail} must be a {.cls qlm_trail} object.",
      "i" = "Create one with {.fn qlm_trail}."
    ))
  }

  # Convert trail to JSON-friendly format
  export_data <- list(
    complete = trail$complete,
    n_runs = length(trail$runs),
    runs = lapply(trail$runs, function(run) {
      list(
        name = run$name,
        parent = run$parent,
        timestamp = if (!is.null(run$metadata$timestamp)) {
          format(run$metadata$timestamp, "%Y-%m-%d %H:%M:%S %Z")
        } else {
          NULL
        },
        model = run$chat_args$name,
        temperature = run$chat_args$temperature,
        codebook_name = run$codebook$name,
        call = deparse(run$call),
        metadata = list(
          n_units = run$metadata$n_units,
          quallmer_version = run$metadata$quallmer_version,
          ellmer_version = run$metadata$ellmer_version,
          R_version = run$metadata$R_version
        )
      )
    })
  )

  # Write JSON
  json_text <- jsonlite::toJSON(export_data, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_text, file)

  cli::cli_alert_success("Trail exported to {.path {file}}")

  invisible(file)
}


#' Generate trail report
#'
#' Generates a human-readable Quarto/RMarkdown document summarizing the
#' audit trail, optionally including assessment metrics across runs.
#'
#' @param trail A `qlm_trail` object from [qlm_trail()].
#' @param file Path to save the report file (`.qmd` or `.Rmd`).
#' @param include_comparisons Logical. If `TRUE`, include comparison metrics
#'   in the report (if any comparisons are in the trail). Default is `FALSE`.
#' @param include_validations Logical. If `TRUE`, include validation metrics
#'   in the report (if any validations are in the trail). Default is `FALSE`.
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' Creates a formatted document showing:
#' - Trail summary and completeness
#' - Timeline of runs
#' - Model parameters and settings for each run
#' - Parent-child relationships
#' - Assessment metrics (if requested):
#'   - Inter-rater reliability comparisons
#'   - Validation results against gold standards
#'
#' The generated file can be rendered to HTML, PDF, or other formats using
#' Quarto or RMarkdown.
#'
#' @examples
#' \dontrun{
#' # Code movie reviews and create replication
#' coded1 <- qlm_code(data_corpus_LMRDsample, data_codebook_sentiment,
#'                    model = "openai/gpt-4o", name = "gpt4o_run")
#' coded2 <- qlm_replicate(coded1, model = "openai/gpt-4o-mini", name = "mini_run")
#'
#' # Generate basic trail report
#' trail <- qlm_trail(coded1, coded2)
#' qlm_trail_report(trail, "analysis_trail.qmd")
#'
#' # Include comparison metrics
#' comparison <- qlm_compare(coded1, coded2, by = sentiment, level = "nominal")
#' trail <- qlm_trail(coded1, coded2, comparison)
#' qlm_trail_report(trail, "full_report.qmd", include_comparisons = TRUE)
#'
#' # Render to HTML
#' quarto::quarto_render("full_report.qmd")
#' }
#'
#' @seealso [qlm_trail()], [qlm_archive()]
#' @export
qlm_trail_report <- function(trail, file, include_comparisons = FALSE,
                              include_validations = FALSE) {
  if (!inherits(trail, "qlm_trail")) {
    cli::cli_abort(c(
      "{.arg trail} must be a {.cls qlm_trail} object.",
      "i" = "Create one with {.fn qlm_trail}."
    ))
  }

  # Determine format from extension
  ext <- tools::file_ext(file)
  if (!ext %in% c("qmd", "Rmd")) {
    cli::cli_abort(c(
      "File must have {.file .qmd} or {.file .Rmd} extension.",
      "x" = "Got {.file .{ext}}"
    ))
  }

  # Generate report content
  lines <- character()

  # YAML header
  lines <- c(lines, "---")
  lines <- c(lines, "title: \"quallmer trail\"")
  lines <- c(lines, paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""))
  if (ext == "qmd") {
    lines <- c(lines, "format: html")
  } else {
    lines <- c(lines, "output: html_document")
  }

  lines <- c(lines, "---")
  lines <- c(lines, "")

  # Summary
  lines <- c(lines, "## Trail summary")
  lines <- c(lines, "")
  lines <- c(lines, paste("- **Number of runs:**", length(trail$runs)))
  lines <- c(lines, paste("- **Complete:**", if (trail$complete) "Yes" else "No (missing parent runs)"))
  lines <- c(lines, "")

  # Timeline
  lines <- c(lines, "## Timeline")
  lines <- c(lines, "")

  for (i in seq_along(trail$runs)) {
    run <- trail$runs[[i]]

    lines <- c(lines, paste0("### ", i, ". ", run$name))
    lines <- c(lines, "")

    if (!is.null(run$parent)) {
      lines <- c(lines, paste("**Parent run:**", run$parent))
    } else {
      lines <- c(lines, "**Parent run:** None (original)")
    }

    if (!is.null(run$metadata$timestamp)) {
      lines <- c(lines, paste("**Timestamp:**", format(run$metadata$timestamp, "%Y-%m-%d %H:%M:%S")))
    }

    if (!is.null(run$chat_args$name)) {
      lines <- c(lines, paste("**Model:**", run$chat_args$name))
    }

    if (!is.null(run$chat_args$temperature)) {
      lines <- c(lines, paste("**Temperature:**", run$chat_args$temperature))
    }

    if (!is.null(run$codebook$name)) {
      lines <- c(lines, paste("**Codebook:**", run$codebook$name))
    }

    if (!is.null(run$metadata$n_units)) {
      lines <- c(lines, paste("**Units coded:**", run$metadata$n_units))
    }

    lines <- c(lines, "")
    lines <- c(lines, "**Call:**")
    lines <- c(lines, "```r")
    lines <- c(lines, deparse(run$call))
    lines <- c(lines, "```")
    lines <- c(lines, "")
  }

  # Extract comparisons and validations from trail if requested
  comparisons_list <- list()
  validations_list <- list()

  if (include_comparisons || include_validations) {
    for (run_name in names(trail$runs)) {
      run <- trail$runs[[run_name]]

      # Check if this run is from a comparison object
      if (include_comparisons && !is.null(run$comparison_data)) {
        comp_data <- run$comparison_data
        parents_str <- if (is.null(run$parent) || length(run$parent) == 0) {
          ""
        } else {
          paste(run$parent, collapse = ", ")
        }

        # Collect relevant measures based on level
        measures <- list()
        if (!is.null(comp_data$level)) {
          if (comp_data$level == "nominal") {
            measures$alpha_nominal <- comp_data$alpha_nominal
            measures$kappa <- comp_data$kappa
            measures$kappa_type <- comp_data$kappa_type
          } else if (comp_data$level == "ordinal") {
            measures$alpha_ordinal <- comp_data$alpha_ordinal
            measures$kappa_weighted <- comp_data$kappa_weighted
            measures$w <- comp_data$w
            measures$rho <- comp_data$rho
          } else if (comp_data$level %in% c("interval", "ratio")) {
            measures$alpha_interval <- comp_data$alpha_interval
            measures$icc <- comp_data$icc
            measures$r <- comp_data$r
          }
          measures$percent_agreement <- comp_data$percent_agreement
        }

        comparisons_list[[length(comparisons_list) + 1]] <- list(
          run = run$name,
          parents = parents_str,
          level = comp_data$level %||% "unknown",
          subjects = comp_data$subjects %||% NA_integer_,
          raters = comp_data$raters %||% NA_integer_,
          measures = measures
        )
      }

      # Check if this run is from a validation object
      if (include_validations && !is.null(run$validation_data)) {
        val_data <- run$validation_data
        parents_str <- if (is.null(run$parent) || length(run$parent) == 0) {
          ""
        } else {
          paste(run$parent, collapse = ", ")
        }

        # Collect relevant metrics based on level
        metrics <- list()
        if (!is.null(val_data$level)) {
          if (val_data$level == "nominal") {
            metrics$accuracy <- val_data$accuracy
            metrics$precision <- val_data$precision
            metrics$recall <- val_data$recall
            metrics$f1 <- val_data$f1
            metrics$kappa <- val_data$kappa
          } else if (val_data$level == "ordinal") {
            metrics$rho <- val_data$rho
            metrics$tau <- val_data$tau
            metrics$mae <- val_data$mae
          } else if (val_data$level == "interval") {
            metrics$r <- val_data$r
            metrics$icc <- val_data$icc
            metrics$mae <- val_data$mae
            metrics$rmse <- val_data$rmse
          }
        }

        validations_list[[length(validations_list) + 1]] <- list(
          run = run$name,
          parents = parents_str,
          level = val_data$level %||% "unknown",
          n = val_data$n %||% NA_integer_,
          average = val_data$average,
          metrics = metrics
        )
      }
    }
  }

  # Assessment Metrics
  has_metrics <- length(comparisons_list) > 0 || length(validations_list) > 0

  if (has_metrics) {
    lines <- c(lines, "## Assessment metrics")
    lines <- c(lines, "")

    # Comparisons
    if (length(comparisons_list) > 0) {
      lines <- c(lines, "### Inter-rater reliability comparisons")
      lines <- c(lines, "")
      lines <- c(lines, "The following comparisons were performed to assess agreement between runs:")
      lines <- c(lines, "")

      for (comp in comparisons_list) {
        lines <- c(lines, sprintf("#### %s", comp$run))
        lines <- c(lines, "")
        lines <- c(lines, paste("- **Compared runs:**", comp$parents))
        lines <- c(lines, paste("- **Level:**", comp$level))
        lines <- c(lines, paste("- **Subjects:**", comp$subjects))
        lines <- c(lines, paste("- **Raters:**", comp$raters))
        lines <- c(lines, "")

        # Display measures
        if (length(comp$measures) > 0) {
          lines <- c(lines, "**Measures:**")
          lines <- c(lines, "")
          for (measure_name in names(comp$measures)) {
            measure_value <- comp$measures[[measure_name]]
            if (!is.null(measure_value) && !is.na(measure_value)) {
              # Format measure name nicely
              display_name <- switch(measure_name,
                "alpha_nominal" = "Krippendorff's alpha (nominal)",
                "alpha_ordinal" = "Krippendorff's alpha (ordinal)",
                "alpha_interval" = "Krippendorff's alpha (interval)",
                "kappa" = paste0(comp$measures$kappa_type %||% "Cohen's", " kappa"),
                "kappa_weighted" = "Weighted kappa",
                "kappa_type" = NULL,  # Skip this, shown with kappa
                "w" = "Kendall's W",
                "rho" = "Spearman's rho",
                "icc" = "ICC",
                "r" = "Pearson's r",
                "percent_agreement" = "Percent agreement",
                measure_name
              )
              if (!is.null(display_name)) {
                if (is.numeric(measure_value)) {
                  lines <- c(lines, sprintf("- %s: %.4f", display_name, measure_value))
                } else {
                  lines <- c(lines, sprintf("- %s: %s", display_name, measure_value))
                }
              }
            }
          }
          lines <- c(lines, "")
        }
      }
    }

    # Validations
    if (length(validations_list) > 0) {
      lines <- c(lines, "### Validation against gold standard")
      lines <- c(lines, "")
      lines <- c(lines, "The following runs were validated against gold standard annotations:")
      lines <- c(lines, "")

      for (val in validations_list) {
        lines <- c(lines, sprintf("#### %s", val$run))
        lines <- c(lines, "")
        lines <- c(lines, paste("- **Compared runs:**", val$parents))
        lines <- c(lines, paste("- **Level:**", val$level))
        lines <- c(lines, paste("- **N:**", val$n))
        if (!is.null(val$average)) {
          lines <- c(lines, paste("- **Average:**", val$average))
        }
        lines <- c(lines, "")

        # Display metrics
        if (length(val$metrics) > 0) {
          lines <- c(lines, "**Metrics:**")
          lines <- c(lines, "")
          for (metric_name in names(val$metrics)) {
            metric_value <- val$metrics[[metric_name]]
            if (!is.null(metric_value) && !is.na(metric_value)) {
              # Format metric name nicely
              display_name <- switch(metric_name,
                "accuracy" = "Accuracy",
                "precision" = "Precision",
                "recall" = "Recall",
                "f1" = "F1-score",
                "kappa" = "Cohen's kappa",
                "rho" = "Spearman's rho",
                "tau" = "Kendall's tau",
                "r" = "Pearson's r",
                "icc" = "ICC",
                "mae" = "MAE",
                "rmse" = "RMSE",
                metric_name
              )
              lines <- c(lines, sprintf("- %s: %.4f", display_name, metric_value))
            }
          }
          lines <- c(lines, "")
        }
      }
    }
  }

  # Metadata
  lines <- c(lines, "## System information")
  lines <- c(lines, "")

  # Get from most recent run
  if (length(trail$runs) > 0) {
    last_run <- trail$runs[[length(trail$runs)]]
    if (!is.null(last_run$metadata)) {
      lines <- c(lines, paste("- **quallmer version:**", last_run$metadata$quallmer_version %||% "unknown"))
      lines <- c(lines, paste("- **ellmer version:**", last_run$metadata$ellmer_version %||% "unknown"))
      lines <- c(lines, paste("- **R version:**", last_run$metadata$R_version %||% "unknown"))
    }
  }

  # Write file
  writeLines(lines, file)

  cli::cli_alert_success("Trail report saved to {.path {file}}")
  cli::cli_alert_info("Render with {.code quarto::quarto_render(\"{file}\")} or {.code rmarkdown::render(\"{file}\")}")

  invisible(file)
}


#' Archive quallmer workflow
#'
#' Convenience function that saves, exports, and optionally generates a report
#' for a quallmer workflow in one call. Accepts either coded objects directly
#' or a pre-built `qlm_trail` object.
#'
#' @param x Either a `qlm_trail` object from [qlm_trail()], or the first
#'   quallmer object (`qlm_coded`, `qlm_comparison`, or `qlm_validation`).
#' @param ... Additional quallmer objects to include in the trail (only used
#'   when `x` is not a `qlm_trail` object).
#' @param path Base path for output files. Creates `{path}.rds`, `{path}.json`,
#'   and optionally `{path}.qmd`.
#' @param report Logical. If `TRUE`, generates a Quarto report file.
#'   Default is `TRUE`.
#'
#' @return Invisibly returns the `qlm_trail` object.
#'
#' @details
#' This function creates a complete archive of your workflow:
#' - `{path}.rds`: Complete trail object for R (can be reloaded with `readRDS()`)
#' - `{path}.json`: Portable metadata for archival or sharing
#' - `{path}.qmd`: Human-readable report (if `report = TRUE`)
#'
#' The function can be used in two ways:
#'
#' 1. **Standalone**: Pass coded objects directly:
#'    ```r
#'    qlm_archive(coded1, coded2, path = "workflow")
#'    ```
#'
#' 2. **Piped**: Pass a pre-built trail:
#'    ```r
#'    qlm_trail(coded1, coded2) |>
#'      qlm_archive(path = "workflow")
#'    ```
#'
#' @examples
#' \dontrun{
#' # Code movie reviews and create replication
#' coded1 <- qlm_code(data_corpus_LMRDsample, data_codebook_sentiment,
#'                    model = "openai/gpt-4o", name = "gpt4o_run")
#' coded2 <- qlm_replicate(coded1, model = "openai/gpt-4o-mini", name = "mini_run")
#'
#' # Archive entire workflow in one call
#' qlm_archive(coded1, coded2, path = "workflow")
#'
#' # Piped usage
#' qlm_trail(coded1, coded2) |>
#'   qlm_archive(path = "workflow")
#'
#' # Without report
#' qlm_archive(coded1, coded2, path = "workflow", report = FALSE)
#' }
#'
#' @seealso [qlm_trail()], [qlm_trail_save()], [qlm_trail_export()], [qlm_trail_report()]
#' @export
qlm_archive <- function(x, ..., path, report = TRUE) {
  # Check if x is already a trail
  if (inherits(x, "qlm_trail")) {
    trail <- x
    # Check for extra objects passed with a trail (not allowed)
    extra <- list(...)
    if (length(extra) > 0) {
      cli::cli_warn(c(
        "Extra objects ignored when {.arg x} is a {.cls qlm_trail}.",
        "i" = "Create trail with all objects: {.code qlm_trail(coded1, coded2, ...)}."
      ))
    }
  } else {
    # Build trail from objects
    trail <- qlm_trail(x, ...)
  }

  # Validate path
  if (missing(path)) {
    cli::cli_abort("{.arg path} is required.")
  }

  # Generate file paths
  rds_file <- paste0(path, ".rds")
  json_file <- paste0(path, ".json")
  qmd_file <- paste0(path, ".qmd")

  # Save RDS
  qlm_trail_save(trail, rds_file)

  # Export JSON
  qlm_trail_export(trail, json_file)

  # Generate report if requested
  if (report) {
    qlm_trail_report(trail, qmd_file)
  }

  invisible(trail)
}
