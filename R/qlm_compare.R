#' Compare coded results for inter-rater reliability
#'
#' Compares two or more data frames or `qlm_coded` objects to assess inter-rater
#' reliability or agreement. This function extracts a specified variable from
#' each object and computes reliability statistics using the irr package.
#'
#' @param ... Two or more data frames, `qlm_coded`, or `as_qlm_coded` objects
#'   to compare. These represent different "raters" (e.g., different LLM runs,
#'   different models, human coders, or human vs. LLM coding). Each object must
#'   have a `.id` column and the variable specified in `by`. Objects should have
#'   the same units (matching `.id` values). Plain data frames are automatically
#'   converted to `as_qlm_coded` objects.
#' @param by Optional. Name of the variable(s) to compare across raters (supports
#'   both quoted and unquoted). If `NULL` (default), all coded variables are
#'   compared. Can be a single variable (`by = sentiment`), a character vector
#'   (`by = c("sentiment", "rating")`), or NULL to process all variables.
#' @param level Optional. Measurement level(s) for the variable(s). Can be:
#'   \itemize{
#'     \item `NULL` (default): Auto-detect from codebook
#'     \item Character scalar: Use same level for all variables
#'     \item Named list: Specify level for each variable
#'   }
#'   Valid levels are `"nominal"`, `"ordinal"`, `"interval"`, or `"ratio"`.
#' @param tolerance Numeric. Tolerance for agreement with numeric data.
#'   Default is 0 (exact agreement required). Used for percent agreement calculation.
#' @param ci Confidence interval method:
#'   \describe{
#'     \item{`"none"`}{No confidence intervals (default)}
#'     \item{`"analytic"`}{Analytic CIs where available (ICC, Pearson's r)}
#'     \item{`"bootstrap"`}{Bootstrap CIs for all metrics via resampling}
#'   }
#' @param bootstrap_n Number of bootstrap resamples when `ci = "bootstrap"`.
#'   Default is 1000. Ignored when `ci` is `"none"` or `"analytic"`.
#'
#' @return A `qlm_comparison` object (a tibble/data frame) with the following columns:
#'   \describe{
#'     \item{`variable`}{Name of the compared variable}
#'     \item{`level`}{Measurement level used}
#'     \item{`measure`}{Name of the reliability metric}
#'     \item{`value`}{Computed value of the metric}
#'     \item{`rater1`, `rater2`, ...}{Names of the compared objects (one column per rater)}
#'     \item{`ci_lower`}{Lower bound of confidence interval (only if `ci != "none"`)}
#'     \item{`ci_upper`}{Upper bound of confidence interval (only if `ci != "none"`)}
#'   }
#'   The object has class `c("qlm_comparison", "tbl_df", "tbl", "data.frame")` and
#'   attributes containing metadata (`raters`, `n`, `call`).
#'
#'   **Metrics computed by measurement level:**
#'   \itemize{
#'     \item **Nominal:** alpha_nominal, kappa (Cohen's/Fleiss'), percent_agreement
#'     \item **Ordinal:** alpha_ordinal, kappa_weighted (2 raters only), w (Kendall's W),
#'       rho (Spearman's), percent_agreement
#'     \item **Interval/Ratio:** alpha_interval/alpha_ratio, icc, r (Pearson's),
#'       percent_agreement
#'   }
#'
#'   **Confidence intervals:**
#'   - `ci = "analytic"`: Provides analytic CIs for ICC and Pearson's r only
#'   - `ci = "bootstrap"`: Provides bootstrap CIs for all metrics via resampling
#'
#' @details
#' The function merges the coded objects by their `.id` column and only includes
#' units that are present in all objects. Missing values in any rater will
#' exclude that unit from analysis.
#'
#' **Measurement levels and statistics:**
#' - **Nominal**: For unordered categories. Computes Krippendorff's alpha,
#'   Cohen's/Fleiss' kappa, and percent agreement.
#' - **Ordinal**: For ordered categories. Computes Krippendorff's alpha (ordinal),
#'   weighted kappa (2 raters only), Kendall's W, Spearman's rho, and percent
#'   agreement.
#' - **Interval**: For continuous data with meaningful intervals. Computes
#'   Krippendorff's alpha (interval), ICC, Pearson's r, and percent agreement.
#' - **Ratio**: For continuous data with a true zero point. Computes the same
#'   measures as interval level, but Krippendorff's alpha uses the ratio-level
#'   formula which accounts for proportional differences.
#'
#' Kendall's W, ICC, and percent agreement are computed using all raters
#' simultaneously. For 3 or more raters, Spearman's rho and Pearson's r are
#' computed as the mean of all pairwise correlations between raters.
#'
#' @seealso [qlm_validate()] for validation of coding against gold standards,
#' [qlm_code()] for LLM coding, [as_qlm_coded()] for human coding.
#'
#' @examples
#' # Load example coded objects
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#'
#' # Compare two coding runs
#' comparison <- qlm_compare(
#'   examples$example_coded_sentiment,
#'   examples$example_coded_mini,
#'   by = "sentiment",
#'   level = "nominal"
#' )
#' print(comparison)
#'
#' # Compare specific variables with explicit levels
#' qlm_compare(
#'   examples$example_coded_sentiment,
#'   examples$example_coded_mini,
#'   by = "sentiment"
#' )
#'
#' @export
qlm_compare <- function(...,
                        by,
                        level = NULL,
                        tolerance = 0,
                        ci = c("none", "analytic", "bootstrap"),
                        bootstrap_n = 1000) {

  # Validate ci parameter
  ci <- match.arg(ci)

  # Check if 'by' was provided
  if (missing(by)) {
    by <- NULL
  } else {
    # Convert 'by' to string(s) - supports both bare names and character vectors
    by <- tryCatch({
      # Try to capture as symbol(s) first
      rlang::as_string(rlang::ensym(by))
    }, error = function(e) {
      # If that fails, it might already be a character vector
      if (is.character(by)) {
        by
      } else {
        cli::cli_abort(c(
          "Invalid {.arg by} argument.",
          "i" = "Use an unquoted variable name (e.g., {.code by = sentiment}) or a character vector (e.g., {.code by = c('sentiment', 'rating')})."
        ))
      }
    })
  }

  # Capture coded objects
  coded_list <- list(...)

  # Validate inputs
  if (length(coded_list) < 2) {
    cli::cli_abort("At least two data frames or {.cls qlm_coded} objects are required for comparison.")
  }

  # Check all objects are data frames
  not_df <- !vapply(coded_list, is.data.frame, logical(1))
  if (any(not_df)) {
    cli::cli_abort(c(
      "All arguments in {.arg ...} must be data frames or {.cls qlm_coded} objects.",
      "x" = "Argument{?s} {which(not_df)} {?is/are} not data frame{?s}.",
      "i" = "Provide data frames with a {.var .id} column and the variable to compare."
    ))
  }

  # Auto-convert plain data.frames to as_qlm_coded with informational message
  not_coded <- !vapply(coded_list, inherits, logical(1), "qlm_coded")
  if (any(not_coded)) {
    cli::cli_inform(c(
      "i" = "Converting {sum(not_coded)} plain data frame{?s} to {.cls as_qlm_coded} object{?s}.",
      "i" = "Use {.fn as_qlm_coded} directly to provide coder names and metadata."
    ))
    for (i in which(not_coded)) {
      coded_list[[i]] <- as_qlm_coded(
        coded_list[[i]],
        name = paste0("auto_converted_", i)
      )
    }
  }

  # Determine which variables to process
  if (is.null(by)) {
    # Extract all coded variables from first object
    by <- get_coded_variables(coded_list[[1]])
    if (length(by) == 0) {
      cli::cli_abort(c(
        "No coded variables found in objects.",
        "i" = "Objects must have variables other than {.var .id}."
      ))
    }
  }

  # Check 'by' variables exist in all objects
  named_list <- coded_list
  names(named_list) <- paste("object", seq_along(coded_list))
  for (var in by) {
    validate_by_variable(var, named_list)
  }

  # Extract codebook from first qlm_coded object
  codebook <- NULL
  for (obj in coded_list) {
    codebook <- extract_codebook_from_coded(obj)
    if (!is.null(codebook)) {
      break
    }
  }

  # Determine levels for each variable
  if (is.null(level)) {
    # Auto-detect from codebook
    if (is.null(codebook)) {
      cli::cli_abort(c(
        "Cannot auto-detect measurement levels without a codebook.",
        "i" = "Provide explicit {.arg level} parameter or use objects with codebooks.",
        "i" = "Use {.fn qlm_code} to create coded objects with codebooks."
      ))
    }
    levels_map <- qlm_levels(codebook)
    if (is.null(levels_map)) {
      cli::cli_abort(c(
        "Cannot auto-detect measurement levels from codebook schema.",
        "i" = "Provide explicit {.arg level} parameter."
      ))
    }

    # Extract levels for variables we're processing
    variable_levels <- levels_map[by]

    # Check that all variables have levels
    missing_levels <- is.na(variable_levels) | variable_levels == "" | !(by %in% names(levels_map))
    if (any(missing_levels)) {
      missing_vars <- by[missing_levels]
      cli::cli_abort(c(
        "Cannot determine measurement level for variable{?s}: {.var {missing_vars}}",
        "i" = "Available levels in codebook: {.val {names(levels_map)}}",
        "i" = "Provide explicit {.arg level} parameter for these variables."
      ))
    }
  } else if (is.character(level) && length(level) == 1) {
    # Single level applies to all variables
    level <- match.arg(level, c("nominal", "ordinal", "interval", "ratio"))
    variable_levels <- stats::setNames(rep(level, length(by)), by)
  } else if (is.list(level) || (is.character(level) && !is.null(names(level)))) {
    # Named list/vector of levels
    # Validate that all specified variables are in 'by'
    unknown_vars <- setdiff(names(level), by)
    if (length(unknown_vars) > 0) {
      cli::cli_abort(c(
        "Variables in {.arg level} not found in {.arg by}: {.var {unknown_vars}}",
        "i" = "Variables to compare: {.val {by}}"
      ))
    }

    # Validate level values
    valid_levels <- c("nominal", "ordinal", "interval", "ratio")
    invalid <- !level %in% valid_levels
    if (any(invalid)) {
      invalid_names <- names(level)[invalid]
      cli::cli_abort(c(
        "Invalid measurement level{?s} for: {.var {invalid_names}}",
        "i" = "Valid levels are: {.val {valid_levels}}"
      ))
    }

    # Use specified levels, fill in missing with auto-detection if possible
    variable_levels <- rep(NA_character_, length(by))
    names(variable_levels) <- by
    variable_levels[names(level)] <- unlist(level)

    # Try to fill in missing levels from codebook
    if (any(is.na(variable_levels)) && !is.null(codebook)) {
      codebook_levels <- qlm_levels(codebook)
      if (!is.null(codebook_levels)) {
        missing_idx <- is.na(variable_levels)
        variable_levels[missing_idx] <- codebook_levels[names(variable_levels)[missing_idx]]
      }
    }

    # Check for still-missing levels
    if (any(is.na(variable_levels))) {
      missing_vars <- names(variable_levels)[is.na(variable_levels)]
      cli::cli_abort(c(
        "Cannot determine measurement level for variable{?s}: {.var {missing_vars}}",
        "i" = "Provide explicit level in {.arg level} parameter."
      ))
    }
  } else {
    cli::cli_abort(c(
      "Invalid {.arg level} argument.",
      "i" = "Must be NULL (auto-detect), a single level, or a named list/vector."
    ))
  }

  # Now process each variable and build results data frame
  n_raters <- length(coded_list)

  # Extract object names from metadata
  object_names <- vapply(coded_list, function(obj) {
    name <- tryCatch(qlm_meta(obj, "name"), error = function(e) NULL)
    if (!is.null(name)) name else NA_character_
  }, character(1))

  # Create names for raters (used in ratings matrix column names)
  rater_names <- names(coded_list)
  if (is.null(rater_names)) {
    rater_names <- paste0("rater", seq_len(n_raters))
  } else {
    # Replace empty names with default
    empty <- rater_names == ""
    rater_names[empty] <- paste0("rater", seq_len(n_raters))[empty]
  }

  # Initialize results list
  all_results <- list()
  n_subjects_total <- NULL

  # Process each variable
  for (var in by) {
    var_level <- variable_levels[[var]]

    # Extract relevant columns and rename for this variable
    data_list <- lapply(seq_along(coded_list), function(i) {
      obj <- coded_list[[i]]
      df <- data.frame(
        .id = obj[[".id"]],
        value = obj[[var]],
        stringsAsFactors = FALSE
      )
      names(df)[2] <- rater_names[i]
      df
    })

    # Merge all data frames
    merged <- data_list[[1]]
    for (i in seq(2, length(data_list))) {
      merged <- merge(merged, data_list[[i]], by = ".id", all = FALSE)
    }

    if (nrow(merged) == 0) {
      cli::cli_warn(c(
        "No common units found for variable {.var {var}}.",
        "i" = "Skipping this variable."
      ))
      next
    }

    # Extract rating matrix (exclude .id column)
    ratings <- as.matrix(merged[, -1, drop = FALSE])

    # Remove rows with any NA values
    complete_rows <- stats::complete.cases(ratings)
    if (!any(complete_rows)) {
      cli::cli_warn(c(
        "No complete cases found for variable {.var {var}}.",
        "i" = "Skipping this variable."
      ))
      next
    }

    ratings <- ratings[complete_rows, , drop = FALSE]
    n_subjects <- nrow(ratings)

    # Store n_subjects for first variable (for metadata)
    if (is.null(n_subjects_total)) {
      n_subjects_total <- n_subjects
    }

    # Compute all reliability measures appropriate for this level
    reliability_output <- compute_reliability_by_level(ratings, n_raters, var_level, tolerance,
                                                        use_ci = if (ci == "analytic") "analytic" else FALSE)

    # Extract results and CIs based on CI method
    if (ci == "analytic") {
      var_results <- reliability_output$values
      var_cis <- reliability_output$cis
    } else if (ci == "bootstrap") {
      var_results <- reliability_output
      # Compute bootstrap CIs
      var_cis <- bootstrap_reliability_ci(ratings, n_raters, var_level, tolerance, bootstrap_n)
    } else {
      var_results <- reliability_output
      var_cis <- NULL
    }

    # Convert results to data frame rows (exclude kappa_type which is metadata)
    for (measure_name in names(var_results)) {
      # Skip kappa_type as it's metadata, not a metric
      if (measure_name == "kappa_type") {
        next
      }

      result_row <- list(
        variable = var,
        level = var_level,
        measure = measure_name,
        value = var_results[[measure_name]]
      )

      # Add rater name columns
      for (i in seq_along(object_names)) {
        result_row[[paste0("rater", i)]] <- object_names[i]
      }

      # Add CI columns if requested
      if (ci != "none") {
        if (!is.null(var_cis[[measure_name]])) {
          result_row$ci_lower <- as.numeric(var_cis[[measure_name]]["lower"])
          result_row$ci_upper <- as.numeric(var_cis[[measure_name]]["upper"])
        } else {
          result_row$ci_lower <- NA_real_
          result_row$ci_upper <- NA_real_
        }
      }

      all_results[[length(all_results) + 1]] <- result_row
    }
  }

  # Check that we got some results
  if (length(all_results) == 0) {
    cli::cli_abort(c(
      "No valid comparisons could be computed.",
      "i" = "Check that objects have overlapping {.var .id} values and non-missing data."
    ))
  }

  # Build data frame using dplyr::bind_rows to handle mixed types better
  result_df <- dplyr::bind_rows(all_results)

  # Extract parent run names from coded objects
  parent_names <- vapply(coded_list, function(obj) {
    name <- tryCatch(qlm_meta(obj, "name"), error = function(e) NULL)
    if (!is.null(name)) name else NA_character_
  }, character(1))

  # Add attributes and class with new metadata structure
  structure(
    result_df,
    class = c("qlm_comparison", class(result_df)),
    raters = n_raters,
    n = n_subjects_total,
    call = match.call(),
    meta = list(
      user = list(
        name = paste0("comparison_", substr(digest::digest(parent_names), 1, 8)),
        notes = NULL
      ),
      object = list(
        call = match.call(),
        parent = parent_names[!is.na(parent_names)],
        n_raters = n_raters,
        variables = by
      ),
      system = list(
        timestamp = Sys.time(),
        quallmer_version = tryCatch(as.character(utils::packageVersion("quallmer")), error = function(e) NA_character_),
        R_version = paste(R.version$major, R.version$minor, sep = ".")
      )
    )
  )
}


#' Convert ratings to numeric format
#'
#' Converts categorical (character/factor) ratings to numeric format for irr package.
#' If data is already numeric, returns as-is.
#'
#' @param ratings Matrix of ratings
#' @return Numeric matrix
#' @keywords internal
#' @noRd
convert_to_numeric <- function(ratings) {
  # If already numeric, return as-is
  if (is.numeric(ratings)) {
    return(ratings)
  }

  # Get unique levels across all raters
  all_values <- unique(as.vector(ratings))
  all_levels <- sort(all_values[!is.na(all_values)])

  # Convert each column to factor with common levels, then to integer
  ratings_numeric <- apply(ratings, 2, function(col) {
    as.integer(factor(col, levels = all_levels))
  })

  # Preserve dimension names
  dimnames(ratings_numeric) <- dimnames(ratings)

  ratings_numeric
}


#' Bootstrap confidence intervals for reliability metrics
#'
#' @param ratings Matrix where rows are subjects and columns are raters
#' @param n_raters Number of raters
#' @param level Measurement level
#' @param tolerance Tolerance for agreement
#' @param bootstrap_n Number of bootstrap resamples
#'
#' @return List of CI bounds (lower, upper) for each metric
#' @keywords internal
#' @noRd
bootstrap_reliability_ci <- function(ratings, n_raters, level, tolerance, bootstrap_n) {
  n_subjects <- nrow(ratings)

  # Storage for bootstrap distributions
  bootstrap_results <- list()

  # Perform bootstrap resampling
  for (b in seq_len(bootstrap_n)) {
    # Resample subjects (rows) with replacement
    sample_idx <- sample(seq_len(n_subjects), size = n_subjects, replace = TRUE)
    boot_ratings <- ratings[sample_idx, , drop = FALSE]

    # Compute metrics on bootstrap sample
    boot_metrics <- compute_reliability_by_level(boot_ratings, n_raters, level, tolerance, use_ci = FALSE)

    # Store results
    for (metric_name in names(boot_metrics)) {
      if (metric_name != "kappa_type") {  # Skip metadata
        bootstrap_results[[metric_name]] <- c(bootstrap_results[[metric_name]], boot_metrics[[metric_name]])
      }
    }
  }

  # Compute CI from bootstrap distribution (percentile method)
  cis <- list()
  for (metric_name in names(bootstrap_results)) {
    values <- bootstrap_results[[metric_name]]
    # Remove NAs before computing quantiles
    values <- values[!is.na(values)]
    if (length(values) > 0) {
      # Use unname() to remove quantile's automatic naming
      ci_values <- unname(stats::quantile(values, c(0.025, 0.975), na.rm = TRUE))
      cis[[metric_name]] <- c(lower = ci_values[1], upper = ci_values[2])
    } else {
      cis[[metric_name]] <- c(lower = NA_real_, upper = NA_real_)
    }
  }

  cis
}


#' Compute all reliability statistics for a given level
#'
#' @param ratings Matrix where rows are subjects and columns are raters
#' @param n_raters Number of raters
#' @param level Measurement level
#' @param tolerance Tolerance for agreement
#' @param use_ci CI method: FALSE, "analytic", or "bootstrap"
#'
#' @return List with all computed measures (and optionally CIs)
#' @keywords internal
#' @noRd
compute_reliability_by_level <- function(ratings, n_raters, level, tolerance, use_ci = FALSE) {

  results <- list()
  cis <- list()  # Store confidence intervals separately

  # Percent agreement (computed for all levels)
  # For nominal data: exact agreement (tolerance is ignored)
  # For ordinal/interval/ratio: agreement within tolerance
  agrees <- apply(ratings, 1, function(row) {
    if (is.numeric(row)) {
      max(row) - min(row) <= tolerance
    } else {
      length(unique(as.character(row))) == 1
    }
  })
  results$percent_agreement <- mean(agrees)

  if (level == "nominal") {
    # Nominal measures: alpha, kappa

    # Krippendorff's alpha (nominal)
    ratings_numeric <- convert_to_numeric(ratings)
    ratings_t <- t(ratings_numeric)

    alpha_result <- tryCatch({
      irr::kripp.alpha(ratings_t, method = "nominal")
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to compute Krippendorff's alpha.",
        "x" = conditionMessage(e)
      ))
      list(value = NA_real_)
    })
    results$alpha_nominal <- alpha_result$value

    # Cohen's/Fleiss' kappa
    kappa_result <- tryCatch({
      if (n_raters == 2) {
        irr::kappa2(ratings_numeric)
      } else {
        irr::kappam.fleiss(ratings_numeric)
      }
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to compute kappa.",
        "x" = conditionMessage(e)
      ))
      list(value = NA_real_)
    })
    results$kappa <- kappa_result$value
    results$kappa_type <- if (n_raters == 2) "Cohen's" else "Fleiss'"

  } else if (level == "ordinal") {
    # Ordinal measures: alpha_ordinal, kappa_weighted, w, rho

    # Krippendorff's alpha (ordinal)
    ratings_numeric <- convert_to_numeric(ratings)
    ratings_t <- t(ratings_numeric)

    alpha_result <- tryCatch({
      irr::kripp.alpha(ratings_t, method = "ordinal")
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to compute Krippendorff's alpha (ordinal).",
        "x" = conditionMessage(e)
      ))
      list(value = NA_real_)
    })
    results$alpha_ordinal <- alpha_result$value

    # Weighted kappa (only for 2 raters)
    if (n_raters == 2) {
      kappa_weighted_result <- tryCatch({
        irr::kappa2(ratings_numeric, weight = "squared")
      }, error = function(e) {
        cli::cli_warn(c(
          "Failed to compute weighted kappa.",
          "x" = conditionMessage(e)
        ))
        list(value = NA_real_)
      })
      results$kappa_weighted <- kappa_weighted_result$value
    } else {
      results$kappa_weighted <- NULL
    }

    # Kendall's W
    kendall_result <- tryCatch({
      irr::kendall(ratings_numeric)
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to compute Kendall's W.",
        "x" = conditionMessage(e)
      ))
      list(value = NA_real_)
    })
    results$w <- kendall_result$value

    # Spearman's rho (average pairwise correlation)
    # Note: cor.test does not provide CIs for Spearman's rho
    if (n_raters >= 2) {
      rho_values <- c()
      for (i in 1:(n_raters - 1)) {
        for (j in (i + 1):n_raters) {
          rho_values <- c(rho_values,
                          stats::cor(ratings_numeric[, i],
                                     ratings_numeric[, j],
                                     method = "spearman"))
        }
      }
      results$rho <- if (n_raters == 2) rho_values else mean(rho_values)
      # No CI available for Spearman's rho
    } else {
      results$rho <- NA_real_
    }

  } else if (level == "interval" || level == "ratio") {
    # Interval measures: alpha_interval, icc, r

    # Krippendorff's alpha (interval/ratio)
    ratings_numeric <- convert_to_numeric(ratings)
    ratings_t <- t(ratings_numeric)

    alpha_result <- tryCatch({
      irr::kripp.alpha(ratings_t, method = if (level == "interval") "interval" else "ratio")
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to compute Krippendorff's alpha (interval/ratio).",
        "x" = conditionMessage(e)
      ))
      list(value = NA_real_)
    })
    results$alpha_interval <- alpha_result$value

    # ICC
    icc_result <- tryCatch({
      irr::icc(ratings_numeric, model = "twoway", type = "agreement", unit = "single")
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to compute ICC.",
        "x" = conditionMessage(e)
      ))
      list(value = NA_real_, lbound = NA_real_, ubound = NA_real_)
    })
    results$icc <- icc_result$value
    if (use_ci == "analytic") {
      cis$icc <- c(lower = icc_result$lbound, upper = icc_result$ubound)
    }

    # Pearson's r (average pairwise correlation)
    if (n_raters >= 2) {
      if (use_ci == "analytic" && n_raters == 2) {
        # For 2 raters, we can get CI from cor.test
        cor_result <- tryCatch({
          stats::cor.test(ratings_numeric[, 1], ratings_numeric[, 2], method = "pearson")
        }, error = function(e) {
          list(estimate = NA_real_, conf.int = c(NA_real_, NA_real_))
        })
        results$r <- as.numeric(cor_result$estimate)
        cis$r <- c(lower = as.numeric(cor_result$conf.int[1]),
                   upper = as.numeric(cor_result$conf.int[2]))
      } else {
        # For 3+ raters, compute average pairwise (no analytic CI available)
        r_values <- c()
        for (i in 1:(n_raters - 1)) {
          for (j in (i + 1):n_raters) {
            r_values <- c(r_values,
                          stats::cor(ratings_numeric[, i],
                                     ratings_numeric[, j],
                                     method = "pearson"))
          }
        }
        results$r <- mean(r_values)
        if (use_ci == "analytic") {
          cis$r <- c(lower = NA_real_, upper = NA_real_)  # No analytic CI for average of correlations
        }
      }
    } else {
      results$r <- NA_real_
      if (use_ci == "analytic") {
        cis$r <- c(lower = NA_real_, upper = NA_real_)
      }
    }
  }

  # Return results with CIs if requested
  if (use_ci == "analytic") {
    list(values = results, cis = cis)
  } else {
    results
  }
}

#' Extract codebook from a qlm_coded object
#'
#' @param obj A qlm_coded object
#' @return A qlm_codebook object or NULL
#' @keywords internal
#' @noRd
extract_codebook_from_coded <- function(obj) {
  if (!inherits(obj, "qlm_coded")) {
    return(NULL)
  }

  codebook(obj)
}


#' Get coded variable names from a qlm_coded object
#'
#' Extracts variable names excluding .id
#'
#' @param obj A qlm_coded object
#' @return Character vector of variable names
#' @keywords internal
#' @noRd
get_coded_variables <- function(obj) {
  setdiff(names(obj), ".id")
}


#' Compute reliability statistic (deprecated, kept for backward compatibility)
#'
#' @param ratings Matrix where rows are subjects and columns are raters
#' @param measure Reliability measure
#' @param level Measurement level
#' @param tolerance Tolerance for agreement
#'
#' @return List with value and detail
#' @keywords internal
#' @noRd
compute_reliability <- function(ratings, measure, level, tolerance) {

  if (measure == "alpha") {
    # Krippendorff's alpha - needs raters as rows
    # Convert to numeric for irr package
    ratings_numeric <- convert_to_numeric(ratings)
    ratings_t <- t(ratings_numeric)

    irr_result <- tryCatch({
      irr::kripp.alpha(ratings_t, method = level)
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to compute Krippendorff's alpha.",
        "x" = conditionMessage(e)
      ))
    })

    list(value = irr_result$value, detail = irr_result)

  } else if (measure == "kappa") {
    # Choose kappa based on number of raters
    # Convert to numeric for irr package
    ratings_numeric <- convert_to_numeric(ratings)
    n_raters <- ncol(ratings_numeric)

    if (n_raters == 2) {
      # Cohen's kappa for 2 raters
      irr_result <- tryCatch({
        irr::kappa2(ratings_numeric)
      }, error = function(e) {
        cli::cli_abort(c(
          "Failed to compute Cohen's kappa.",
          "x" = conditionMessage(e)
        ))
      })
    } else {
      # Fleiss' kappa for 3+ raters
      irr_result <- tryCatch({
        irr::kappam.fleiss(ratings_numeric)
      }, error = function(e) {
        cli::cli_abort(c(
          "Failed to compute Fleiss' kappa.",
          "x" = conditionMessage(e)
        ))
      })
    }

    list(value = irr_result$value, detail = irr_result)

  } else if (measure == "kendall") {
    # Kendall's W
    irr_result <- tryCatch({
      irr::kendall(ratings)
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to compute Kendall's W.",
        "x" = conditionMessage(e)
      ))
    })

    list(value = irr_result$value, detail = irr_result)

  } else if (measure == "agreement") {
    # Simple percent agreement
    # For each subject, check if all raters agree (within tolerance)
    agrees <- apply(ratings, 1, function(row) {
      if (is.numeric(row)) {
        max(row) - min(row) <= tolerance
      } else {
        length(unique(as.character(row))) == 1
      }
    })

    pct_agree <- mean(agrees)

    list(
      value = pct_agree,
      detail = list(
        subjects = nrow(ratings),
        raters = ncol(ratings),
        agreement = pct_agree,
        method = "percent agreement"
      )
    )

  } else {
    cli::cli_abort("Unknown measure: {.val {measure}}")
  }
}


#' Print a qlm_comparison object
#'
#' @param x A qlm_comparison object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#' @keywords internal
#' @export
print.qlm_comparison <- function(x, ...) {
  cli::cli_h2("Inter-rater reliability")

  # Metadata from attributes
  cli::cli_text("Subjects: {attr(x, 'n')}")
  cli::cli_text("Raters:   {attr(x, 'raters')}")
  cli::cli_text("")

  # Group by variable
  variables <- unique(x$variable)

  for (var in variables) {
    var_data <- x[x$variable == var, ]
    level <- unique(var_data$level)

    cli::cli_h3("{var} ({level})")

    # Print each measure
    for (i in seq_len(nrow(var_data))) {
      measure <- var_data$measure[i]
      value <- var_data$value[i]

      # Format measure name for display
      measure_label <- format_measure_name(measure)

      # Check if we have CI columns
      has_ci <- "ci_lower" %in% names(var_data)

      if (has_ci && !is.na(var_data$ci_lower[i])) {
        # Print with CI
        cli::cli_text("  {measure_label}: {sprintf('%.4f', value)} [{sprintf('%.4f', var_data$ci_lower[i])}, {sprintf('%.4f', var_data$ci_upper[i])}]")
      } else {
        # Print without CI
        cli::cli_text("  {measure_label}: {sprintf('%.4f', value)}")
      }
    }

    cli::cli_text("")
  }

  invisible(x)
}


#' Format measure name for display
#'
#' @param measure Character string
#' @return Formatted string
#' @keywords internal
#' @noRd
format_measure_name <- function(measure) {
  # Map internal measure names to display names
  name_map <- c(
    "alpha_nominal" = "Krippendorff's alpha",
    "alpha_ordinal" = "Krippendorff's alpha",
    "alpha_interval" = "Krippendorff's alpha",
    "alpha_ratio" = "Krippendorff's alpha",
    "kappa" = "Kappa",
    "kappa_type" = "Kappa type",
    "kappa_weighted" = "Weighted kappa",
    "percent_agreement" = "Percent agreement",
    "w" = "Kendall's W",
    "rho" = "Spearman's rho",
    "icc" = "ICC",
    "r" = "Pearson's r"
  )

  # Return mapped name or original if not found
  name_map[measure] %||% measure
}
