# Declare global variables used in yardstick functions to avoid R CMD check NOTEs
utils::globalVariables(c("truth", "estimate"))


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


#' Compute bootstrap confidence intervals for validation metrics
#'
#' @param merged Data frame with 'estimate' and 'truth' columns
#' @param var_level Measurement level ("nominal", "ordinal", or "interval")
#' @param metrics_to_compute Character vector of metric names to compute
#' @param estimator Yardstick estimator (for nominal data)
#' @param bootstrap_n Number of bootstrap resamples
#' @return Named list of CIs, each with 'lower' and 'upper' elements
#' @keywords internal
#' @noRd
bootstrap_validation_ci <- function(merged, var_level, metrics_to_compute, estimator, bootstrap_n) {
  n_units <- nrow(merged)
  bootstrap_results <- list()

  # Run bootstrap resampling
  for (b in seq_len(bootstrap_n)) {
    # Resample units with replacement
    sample_idx <- sample(seq_len(n_units), size = n_units, replace = TRUE)
    boot_merged <- merged[sample_idx, ]

    # Ensure factor levels are preserved
    boot_merged$estimate <- factor(boot_merged$estimate, levels = levels(merged$estimate))
    boot_merged$truth <- factor(boot_merged$truth, levels = levels(merged$truth))

    # Compute all metrics on this bootstrap sample
    if (var_level == "nominal") {
      # Accuracy
      if ("accuracy" %in% metrics_to_compute) {
        acc <- yardstick::accuracy(boot_merged, truth = truth, estimate = estimate)
        bootstrap_results$accuracy <- c(bootstrap_results$accuracy, acc$.estimate)
      }

      # Precision
      if ("precision" %in% metrics_to_compute) {
        prec <- yardstick::precision(boot_merged, truth = truth, estimate = estimate, estimator = estimator)
        bootstrap_results$precision <- c(bootstrap_results$precision, prec$.estimate)
      }

      # Recall
      if ("recall" %in% metrics_to_compute) {
        rec <- yardstick::recall(boot_merged, truth = truth, estimate = estimate, estimator = estimator)
        bootstrap_results$recall <- c(bootstrap_results$recall, rec$.estimate)
      }

      # F1
      if ("f1" %in% metrics_to_compute) {
        f1 <- yardstick::f_meas(boot_merged, truth = truth, estimate = estimate, estimator = estimator)
        bootstrap_results$f1 <- c(bootstrap_results$f1, f1$.estimate)
      }

      # Kappa
      if ("kappa" %in% metrics_to_compute) {
        kap <- tryCatch({
          yardstick::kap(boot_merged, truth = truth, estimate = estimate, weighting = "none")$.estimate
        }, error = function(e) NA_real_)
        bootstrap_results$kappa <- c(bootstrap_results$kappa, kap)
      }

    } else if (var_level == "ordinal") {
      # Convert to numeric for ordinal measures
      estimate_num <- as.numeric(boot_merged$estimate)
      truth_num <- as.numeric(boot_merged$truth)

      # Spearman's rho
      if ("rho" %in% metrics_to_compute) {
        rho <- tryCatch({
          stats::cor(truth_num, estimate_num, method = "spearman", use = "complete.obs")
        }, error = function(e) NA_real_)
        bootstrap_results$rho <- c(bootstrap_results$rho, rho)
      }

      # Kendall's tau
      if ("tau" %in% metrics_to_compute) {
        tau <- tryCatch({
          stats::cor(truth_num, estimate_num, method = "kendall", use = "complete.obs")
        }, error = function(e) NA_real_)
        bootstrap_results$tau <- c(bootstrap_results$tau, tau)
      }

      # MAE
      if ("mae" %in% metrics_to_compute) {
        mae <- mean(abs(estimate_num - truth_num), na.rm = TRUE)
        bootstrap_results$mae <- c(bootstrap_results$mae, mae)
      }

    } else if (var_level == "interval") {
      # Convert to numeric for interval measures
      estimate_num <- as.numeric(as.character(boot_merged$estimate))
      truth_num <- as.numeric(as.character(boot_merged$truth))

      # Create data frame for yardstick
      numeric_data <- data.frame(truth = truth_num, estimate = estimate_num)

      # Pearson's r
      if ("r" %in% metrics_to_compute) {
        r <- tryCatch({
          stats::cor(truth_num, estimate_num, method = "pearson", use = "complete.obs")
        }, error = function(e) NA_real_)
        bootstrap_results$r <- c(bootstrap_results$r, r)
      }

      # MAE
      if ("mae" %in% metrics_to_compute) {
        mae_result <- tryCatch({
          yardstick::mae(numeric_data, truth = truth, estimate = estimate)$.estimate
        }, error = function(e) NA_real_)
        bootstrap_results$mae <- c(bootstrap_results$mae, mae_result)
      }

      # RMSE
      if ("rmse" %in% metrics_to_compute) {
        rmse_result <- tryCatch({
          yardstick::rmse(numeric_data, truth = truth, estimate = estimate)$.estimate
        }, error = function(e) NA_real_)
        bootstrap_results$rmse <- c(bootstrap_results$rmse, rmse_result)
      }

      # ICC
      if ("icc" %in% metrics_to_compute && requireNamespace("irr", quietly = TRUE)) {
        icc_result <- tryCatch({
          icc_data <- data.frame(truth = truth_num, estimate = estimate_num)
          irr::icc(icc_data, model = "twoway", type = "agreement", unit = "single")$value
        }, error = function(e) NA_real_)
        bootstrap_results$icc <- c(bootstrap_results$icc, icc_result)
      }
    }
  }

  # Compute percentile CIs from bootstrap distributions
  cis <- list()
  for (metric_name in names(bootstrap_results)) {
    values <- bootstrap_results[[metric_name]][!is.na(bootstrap_results[[metric_name]])]
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


#' Validate coded results against a gold standard
#'
#' Validates LLM-coded results from one or more `qlm_coded` objects against a
#' gold standard (typically human annotations) using appropriate metrics based
#' on measurement level. For nominal data, computes accuracy, precision, recall,
#' F1-score, and Cohen's kappa. For ordinal data, computes accuracy and weighted
#' kappa (linear weighting), which accounts for the ordering and distance between
#' categories.
#'
#' @param ... One or more data frames, `qlm_coded`, or `as_qlm_coded` objects
#'   containing predictions to validate. Must include a `.id` column and the
#'   variable(s) specified in `by`. Plain data frames are automatically converted
#'   to `as_qlm_coded` objects. Multiple objects will be validated separately
#'   against the same gold standard, and results combined with a `rater` column
#'   to distinguish them.
#' @param gold A data frame, `qlm_coded`, or object created with [as_qlm_coded()]
#'   containing gold standard annotations. Must include a `.id` column for joining
#'   with objects in `...` and the variable(s) specified in `by`. Plain data frames
#'   are automatically converted. **Optional** when using objects marked with
#'   `as_qlm_coded(data, is_gold = TRUE)` - these are auto-detected.
#' @param by Optional. Name of the variable(s) to validate (supports both quoted
#'   and unquoted). If `NULL` (default), all coded variables are validated. Can
#'   be a single variable (`by = sentiment`), a character vector
#'   (`by = c("sentiment", "rating")`), or NULL to process all variables.
#' @param level Optional. Measurement level(s) for the variable(s). Can be:
#'   \itemize{
#'     \item `NULL` (default): Auto-detect from codebook
#'     \item Character scalar: Use same level for all variables
#'     \item Named list: Specify level for each variable
#'   }
#'   Valid levels are `"nominal"`, `"ordinal"`, or `"interval"`.
#' @param average Character scalar. Averaging method for multiclass metrics
#'   (nominal level only):
#'   \describe{
#'     \item{`"macro"`}{Unweighted mean across classes (default)}
#'     \item{`"micro"`}{Aggregate contributions globally (sum TP, FP, FN)}
#'     \item{`"weighted"`}{Weighted mean by class prevalence}
#'     \item{`"none"`}{Return per-class metrics in addition to global metrics}
#'   }
#' @param ci Confidence interval method:
#'   \describe{
#'     \item{`"none"`}{No confidence intervals (default)}
#'     \item{`"analytic"`}{Analytic CIs where available (ICC, Pearson's r)}
#'     \item{`"bootstrap"`}{Bootstrap CIs for all metrics via resampling}
#'   }
#' @param bootstrap_n Number of bootstrap resamples when `ci = "bootstrap"`.
#'   Default is 1000. Ignored when `ci` is `"none"` or `"analytic"`.
#' @return A `qlm_validation` object (a tibble/data frame) with the following columns:
#'   \describe{
#'     \item{`variable`}{Name of the validated variable}
#'     \item{`level`}{Measurement level used}
#'     \item{`measure`}{Name of the validation metric}
#'     \item{`value`}{Computed value of the metric}
#'     \item{`class`}{For nominal data: averaging method used (e.g., "macro", "micro",
#'       "weighted") or class label (when `average = "none"`). For ordinal/interval
#'       data: NA (averaging not applicable).}
#'     \item{`rater`}{Name of the object being validated (from input names)}
#'     \item{`ci_lower`}{Lower bound of confidence interval (only if `ci != "none"`)}
#'     \item{`ci_upper`}{Upper bound of confidence interval (only if `ci != "none"`)}
#'   }
#'   The object has class `c("qlm_validation", "tbl_df", "tbl", "data.frame")` and
#'   attributes containing metadata (`n`, `call`).
#'
#'   **Metrics computed by measurement level:**
#'   \itemize{
#'     \item **Nominal:** accuracy, precision, recall, f1, kappa
#'     \item **Ordinal:** rho (Spearman's), tau (Kendall's), mae
#'     \item **Interval:** icc, r (Pearson's), mae, rmse
#'   }
#'
#'   **Confidence intervals:**
#'   - `ci = "analytic"`: Provides analytic CIs for ICC and Pearson's r only
#'   - `ci = "bootstrap"`: Provides bootstrap CIs for all metrics via resampling
#'
#' @details
#' The function performs an inner join between `x` and `gold` using the `.id`
#' column, so only units present in both datasets are included in validation.
#' Missing values (NA) in either predictions or gold standard are excluded with
#' a warning.
#'
#' **Measurement levels:**
#' - **Nominal**: Categories with no inherent ordering (e.g., topics, sentiment
#'   polarity). Metrics: accuracy, precision, recall, F1-score, Cohen's kappa
#'   (unweighted).
#' - **Ordinal**: Categories with meaningful ordering but unequal intervals
#'   (e.g., ratings 1-5, Likert scales). Metrics: Spearman's rho (`rho`, rank
#'   correlation), Kendall's tau (`tau`, rank correlation), and MAE (`mae`, mean
#'   absolute error). These measures account for the ordering of categories
#'   without assuming equal intervals.
#' - **Interval/Ratio**: Numeric data with equal intervals (e.g., counts,
#'   continuous measurements). Metrics: ICC (intraclass correlation), Pearson's r
#'   (linear correlation), MAE (mean absolute error), and RMSE (root mean squared
#'   error).
#'
#' For multiclass problems with nominal data, the `average` parameter controls
#' how per-class metrics are aggregated:
#' - **Macro averaging** computes metrics for each class independently and takes
#'   the unweighted mean. This treats all classes equally regardless of size.
#' - **Micro averaging** aggregates all true positives, false positives, and
#'   false negatives globally before computing metrics. This weights classes by
#'   their prevalence.
#' - **Weighted averaging** computes metrics for each class and takes the mean
#'   weighted by class size.
#' - **No averaging** (`average = "none"`) returns global macro-averaged metrics
#'   plus per-class breakdown.
#'
#' Note: The `average` parameter only affects precision, recall, and F1 for
#' nominal data. For ordinal data, these metrics are not computed.
#'
#' @seealso
#' [qlm_compare()] for inter-rater reliability between coded objects,
#' [qlm_code()] for LLM coding, [as_qlm_coded()] for converting human-coded data,
#' [yardstick::accuracy()], [yardstick::precision()], [yardstick::recall()],
#' [yardstick::f_meas()], [yardstick::kap()], [yardstick::conf_mat()]
#'
#' @examples
#' # Load example coded objects
#' examples <- readRDS(system.file("extdata", "example_objects.rds", package = "quallmer"))
#'
#' # Validate against gold standard (auto-detected)
#' validation <- qlm_validate(
#'   examples$example_coded_mini,
#'   examples$example_gold_standard,
#'   by = "sentiment",
#'   level = "nominal"
#' )
#' print(validation)
#'
#' # Explicit gold parameter (backward compatible)
#' validation2 <- qlm_validate(
#'   examples$example_coded_mini,
#'   gold = examples$example_gold_standard,
#'   by = "sentiment",
#'   level = "nominal"
#' )
#' print(validation2)
#'
#' @export
qlm_validate <- function(
    ...,
    gold,
    by,
    level = NULL,
    average = c("macro", "micro", "weighted", "none"),
    ci = c("none", "analytic", "bootstrap"),
    bootstrap_n = 1000
) {

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

  # Check if average was explicitly provided before match.arg() assigns default
  average_was_supplied <- !missing(average)
  average <- match.arg(average)

  # Capture objects to validate
  x_list <- list(...)

  # Check for named arguments in ... (likely misspelled parameters)
  if (!is.null(names(x_list)) && any(names(x_list) != "")) {
    bad_names <- names(x_list)[names(x_list) != ""]
    suggestions <- c()
    if ("use_ci" %in% bad_names) {
      suggestions <- c(suggestions, "Did you mean {.code ci = } instead of {.code use_ci = }?")
    }
    cli::cli_abort(c(
      "{cli::qty(length(bad_names))} Unknown parameter{?s} passed to {.fn qlm_validate}: {.arg {bad_names}}",
      if (length(suggestions) > 0) suggestions,
      "i" = "Named parameters must come after {.arg gold}.",
      "i" = "Example: {.code qlm_validate(coded1, gold = gold, by = 'var', ci = 'bootstrap')}"
    ))
  }

  # Auto-detect gold standard if not explicitly provided
  if (missing(gold)) {
    if (length(x_list) == 0) {
      cli::cli_abort(c(
        "Both validation object(s) and {.arg gold} standard are required.",
        "i" = "Usage: {.code qlm_validate(coded, gold = gold_standard, by = 'variable', level = 'nominal')}",
        "i" = "Or mark gold standard with {.code as_qlm_coded(data, is_gold = TRUE)}"
      ))
    }

    # Check for objects marked as gold standards
    is_gold <- vapply(x_list, function(obj) {
      obj <- upgrade_meta(obj)
      meta_attr <- attr(obj, "meta")
      !is.null(meta_attr) && !is.null(meta_attr$object$is_gold) && isTRUE(meta_attr$object$is_gold)
    }, logical(1))

    if (sum(is_gold) == 1) {
      # Found exactly one gold standard - use it automatically
      gold <- x_list[[which(is_gold)]]
      x_list <- x_list[!is_gold]
    } else if (sum(is_gold) > 1) {
      # Multiple gold standards found - ambiguous
      cli::cli_abort(c(
        "{sum(is_gold)} objects are marked as gold standards.",
        "x" = "Cannot automatically determine which one to use.",
        "i" = "Use {.code gold = } to explicitly specify the gold standard.",
        "i" = "Example: {.code qlm_validate(coded1, coded2, gold = gold_standard, by = 'var')}"
      ))
    } else {
      # No gold standards found - helpful error
      last_is_df <- is.data.frame(x_list[[length(x_list)]])
      cli::cli_abort(c(
        "The {.arg gold} parameter is required but was not provided.",
        "i" = if (last_is_df && length(x_list) > 1) {
          "Did you forget {.code gold = } before the gold standard argument?"
        } else {
          "Use named parameter: {.code gold = your_gold_standard}"
        },
        "i" = "Or mark gold standard with {.code as_qlm_coded(data, is_gold = TRUE)}",
        "i" = "Example: {.code qlm_validate(coded1, coded2, gold = gold_standard, by = 'var')}"
      ))
    }
  }

  # Validate that at least one object was provided
  if (length(x_list) == 0) {
    cli::cli_abort(c(
      "At least one object must be provided to validate.",
      "i" = "Provide one or more {.cls qlm_coded} objects before the {.arg gold} parameter.",
      "i" = "Example: {.code qlm_validate(coded1, coded2, gold = gold_standard, by = 'var')}"
    ))
  }

  # Check all objects are data frames with helpful error messages
  not_df <- !vapply(x_list, is.data.frame, logical(1))
  if (any(not_df)) {
    # Check if any are strings (might be misspelled parameter names)
    not_df_types <- vapply(x_list[not_df], function(x) class(x)[1], character(1))

    if (any(not_df_types == "character" & lengths(x_list[not_df]) == 1)) {
      # Likely a misspelled parameter
      bad_params <- unlist(x_list[not_df][not_df_types == "character"])
      cli::cli_abort(c(
        "Invalid arguments in {.arg ...}.",
        "x" = "{cli::qty(sum(not_df))} Argument{?s} {which(not_df)} {?is/are} not data frame{?s}.",
        "i" = "{cli::qty(length(bad_params))} Possible misspelled parameter{?s}: {.val {bad_params}}",
        "i" = "Remember: parameter is {.code ci} not {.code use_ci}",
        "i" = "All arguments before {.arg gold} must be data frames or {.cls qlm_coded} objects."
      ))
    } else {
      cli::cli_abort(c(
        "All arguments in {.arg ...} must be data frames or {.cls qlm_coded} objects.",
        "x" = "{cli::qty(sum(not_df))} Argument{?s} {which(not_df)} {?is/are} {not_df_types[not_df]}, not data frame{?s}.",
        "i" = "Only provide data frames before the {.arg gold} parameter.",
        "i" = "Use named parameters for options: {.code ci = 'bootstrap'}, not {.code use_ci = 'bootstrap'}"
      ))
    }
  }

  # Validate gold standard
  if (!is.data.frame(gold)) {
    cli::cli_abort(c(
      "{.arg gold} must be a data frame or {.cls qlm_coded} object.",
      "i" = "Provide a data frame with a {.var .id} column and the variable to validate."
    ))
  }

  # Auto-convert plain data.frames in x_list to as_qlm_coded
  not_coded_x <- !vapply(x_list, inherits, logical(1), "qlm_coded")
  if (any(not_coded_x)) {
    cli::cli_inform(c(
      "i" = "Converting {sum(not_coded_x)} plain data frame{?s} to {.cls as_qlm_coded} object{?s}.",
      "i" = "Use {.fn as_qlm_coded} directly to provide coder names and metadata."
    ))
    for (i in which(not_coded_x)) {
      x_list[[i]] <- as_qlm_coded(
        x_list[[i]],
        name = paste0("auto_converted_", i)
      )
    }
  }

  # Auto-convert gold standard if needed
  if (!inherits(gold, "qlm_coded")) {
    cli::cli_inform(c(
      "i" = "Converting {.arg gold} to {.cls as_qlm_coded} object.",
      "i" = "Use {.fn as_qlm_coded} directly to provide coder names and metadata."
    ))
    gold <- as_qlm_coded(gold, name = "auto_converted_gold")
  }

  # Extract object names from metadata
  object_names <- vapply(x_list, function(obj) {
    name <- tryCatch(qlm_meta(obj, "name"), error = function(e) NULL)
    if (!is.null(name)) name else NA_character_
  }, character(1))

  # Extract gold standard name
  gold_name <- tryCatch(qlm_meta(gold, "name"), error = function(e) NA_character_)

  # Validate .id columns
  for (i in seq_along(x_list)) {
    if (!".id" %in% names(x_list[[i]])) {
      cli::cli_abort(c(
        "Object {i} in {.arg ...} must contain a {.var .id} column.",
        "i" = "This should be created automatically by {.fn qlm_code}."
      ))
    }
  }

  if (!".id" %in% names(gold)) {
    cli::cli_abort(c(
      "{.arg gold} must contain a {.var .id} column for joining.",
      "i" = "Add a {.var .id} column matching the identifiers in validation objects."
    ))
  }

  # Determine which variables to process from first object
  if (is.null(by)) {
    # Extract all coded variables from first object
    by <- get_coded_variables(x_list[[1]])
    if (length(by) == 0) {
      cli::cli_abort(c(
        "No coded variables found in objects.",
        "i" = "Objects must have variables other than {.var .id}."
      ))
    }
  }

  # Check that 'by' variables exist in all objects and gold
  for (var in by) {
    all_objs <- c(x_list, list(gold = gold))
    names(all_objs)[seq_along(x_list)] <- paste0("object", seq_along(x_list))
    validate_by_variable(var, all_objs)
  }

  # Extract codebook from first qlm_coded object
  codebook <- NULL
  for (obj in x_list) {
    codebook <- extract_codebook_from_coded(obj)
    if (!is.null(codebook)) {
      break
    }
  }

  # Determine levels for each variable (same logic as qlm_compare)
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
    level <- match.arg(level, c("nominal", "ordinal", "interval"))
    variable_levels <- stats::setNames(rep(level, length(by)), by)
  } else if (is.list(level) || (is.character(level) && !is.null(names(level)))) {
    # Named list/vector of levels
    # Validate that all specified variables are in 'by'
    unknown_vars <- setdiff(names(level), by)
    if (length(unknown_vars) > 0) {
      cli::cli_abort(c(
        "Variables in {.arg level} not found in {.arg by}: {.var {unknown_vars}}",
        "i" = "Variables to validate: {.val {by}}"
      ))
    }

    # Validate level values
    valid_levels <- c("nominal", "ordinal", "interval")
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

  # Initialize results list
  all_results <- list()
  n_subjects_total <- NULL

  # Process each object in x_list
  for (obj_idx in seq_along(x_list)) {
    x_obj <- x_list[[obj_idx]]
    x_obj_name <- object_names[obj_idx]

    # Process each variable
    for (var in by) {
      var_level <- variable_levels[[var]]

      # Check if average was supplied for this iteration
      # (we only warn once per call, not per variable)
      if (var_level != "nominal" && average_was_supplied && obj_idx == 1 && var == by[1]) {
        cli::cli_warn(c(
          "The {.arg average} parameter only applies to nominal (multiclass) data.",
          "i" = "For {.val {var_level}} data, this parameter is ignored."
        ))
      }

      # Extract relevant columns
      x_data <- data.frame(
        .id = x_obj[[".id"]],
        estimate = x_obj[[var]],
        stringsAsFactors = FALSE
      )

      gold_data <- data.frame(
        .id = gold[[".id"]],
        truth = gold[[var]],
        stringsAsFactors = FALSE
      )

      # Inner join by .id
      merged <- merge(x_data, gold_data, by = ".id", all = FALSE, sort = TRUE)

    # Check for empty result
    if (nrow(merged) == 0) {
      cli::cli_warn(c(
        "No matching units found for variable {.var {var}}.",
        "i" = "Skipping this variable."
      ))
      next
    }

    # Check for NA values and warn
    na_estimate <- sum(is.na(merged$estimate))
    na_truth <- sum(is.na(merged$truth))

    if (na_estimate > 0 || na_truth > 0) {
      cli::cli_warn(c(
        "Missing values in variable {.var {var}} will be excluded:",
        "i" = "{na_estimate} missing value{?s} in predictions",
        "i" = "{na_truth} missing value{?s} in gold standard"
      ))
    }

    # Remove rows with any NA
    merged <- merged[stats::complete.cases(merged), ]

    # Check for remaining data
    if (nrow(merged) == 0) {
      cli::cli_warn(c(
        "No complete cases found for variable {.var {var}} after removing missing values.",
        "i" = "Skipping this variable."
      ))
      next
    }

    # Store n_subjects for first variable (for metadata)
    if (is.null(n_subjects_total)) {
      n_subjects_total <- nrow(merged)
    }

    # Get unique levels from both columns
    all_levels <- sort(unique(c(
      as.character(merged$estimate),
      as.character(merged$truth)
    )))

    # Convert both to factors with shared levels
    # For ordinal data, use ordered factors for proper weighted kappa
    if (var_level == "ordinal") {
      merged$estimate <- factor(merged$estimate, levels = all_levels, ordered = TRUE)
      merged$truth <- factor(merged$truth, levels = all_levels, ordered = TRUE)
    } else {
      merged$estimate <- factor(merged$estimate, levels = all_levels)
      merged$truth <- factor(merged$truth, levels = all_levels)
    }

    # Map average to yardstick estimator
    estimator <- switch(average,
      "macro" = "macro",
      "micro" = "micro",
      "weighted" = "macro_weighted",
      "none" = "macro"  # Use macro for global metrics when average = "none"
    )

    # Determine which metrics to compute based on level
    if (var_level == "nominal") {
      metrics_to_compute <- c("accuracy", "precision", "recall", "f1", "kappa")
    } else if (var_level == "ordinal") {
      metrics_to_compute <- c("rho", "tau", "mae")
    } else if (var_level == "interval") {
      metrics_to_compute <- c("icc", "r", "mae", "rmse")
    }

    # Initialize results list for this variable
    results <- list()
    cis <- list()  # Store confidence intervals

    # Compute bootstrap CIs if requested
    if (ci == "bootstrap") {
      cis <- bootstrap_validation_ci(merged, var_level, metrics_to_compute, estimator, bootstrap_n)
    }

    # Compute accuracy (no estimator parameter)
    if ("accuracy" %in% metrics_to_compute) {
      acc <- yardstick::accuracy(merged, truth = truth, estimate = estimate)
      results$accuracy <- acc$.estimate
    }

    # Compute precision
    if ("precision" %in% metrics_to_compute) {
      prec <- yardstick::precision(merged, truth = truth, estimate = estimate,
                                    estimator = estimator)
      results$precision <- prec$.estimate
    }

    # Compute recall
    if ("recall" %in% metrics_to_compute) {
      rec <- yardstick::recall(merged, truth = truth, estimate = estimate,
                               estimator = estimator)
      results$recall <- rec$.estimate
    }

    # Compute F1
    if ("f1" %in% metrics_to_compute) {
      f1 <- yardstick::f_meas(merged, truth = truth, estimate = estimate,
                              estimator = estimator)
      results$f1 <- f1$.estimate
    }

    # Compute kappa (only for nominal data)
    if ("kappa" %in% metrics_to_compute) {
      kap <- yardstick::kap(merged, truth = truth, estimate = estimate,
                            weighting = "none")
      results$kappa <- kap$.estimate
    }

    # Ordinal measures (require numeric conversion)
    if (var_level == "ordinal") {
      # Convert ordered factors to numeric for correlation and distance measures
      estimate_num <- as.numeric(merged$estimate)
      truth_num <- as.numeric(merged$truth)

      # Spearman's rho (rank correlation)
      # Note: cor.test does not provide CIs for Spearman's rho
      if ("rho" %in% metrics_to_compute) {
        results$rho <- stats::cor(truth_num, estimate_num, method = "spearman")
        # No CI available for Spearman's rho from cor.test
      }

      # Kendall's tau (rank correlation)
      # Note: cor.test does not provide CIs for Kendall's tau
      if ("tau" %in% metrics_to_compute) {
        results$tau <- stats::cor(truth_num, estimate_num, method = "kendall")
        # No CI available for Kendall's tau from cor.test
      }

      # Mean Absolute Error
      if ("mae" %in% metrics_to_compute) {
        results$mae <- mean(abs(estimate_num - truth_num))
      }
    }

    # Interval measures (require numeric conversion)
    if (var_level == "interval") {
      # For interval data, convert to numeric
      estimate_num <- as.numeric(as.character(merged$estimate))
      truth_num <- as.numeric(as.character(merged$truth))

      # Create data frame for yardstick functions
      numeric_data <- data.frame(
        truth = truth_num,
        estimate = estimate_num
      )

      # Pearson's r (linear correlation)
      if ("r" %in% metrics_to_compute) {
        if (ci == "analytic") {
          r_result <- tryCatch({
            stats::cor.test(truth_num, estimate_num, method = "pearson")
          }, error = function(e) {
            list(estimate = NA_real_, conf.int = c(NA_real_, NA_real_))
          })
          results$r <- as.numeric(r_result$estimate)
          cis$r <- c(lower = as.numeric(r_result$conf.int[1]),
                     upper = as.numeric(r_result$conf.int[2]))
        } else {
          results$r <- stats::cor(truth_num, estimate_num, method = "pearson")
        }
      }

      # Mean Absolute Error (using yardstick)
      if ("mae" %in% metrics_to_compute) {
        mae_result <- yardstick::mae(numeric_data, truth = truth, estimate = estimate)
        results$mae <- mae_result$.estimate
      }

      # Root Mean Squared Error (using yardstick)
      if ("rmse" %in% metrics_to_compute) {
        rmse_result <- yardstick::rmse(numeric_data, truth = truth, estimate = estimate)
        results$rmse <- rmse_result$.estimate
      }

      # Intraclass Correlation Coefficient (using irr package)
      if ("icc" %in% metrics_to_compute) {
        if (requireNamespace("irr", quietly = TRUE)) {
          # ICC for two-rater agreement (model = "twoway", type = "agreement")
          icc_data <- data.frame(truth = truth_num, estimate = estimate_num)
          icc_result <- irr::icc(icc_data, model = "twoway", type = "agreement", unit = "single")
          results$icc <- icc_result$value
          if (ci == "analytic") {
            cis$icc <- c(lower = icc_result$lbound, upper = icc_result$ubound)
          }
        } else {
          cli::cli_warn(c(
            "Package {.pkg irr} is required for ICC computation but is not installed.",
            "i" = "Install it with: {.code install.packages('irr')}"
          ))
          results$icc <- NA_real_
          if (ci == "analytic") {
            cis$icc <- c(lower = NA_real_, upper = NA_real_)
          }
        }
      }
    }

    # Add global metrics to all_results
    for (measure_name in names(results)) {
      # Determine class column value:
      # - For nominal: show average method in angle brackets (unless average = "none", then NA for global metrics)
      # - For non-nominal: NA
      class_value <- if (var_level == "nominal") {
        if (average == "none") NA_character_ else paste0("<", average, ">")
      } else {
        NA_character_
      }

      result_row <- list(
        variable = var,
        level = var_level,
        measure = measure_name,
        value = results[[measure_name]],
        class = class_value,
        rater = x_obj_name
      )

      # Add CI columns if requested
      if (ci != "none") {
        if (!is.null(cis[[measure_name]])) {
          result_row$ci_lower <- as.numeric(cis[[measure_name]]["lower"])
          result_row$ci_upper <- as.numeric(cis[[measure_name]]["upper"])
        } else {
          result_row$ci_lower <- NA_real_
          result_row$ci_upper <- NA_real_
        }
      }

      all_results[[length(all_results) + 1]] <- result_row
    }

    # Compute per-class metrics if average = "none" and nominal data
    if (average == "none" && var_level == "nominal") {
      # Compute confusion matrix for this variable
      conf_mat <- yardstick::conf_mat(merged, truth = truth, estimate = estimate)

      # Extract confusion matrix table
      cm_table <- conf_mat$table
      classes <- rownames(cm_table)

      # Compute per-class metrics
      for (i in seq_along(classes)) {
        class_label <- classes[i]

        # Extract TP, FP, FN for this class
        TP <- cm_table[class_label, class_label]
        FP <- sum(cm_table[, class_label]) - TP
        FN <- sum(cm_table[class_label, ]) - TP

        # Compute precision
        if ("precision" %in% metrics_to_compute) {
          prec <- if (TP + FP == 0) NA_real_ else TP / (TP + FP)
          result_row <- list(
            variable = var,
            level = var_level,
            measure = "precision",
            value = prec,
            class = class_label,
            rater = x_obj_name
          )
          if (ci != "none") {
            result_row$ci_lower <- NA_real_
            result_row$ci_upper <- NA_real_
          }
          all_results[[length(all_results) + 1]] <- result_row
        }

        # Compute recall
        if ("recall" %in% metrics_to_compute) {
          rec <- if (TP + FN == 0) NA_real_ else TP / (TP + FN)
          result_row <- list(
            variable = var,
            level = var_level,
            measure = "recall",
            value = rec,
            class = class_label,
            rater = x_obj_name
          )
          if (ci != "none") {
            result_row$ci_lower <- NA_real_
            result_row$ci_upper <- NA_real_
          }
          all_results[[length(all_results) + 1]] <- result_row
        }

        # Compute F1
        if ("f1" %in% metrics_to_compute) {
          prec <- if (TP + FP == 0) NA_real_ else TP / (TP + FP)
          rec <- if (TP + FN == 0) NA_real_ else TP / (TP + FN)
          if (is.na(prec) || is.na(rec) || (prec + rec) == 0) {
            f1 <- NA_real_
          } else {
            f1 <- 2 * prec * rec / (prec + rec)
          }
          result_row <- list(
            variable = var,
            level = var_level,
            measure = "f1",
            value = f1,
            class = class_label,
            rater = x_obj_name
          )
          if (ci != "none") {
            result_row$ci_lower <- NA_real_
            result_row$ci_upper <- NA_real_
          }
          all_results[[length(all_results) + 1]] <- result_row
        }
      }
    }
    }  # End of variable loop
  }  # End of object loop

  # Check that we got some results
  if (length(all_results) == 0) {
    cli::cli_abort(c(
      "No valid validations could be computed.",
      "i" = "Check that objects have overlapping {.var .id} values and non-missing data."
    ))
  }

  # Build data frame using dplyr::bind_rows to handle mixed types better
  result_df <- dplyr::bind_rows(all_results)

  # Extract parent run names from all objects
  parent_names <- vapply(x_list, function(obj) {
    if (inherits(obj, "qlm_coded")) {
      name <- tryCatch(qlm_meta(obj, "name"), error = function(e) NULL)
      if (!is.null(name)) return(name)
    }
    NA_character_
  }, character(1))

  # Extract gold standard name
  gold_run_name <- NA_character_
  if (inherits(gold, "qlm_coded")) {
    gold_run_name <- tryCatch(qlm_meta(gold, "name"), error = function(e) NA_character_)
  }

  # Add attributes and class with new metadata structure
  structure(
    result_df,
    class = c("qlm_validation", class(result_df)),
    n = n_subjects_total,
    call = match.call(),
    meta = list(
      user = list(
        name = paste0("validation_", substr(digest::digest(list(parent_names, gold_run_name)), 1, 8)),
        notes = NULL
      ),
      object = list(
        call = match.call(),
        parent = c(parent_names[!is.na(parent_names)], gold_run_name[!is.na(gold_run_name)]),
        variables = by,
        average = average
      ),
      system = list(
        timestamp = Sys.time(),
        quallmer_version = tryCatch(as.character(utils::packageVersion("quallmer")), error = function(e) NA_character_),
        R_version = paste(R.version$major, R.version$minor, sep = ".")
      )
    )
  )
}


#' Print a qlm_validation object
#'
#' @param x A qlm_validation object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object.
#' @keywords internal
#' @export
print.qlm_validation <- function(x, ...) {
  cli::cli_h2("quallmer validation")

  # Metadata from attributes
  cli::cli_text("n: {attr(x, 'n')}")
  cli::cli_text("")

  # Group by variable
  variables <- unique(x$variable)

  for (var in variables) {
    var_data <- x[x$variable == var, ]
    level <- unique(var_data$level)

    # Check if we have per-class metrics
    has_class_metrics <- !all(is.na(var_data$class))

    if (has_class_metrics) {
      # Separate global and per-class metrics
      global_data <- var_data[is.na(var_data$class), ]
      class_data <- var_data[!is.na(var_data$class), ]

      cli::cli_h3("{var} ({level})")

      # Print global metrics
      if (nrow(global_data) > 0) {
        cli::cli_text("Global:")
        for (i in seq_len(nrow(global_data))) {
          measure <- global_data$measure[i]
          value <- global_data$value[i]
          measure_label <- format_validation_measure_name(measure)

          # Check if we have CI columns
          has_ci <- "ci_lower" %in% names(global_data)

          if (has_ci && !is.na(global_data$ci_lower[i])) {
            cli::cli_text("  {measure_label}: {sprintf('%.4f', value)} [{sprintf('%.4f', global_data$ci_lower[i])}, {sprintf('%.4f', global_data$ci_upper[i])}]")
          } else {
            cli::cli_text("  {measure_label}: {sprintf('%.4f', value)}")
          }
        }
        cli::cli_text("")
      }

      # Print per-class metrics
      if (nrow(class_data) > 0) {
        cli::cli_text("By class:")
        # Reshape to wide format for printing
        classes <- unique(class_data$class)
        measures <- unique(class_data$measure)

        for (class_label in classes) {
          cli::cli_text("  {class_label}:")
          class_metrics <- class_data[class_data$class == class_label, ]
          for (i in seq_len(nrow(class_metrics))) {
            measure <- class_metrics$measure[i]
            value <- class_metrics$value[i]
            measure_label <- format_validation_measure_name(measure)
            cli::cli_text("    {measure_label}: {sprintf('%.4f', value)}")
          }
        }
        cli::cli_text("")
      }
    } else {
      # No per-class metrics - print normally
      cli::cli_h3("{var} ({level})")

      for (i in seq_len(nrow(var_data))) {
        measure <- var_data$measure[i]
        value <- var_data$value[i]
        measure_label <- format_validation_measure_name(measure)

        # Check if we have CI columns
        has_ci <- "ci_lower" %in% names(var_data)

        if (has_ci && !is.na(var_data$ci_lower[i])) {
          cli::cli_text("  {measure_label}: {sprintf('%.4f', value)} [{sprintf('%.4f', var_data$ci_lower[i])}, {sprintf('%.4f', var_data$ci_upper[i])}]")
        } else {
          cli::cli_text("  {measure_label}: {sprintf('%.4f', value)}")
        }
      }

      cli::cli_text("")
    }
  }

  invisible(x)
}


#' Format validation measure name for display
#'
#' @param measure Character string
#' @return Formatted string
#' @keywords internal
#' @noRd
format_validation_measure_name <- function(measure) {
  # Map internal measure names to display names
  name_map <- c(
    "accuracy" = "accuracy",
    "precision" = "precision",
    "recall" = "recall",
    "f1" = "F1",
    "kappa" = "Cohen's kappa",
    "rho" = "Spearman's rho",
    "tau" = "Kendall's tau",
    "r" = "Pearson's r",
    "icc" = "ICC",
    "mae" = "MAE",
    "rmse" = "RMSE"
  )

  # Return mapped name or original if not found
  name_map[measure] %||% measure
}
