#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @importFrom irr kripp.alpha kappam.fleiss kappa2
#' @importFrom stats na.omit
NULL

# Declare global variables for dplyr NSE
utils::globalVariables(c("unit_id", "coder_id", "code"))

# -------------------------------
# Internals for validate()
# -------------------------------

#' @noRd
`%||%` <- function(x, y) if (length(x) == 0) y else x

#' @noRd
make_long_icr <- function(df, id, coder_cols) {
  stopifnot(id %in% names(df), all(coder_cols %in% names(df)))
  df %>%
    dplyr::mutate(unit_id = as.character(.data[[id]])) %>%
    dplyr::select(unit_id, dplyr::all_of(coder_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(coder_cols),
      names_to = "coder_id",
      values_to = "code"
    ) %>%
    dplyr::mutate(
      coder_id = as.character(.data$coder_id),
      code     = as.character(.data$code)
    ) %>%
    dplyr::group_by(.data$unit_id, .data$coder_id) %>%
    dplyr::summarise(
      code = dplyr::first(.data$code[!is.na(.data$code)] %||% NA_character_),
      .groups = "drop"
    ) %>%
    tidyr::complete(unit_id, coder_id, fill = list(code = NA_character_))
}

#' @noRd
filter_units_by_coders <- function(long_df, min_coders = 2L) {
  long_df %>%
    dplyr::group_by(.data$unit_id) %>%
    dplyr::filter(sum(!is.na(.data$code)) >= min_coders) %>%
    dplyr::ungroup()
}

#' @noRd
compute_icr_summary <- function(long_df, output = c("list", "data.frame")) {
  output <- match.arg(output)

  wide <- long_df %>%
    tidyr::pivot_wider(names_from = coder_id, values_from = code) %>%
    dplyr::arrange(.data$unit_id)

  metrics <- c(
    "units_included", "coders", "categories",
    "percent_unanimous_units",
    "mean_pairwise_percent_agreement",
    "mean_pairwise_cohens_kappa",
    "kripp_alpha_nominal", "fleiss_kappa"
  )

  if (!"unit_id" %in% names(wide)) {
    values <- c(0, 0, NA, NA, NA, NA, NA, NA)
    if (output == "data.frame") {
      return(data.frame(
        metric = metrics,
        value  = values,
        stringsAsFactors = FALSE
      ))
    } else {
      return(as.list(stats::setNames(values, metrics)))
    }
  }

  ratings_raw <- wide %>% dplyr::select(-.data$unit_id)
  n_units  <- nrow(ratings_raw)
  n_coders <- ncol(ratings_raw)

  if (n_units == 0L || n_coders < 2L) {
    values <- c(n_units, n_coders, NA, NA, NA, NA, NA, NA)
    if (output == "data.frame") {
      return(data.frame(
        metric = metrics,
        value  = values,
        stringsAsFactors = FALSE
      ))
    } else {
      return(as.list(stats::setNames(values, metrics)))
    }
  }

  # Factorize on common levels
  all_levels <- sort(unique(stats::na.omit(unlist(ratings_raw))))
  ratings_fac <- as.data.frame(
    lapply(ratings_raw, function(col) factor(col, levels = all_levels))
  )
  ratings_int <- as.data.frame(
    lapply(ratings_fac, function(col) as.integer(col))
  )

  # Krippendorff's alpha (nominal)
  alpha_val <- tryCatch({
    rmat <- t(as.matrix(ratings_int))
    irr::kripp.alpha(rmat, method = "nominal")$value
  }, error = function(e) NA_real_)

  # Fleiss' kappa (complete cases only)
  fleiss_val <- tryCatch({
    comp <- ratings_int[stats::complete.cases(ratings_int), , drop = FALSE]
    if (nrow(comp) >= 2L && length(all_levels) >= 2L) {
      irr::kappam.fleiss(comp)$value
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

  # Percent unanimous units (among units with >=2 non-NA codings)
  unanim <- tryCatch({
    nn <- apply(ratings_fac, 1L, function(r) sum(!is.na(r)))
    eligible <- which(nn >= 2L)
    if (length(eligible) == 0L) {
      NA_real_
    } else {
      ok <- vapply(eligible, function(i) {
        v <- ratings_fac[i, , drop = TRUE]
        v <- v[!is.na(v)]
        length(unique(v)) == 1L
      }, logical(1))
      mean(ok)
    }
  }, error = function(e) NA_real_)

  # Mean pairwise percent agreement and Cohen's kappa
  pw_cols  <- names(ratings_fac)
  pairs    <- utils::combn(pw_cols, 2L, simplify = FALSE)
  pw_agree <- numeric()
  pw_kappa <- numeric()

  for (pr in pairs) {
    a <- ratings_fac[[pr[1]]]
    b <- ratings_fac[[pr[2]]]
    keep <- !is.na(a) & !is.na(b)
    if (sum(keep) >= 2L) {
      pw_agree <- c(pw_agree, mean(as.character(a[keep]) == as.character(b[keep])))
      k2 <- tryCatch({
        irr::kappa2(data.frame(a = a[keep], b = b[keep]))$value
      }, error = function(e) NA_real_)
      pw_kappa <- c(pw_kappa, k2)
    }
  }

  mean_pw_agree <- if (length(pw_agree)) mean(pw_agree, na.rm = TRUE) else NA_real_
  mean_pw_kappa <- if (length(pw_kappa)) mean(pw_kappa, na.rm = TRUE) else NA_real_

  values <- c(
    n_units,
    n_coders,
    length(all_levels),
    round(unanim, 4),
    round(mean_pw_agree, 4),
    round(mean_pw_kappa, 4),
    round(alpha_val, 4),
    round(fleiss_val, 4)
  )

  if (output == "data.frame") {
    data.frame(
      metric = metrics,
      value  = values,
      stringsAsFactors = FALSE
    )
  } else {
    as.list(stats::setNames(values, metrics))
  }
}

#' @noRd
compute_gold_summary <- function(long_df, gold) {
  wide <- long_df %>%
    tidyr::pivot_wider(names_from = coder_id, values_from = code) %>%
    dplyr::arrange(.data$unit_id)

  if (!"unit_id" %in% names(wide)) {
    return(data.frame(
      coder_id        = character(),
      n               = integer(),
      accuracy        = double(),
      precision_macro = double(),
      recall_macro    = double(),
      f1_macro        = double(),
      stringsAsFactors = FALSE
    ))
  }

  if (!gold %in% names(wide)) {
    stop("Gold-standard coder '", gold, "' not found among coder columns.")
  }

  ratings_raw <- wide %>% dplyr::select(-.data$unit_id)

  # Drop units where gold is NA
  truth_all <- ratings_raw[[gold]]
  keep_rows <- !is.na(truth_all)
  ratings_raw <- ratings_raw[keep_rows, , drop = FALSE]
  truth_all   <- ratings_raw[[gold]]

  if (nrow(ratings_raw) == 0L) {
    return(data.frame(
      coder_id        = character(),
      n               = integer(),
      accuracy        = double(),
      precision_macro = double(),
      recall_macro    = double(),
      f1_macro        = double(),
      stringsAsFactors = FALSE
    ))
  }

  # Factorize on common levels across all remaining coders (including gold)
  all_levels <- sort(unique(stats::na.omit(unlist(ratings_raw))))
  ratings_fac <- as.data.frame(
    lapply(ratings_raw, function(col) factor(col, levels = all_levels))
  )

  coders <- setdiff(names(ratings_fac), gold)
  out_list <- list()

  for (cd in coders) {
    truth <- ratings_fac[[gold]]
    pred  <- ratings_fac[[cd]]

    keep <- !is.na(truth) & !is.na(pred)
    if (sum(keep) == 0L) {
      out_list[[cd]] <- data.frame(
        coder_id        = cd,
        n               = 0L,
        accuracy        = NA_real_,
        precision_macro = NA_real_,
        recall_macro    = NA_real_,
        f1_macro        = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    t_vec <- truth[keep]
    p_vec <- pred[keep]
    n_obs <- length(t_vec)

    # Accuracy
    acc <- mean(as.character(t_vec) == as.character(p_vec))

    # Confusion table for multi-class metrics
    tab <- table(t_vec, p_vec)

    precision_vals <- numeric()
    recall_vals    <- numeric()
    f1_vals        <- numeric()

    lvls <- rownames(tab)
    for (lv in lvls) {
      TP <- tab[lv, lv]
      FP <- sum(tab[, lv]) - TP
      FN <- sum(tab[lv, ]) - TP

      prec <- if ((TP + FP) == 0L) NA_real_ else TP / (TP + FP)
      rec  <- if ((TP + FN) == 0L) NA_real_ else TP / (TP + FN)
      f1   <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) {
        NA_real_
      } else {
        2 * prec * rec / (prec + rec)
      }

      precision_vals <- c(precision_vals, prec)
      recall_vals    <- c(recall_vals, rec)
      f1_vals        <- c(f1_vals, f1)
    }

    precision_macro <- if (length(precision_vals)) mean(precision_vals, na.rm = TRUE) else NA_real_
    recall_macro    <- if (length(recall_vals)) mean(recall_vals, na.rm = TRUE) else NA_real_
    f1_macro        <- if (length(f1_vals)) mean(f1_vals, na.rm = TRUE) else NA_real_

    out_list[[cd]] <- data.frame(
      coder_id        = cd,
      n               = n_obs,
      accuracy        = round(acc, 4),
      precision_macro = round(precision_macro, 4),
      recall_macro    = round(recall_macro, 4),
      f1_macro        = round(f1_macro, 4),
      stringsAsFactors = FALSE
    )
  }

  if (!length(out_list)) {
    return(data.frame(
      coder_id        = character(),
      n               = integer(),
      accuracy        = double(),
      precision_macro = double(),
      recall_macro    = double(),
      f1_macro        = double(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, out_list)
}

# -------------------------------
# User-facing API
# -------------------------------

#' Validate coding: intercoder reliability or gold-standard comparison
#'
#' This function validates nominal coding data with multiple coders in two ways:
#' \itemize{
#'   \item \code{mode = "icr"}: compute intercoder reliability statistics
#'     (Krippendorff's alpha (nominal), Fleiss' kappa, mean pairwise Cohen's
#'     kappa, mean pairwise percent agreement, share of unanimous units, and
#'     basic counts).
#'   \item \code{mode = "gold"}: treat one coder column as a gold standard
#'     (typically a human coder) and, for each other coder, compute accuracy,
#'     macro-averaged precision, recall, and F1.
#' }
#'
#' @param data A data frame containing the unit identifier and coder columns.
#' @param id Character scalar. Name of the column identifying units
#'   (e.g. document ID, paragraph ID).
#' @param coder_cols Character vector. Names of columns containing the
#'   coders' codes (each column = one coder).
#' @param min_coders Integer: minimum number of non-missing coders per unit
#'   for that unit to be included. Default is 2.
#' @param mode Character scalar: either \code{"icr"} for intercoder reliability
#'   statistics, or \code{"gold"} to compare coders against a gold-standard
#'   coder.
#' @param gold Character scalar: name of the gold-standard coder column
#'   (must be one of \code{coder_cols}) when \code{mode = "gold"}.
#' @param output Character scalar: either \code{"list"} (default) to return a
#'   named list of metrics when \code{mode = "icr"}, or \code{"data.frame"} to
#'   return a long data frame with columns \code{metric} and \code{value}.
#'   For \code{mode = "gold"}, the result is always a data frame.
#'
#' @return
#'   If \code{mode = "icr"}:
#'   \itemize{
#'     \item If \code{output = "list"} (default): a named list of scalar
#'       metrics (e.g. \code{res$fleiss_kappa}).
#'     \item If \code{output = "data.frame"}: a data frame with columns
#'       \code{metric} and \code{value}.
#'   }
#'
#'   If \code{mode = "gold"}: a data frame with one row per non-gold coder and
#'   columns:
#'   \describe{
#'     \item{coder_id}{Name of the coder column compared to the gold standard}
#'     \item{n}{Number of units with non-missing gold and coder codes}
#'     \item{accuracy}{Overall accuracy}
#'     \item{precision_macro}{Macro-averaged precision across categories}
#'     \item{recall_macro}{Macro-averaged recall across categories}
#'     \item{f1_macro}{Macro-averaged F1 score across categories}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Intercoder reliability (list output)
#' res_icr <- validate(
#'   data = my_df,
#'   id   = "doc_id",
#'   coder_cols  = c("coder1", "coder2", "coder3"),
#'   mode = "icr"
#' )
#' res_icr$fleiss_kappa
#'
#' # Intercoder reliability (data.frame output)
#' res_icr_df <- validate(
#'   data = my_df,
#'   id   = "doc_id",
#'   coder_cols  = c("coder1", "coder2", "coder3"),
#'   mode   = "icr",
#'   output = "data.frame"
#' )
#'
#' # Gold-standard validation, assuming coder1 is human gold standard
#' res_gold <- validate(
#'   data = my_df,
#'   id   = "doc_id",
#'   coder_cols  = c("coder1", "coder2", "llm1", "llm2"),
#'   mode = "gold",
#'   gold = "coder1"
#' )
#' }
validate <- function(data,
                     id,
                     coder_cols,
                     min_coders = 2L,
                     mode = c("icr", "gold"),
                     gold = NULL,
                     output = c("list", "data.frame")) {
  mode   <- match.arg(mode)
  output <- match.arg(output)

  stopifnot(is.data.frame(data))
  if (!id %in% names(data)) {
    stop("id '", id, "' not found in data.")
  }
  if (!all(coder_cols %in% names(data))) {
    missing <- setdiff(coder_cols, names(data))
    stop("The following coder_cols are not in data: ",
         paste(missing, collapse = ", "))
  }
  if (length(coder_cols) < 2L) {
    stop("You must provide at least two coder columns.")
  }

  if (mode == "gold") {
    if (is.null(gold)) {
      stop("When mode = 'gold', you must supply `gold` as the name of the gold-standard coder column.")
    }
    if (!gold %in% coder_cols) {
      stop("`gold` must be one of the coder_cols.")
    }
  }

  long_data     <- make_long_icr(data, id = id, coder_cols = coder_cols)
  long_filtered <- filter_units_by_coders(long_data, min_coders = min_coders)

  if (nrow(long_filtered) == 0L) {
    if (mode == "icr") {
      empty_df <- data.frame(
        metric = "message",
        value  = "No units have at least `min_coders` non-missing coders.",
        stringsAsFactors = FALSE
      )
      if (output == "data.frame") {
        return(empty_df)
      } else {
        return(list(message = empty_df$value))
      }
    } else {
      return(data.frame(
        coder_id        = character(),
        n               = integer(),
        accuracy        = double(),
        precision_macro = double(),
        recall_macro    = double(),
        f1_macro        = double(),
        stringsAsFactors = FALSE
      ))
    }
  }

  if (mode == "icr") {
    compute_icr_summary(long_filtered, output = output)
  } else {
    compute_gold_summary(long_filtered, gold = gold)
  }
}
