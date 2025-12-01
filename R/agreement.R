#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @importFrom irr kripp.alpha kappam.fleiss kappa2
#' @importFrom stats na.omit
NULL

# Declare global variables for dplyr NSE
utils::globalVariables(c("unit_id", "coder_id", "code"))

# -------------------------------
# Internals for agreement()
# -------------------------------

#' @noRd
`%||%` <- function(x, y) if (length(x) == 0) y else x

#' @noRd
make_long_icr <- function(df, unit_id_col, coder_cols) {
  stopifnot(unit_id_col %in% names(df), all(coder_cols %in% names(df)))
  df %>%
    dplyr::mutate(unit_id = as.character(.data[[unit_id_col]])) %>%
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
compute_icr_summary <- function(long_df) {
  wide <- long_df %>%
    tidyr::pivot_wider(names_from = coder_id, values_from = code) %>%
    dplyr::arrange(.data$unit_id)

  if (!"unit_id" %in% names(wide)) {
    return(data.frame(
      metric = character(),
      value  = character(),
      stringsAsFactors = FALSE
    ))
  }

  ratings_raw <- wide %>% dplyr::select(-.data$unit_id)
  n_units  <- nrow(ratings_raw)
  n_coders <- ncol(ratings_raw)

  if (n_units == 0L || n_coders < 2L) {
    return(data.frame(
      metric = c(
        "units_included", "coders", "categories",
        "percent_unanimous_units",
        "mean_pairwise_percent_agreement",
        "mean_pairwise_cohens_kappa",
        "kripp_alpha_nominal", "fleiss_kappa"
      ),
      value = c(n_units, n_coders, NA, NA, NA, NA, NA, NA),
      stringsAsFactors = FALSE
    ))
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

  data.frame(
    metric = c(
      "units_included", "coders", "categories",
      "percent_unanimous_units",
      "mean_pairwise_percent_agreement",
      "mean_pairwise_cohens_kappa",
      "kripp_alpha_nominal", "fleiss_kappa"
    ),
    value = c(
      n_units, n_coders, length(all_levels),
      round(unanim, 4),
      round(mean_pw_agree, 4),
      round(mean_pw_kappa, 4),
      round(alpha_val, 4),
      round(fleiss_val, 4)
    ),
    stringsAsFactors = FALSE
  )
}

# -------------------------------
# User-facing API
# -------------------------------

#' Compute intercoder reliability statistics
#'
#' This function computes a set of intercoder reliability statistics for
#' nominal coding data with multiple coders:
#' Krippendorff's alpha (nominal), Fleiss' kappa, mean pairwise Cohen's kappa,
#' mean pairwise percent agreement, share of unanimous units, and basic counts.
#'
#' @param data A data frame containing the unit identifier and coder columns.
#' @param unit_id_col Character scalar. Name of the column identifying units
#'   (e.g. document ID, paragraph ID).
#' @param coder_cols Character vector. Names of columns containing the
#'   coders' codes (each column = one coder).
#' @param min_coders Integer: minimum number of non-missing coders per unit
#'   for that unit to be included. Default is 2.
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{metric}{Name of the statistic}
#'     \item{value}{Estimated value}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' agreement(
#'   data = my_df,
#'   unit_id_col = "doc_id",
#'   coder_cols  = c("coder1", "coder2", "coder3")
#' )
#' }
agreement <- function(data,
                      unit_id_col,
                      coder_cols,
                      min_coders = 2L) {
  stopifnot(is.data.frame(data))
  if (!unit_id_col %in% names(data)) {
    stop("unit_id_col '", unit_id_col, "' not found in data.")
  }
  if (!all(coder_cols %in% names(data))) {
    missing <- setdiff(coder_cols, names(data))
    stop("The following coder_cols are not in data: ",
         paste(missing, collapse = ", "))
  }
  if (length(coder_cols) < 2L) {
    stop("You must provide at least two coder columns.")
  }

  long_data     <- make_long_icr(data, unit_id_col = unit_id_col, coder_cols = coder_cols)
  long_filtered <- filter_units_by_coders(long_data, min_coders = min_coders)

  if (nrow(long_filtered) == 0L) {
    return(data.frame(
      metric = "message",
      value  = "No units have at least `min_coders` non-missing coders.",
      stringsAsFactors = FALSE
    ))
  }

  compute_icr_summary(long_filtered)
}
