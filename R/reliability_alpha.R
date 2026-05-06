#' Krippendorff's alpha for predefined units
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of Krippendorff's alpha (`_c_alpha`) for the coding
#' of predefined units, following Krippendorff (2019, section 12.3).
#'
#' @param observations A `subjects x raters` (units x observers) matrix or
#'   data.frame. Rows are predefined units; columns are observers. Cells
#'   contain the value assigned by each observer to each unit (use `NA`
#'   for missing values).
#' @param method One of `"nominal"`, `"ordinal"`, `"interval"`, `"ratio"` --
#'   the metric (difference function) for `delta^2_ck`. See Krippendorff
#'   (2019, section 12.3.3).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`method`}{Character, e.g. `"alpha_nominal"`.}
#'     \item{`value`}{Numeric -- the overall alpha coefficient.}
#'     \item{`ci_lower`, `ci_upper`}{Numeric -- confidence interval bounds
#'       (always `NA` for alpha; included for uniform output across
#'       reliability functions).}
#'     \item{`per_value`}{For `method = "nominal"`: a data.frame with columns
#'       `value`, `alpha`, `n` giving per-category alpha (each category
#'       dichotomised against all others) and its marginal count `n.c`.
#'       `NULL` for ordered metrics.}
#'     \item{`n_observers`}{Number of observers (`m`).}
#'     \item{`n_units`}{Number of units with pairable values (`m_u >= 2`).}
#'     \item{`n_pairable`}{Total pairable values (`n..`).}
#'     \item{`coincidence`}{The values-by-values coincidence matrix `o_ck`.}
#'   }
#'
#' @references
#' Krippendorff, K. (2019). *Content Analysis: An Introduction to Its
#' Methodology* (4th ed.). Sage. \doi{10.4135/9781071878781}
#'
#' @keywords internal
reliability_alpha <- function(observations,
                              method = c("nominal", "ordinal", "interval", "ratio")) {
  method <- match.arg(method)

  if (is.data.frame(observations)) observations <- as.matrix(observations)
  if (!is.matrix(observations)) {
    cli::cli_abort("{.arg observations} must be a matrix or data.frame.")
  }
  if (ncol(observations) < 2L) {
    cli::cli_abort("At least two observers (columns) are required.")
  }

  # Internally use observers x units (the canonical form, Figure 12.3)
  x <- t(observations)
  m <- nrow(x)
  N_total <- ncol(x)

  values <- sort(unique(as.vector(x[!is.na(x)])))
  nv <- length(values)

  cm <- matrix(0.0, nrow = nv, ncol = nv,
               dimnames = list(as.character(values), as.character(values)))

  n_pairable <- 0L
  N_units    <- 0L

  for (u in seq_len(N_total)) {
    obs <- x[, u]
    present <- obs[!is.na(obs)]
    mu <- length(present)
    if (mu < 2L) next
    n_pairable <- n_pairable + mu
    N_units    <- N_units + 1L

    w <- 1.0 / (mu - 1L)
    for (ii in seq_len(mu - 1L)) {
      for (jj in seq.int(ii + 1L, mu)) {
        ci <- match(present[ii], values)
        ki <- match(present[jj], values)
        if (ci == ki) {
          cm[ci, ci] <- cm[ci, ci] + 2.0 * w
        } else {
          cm[ci, ki] <- cm[ci, ki] + w
          cm[ki, ci] <- cm[ki, ci] + w
        }
      }
    }
  }

  empty_result <- function(value) {
    list(
      method      = paste0("alpha_", method),
      value       = value,
      ci_lower    = NA_real_,
      ci_upper    = NA_real_,
      per_value   = NULL,
      n_observers = m,
      n_units     = N_units,
      n_pairable  = as.integer(n_pairable),
      coincidence = cm
    )
  }

  if (nv < 2L) {
    return(empty_result(if (n_pairable == 0L) NA_real_ else 1.0))
  }

  n_c     <- rowSums(cm)
  n_total <- sum(cm)
  vals_num <- suppressWarnings(as.numeric(values))

  d2_fun <- switch(method,
    nominal  = function(ci, ki) 1.0,
    ordinal  = function(ci, ki) (sum(n_c[ki:ci]) - (n_c[ki] + n_c[ci]) / 2.0)^2,
    interval = function(ci, ki) (vals_num[ci] - vals_num[ki])^2,
    ratio    = function(ci, ki) {
      s <- vals_num[ci] + vals_num[ki]
      if (s == 0.0) 0.0 else (vals_num[ci] - vals_num[ki])^2 / s^2
    }
  )

  D_o <- 0.0
  D_e <- 0.0
  for (ci in seq.int(2L, nv)) {
    for (ki in seq_len(ci - 1L)) {
      d2 <- d2_fun(ci, ki)
      D_o <- D_o + cm[ci, ki] * d2
      D_e <- D_e + n_c[ci] * n_c[ki] * d2
    }
  }
  alpha_value <- unname(if (D_e == 0.0) 1.0 else 1.0 - (n_total - 1.0) * D_o / D_e)

  per_value <- if (method == "nominal") {
    per_category_alpha(cm, n_c, n_total, values)
  } else {
    NULL
  }

  list(
    method      = paste0("alpha_", method),
    value       = alpha_value,
    ci_lower    = NA_real_,
    ci_upper    = NA_real_,
    per_value   = per_value,
    n_observers = m,
    n_units     = N_units,
    n_pairable  = as.integer(n_pairable),
    coincidence = cm
  )
}


# Per-category nominal alpha: dichotomise each category against all others
# (binary collapse of the coincidence matrix) and apply the binary form of
# alpha (Krippendorff 2019, eq. 16).
per_category_alpha <- function(cm, n_c, n_total, values) {
  out <- data.frame(
    value = as.character(values),
    alpha = NA_real_,
    n     = as.integer(unname(n_c)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(values)) {
    nc   <- n_c[i]
    n_nc <- n_total - nc
    o_cc <- cm[i, i]

    if (n_nc == 0 || nc == 0) {
      # Degenerate: all (or none) of the values are c. Perfect agreement on c.
      out$alpha[i] <- 1.0
      next
    }

    o_off <- nc - o_cc          # off-diagonal pairs involving c
    out$alpha[i] <- unname(1.0 - (n_total - 1.0) * o_off / (nc * n_nc))
  }

  out
}


#' Align segment texts to character positions in a source text
#'
#' `r lifecycle::badge("experimental")`
#'
#' Maps an ordered sequence of segment texts to their `(start, end)` character
#' positions within a source text. Characters between consecutive segments
#' (whitespace, newlines) become gaps.
#'
#' @param source_text Character string: the original unsegmented text.
#' @param segments Character vector of segment texts, in document order.
#'   Each must be a substring of `source_text`.
#'
#' @return A data.frame with columns `start` and `end` (1-based, inclusive
#'   character positions), one row per segment.
#'
#' @keywords internal
align_segments <- function(source_text, segments) {
  if (!is.character(source_text) || length(source_text) != 1L) {
    cli::cli_abort("{.arg source_text} must be a single character string.")
  }
  if (!is.character(segments) || length(segments) == 0L) {
    cli::cli_abort("{.arg segments} must be a non-empty character vector.")
  }

  starts <- integer(length(segments))
  ends   <- integer(length(segments))
  cursor <- 1L

  for (i in seq_along(segments)) {
    seg_trimmed <- trimws(segments[i])
    if (nchar(seg_trimmed) == 0L) {
      cli::cli_abort("Segment {i} is empty after trimming.")
    }

    remainder <- substring(source_text, cursor)
    pos <- regexpr(seg_trimmed, remainder, fixed = TRUE)

    if (pos == -1L) {
      words <- strsplit(seg_trimmed, "\\s+")[[1L]]
      escaped <- gsub("([\\\\^$.|?*+(){}\\[\\]])", "\\\\\\1", words)
      seg_pattern <- paste(escaped, collapse = "\\s+")
      pos <- regexpr(seg_pattern, remainder, perl = TRUE)
    }

    if (pos == -1L) {
      cli::cli_abort(c(
        "Segment {i} could not be found in the source text after position {cursor}.",
        "i" = "Segment text: {.val {substr(segments[i], 1, 80)}}"
      ))
    }

    start_in_source <- cursor + as.integer(pos) - 1L
    end_in_source   <- start_in_source + attr(pos, "match.length") - 1L

    starts[i] <- start_in_source
    ends[i]   <- end_in_source
    cursor    <- end_in_source + 1L
  }

  data.frame(start = starts, end = ends)
}


#' Krippendorff's alpha for unitizing
#'
#' `r lifecycle::badge("experimental")`
#'
#' Native implementation of the `_u_alpha` family for two or more
#' unitizations of a common continuum (Krippendorff, 2019, section 12.6).
#' One call computes all variants -- overall (`_u_alpha_nominal`),
#' boundary-only (`|_u_alpha_binary`), coding-conditional
#' (`_cu_alpha_nominal`), and per-value (`_(k)u_alpha_nominal`).
#'
#' @param unitizations A list of data.frames, one per observer. Each must
#'   have columns `start`, `end` (1-based, inclusive character positions),
#'   and `value` (the category assigned to the segment).
#' @param L Integer length of the continuum.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`method`}{`"alpha_u"`.}
#'     \item{`value`}{Numeric -- `_u_alpha_nominal` (overall agreement on
#'       both boundaries and codes; section 12.6.3, eq. 34).}
#'     \item{`binary`}{Numeric -- `|_u_alpha_binary` (boundary-only;
#'       section 12.6.4, eq. 35).}
#'     \item{`cu_nominal`}{Numeric -- `_cu_alpha_nominal` (coding given
#'       unitization; section 12.6.5, eqs. 36--37).}
#'     \item{`ci_lower`, `ci_upper`}{`NA_real_` (uniform shape).}
#'     \item{`per_value`}{Data.frame with columns `value`, `alpha`,
#'       `coverage` -- per-value reliability `_(k)u_alpha_nominal`
#'       (section 12.6.6, eq. 38).}
#'     \item{`n_observers`}{Number of observers (`m`).}
#'     \item{`L`}{Continuum length.}
#'   }
#'
#' @references
#' Krippendorff, K. (2019). *Content Analysis: An Introduction to Its
#' Methodology* (4th ed.). Sage.
#'
#' @keywords internal
reliability_alpha_u <- function(unitizations, L) {
  m <- length(unitizations)
  if (m < 2L) {
    cli::cli_abort("At least two unitizations are required.")
  }
  if (!is.numeric(L) || length(L) != 1L || L < 1L) {
    cli::cli_abort("{.arg L} must be a positive integer.")
  }
  L <- as.integer(L)

  phi <- "\u03c6"  # Greek small letter phi (gap marker)

  expand_to_partition <- function(df) {
    if (nrow(df) == 0L) {
      return(data.frame(start = 1L, end = L, value = phi,
                        stringsAsFactors = FALSE))
    }
    df <- df[order(df$start), ]
    parts <- vector("list", 2L * nrow(df) + 1L)
    k <- 0L
    cursor <- 1L
    for (i in seq_len(nrow(df))) {
      seg_start <- df$start[i]
      seg_end   <- df$end[i]
      seg_val   <- as.character(df$value[i])
      if (seg_start > cursor) {
        k <- k + 1L
        parts[[k]] <- data.frame(start = cursor, end = seg_start - 1L,
                                 value = phi, stringsAsFactors = FALSE)
      }
      k <- k + 1L
      parts[[k]] <- data.frame(start = seg_start, end = seg_end,
                                value = seg_val, stringsAsFactors = FALSE)
      cursor <- seg_end + 1L
    }
    if (cursor <= L) {
      k <- k + 1L
      parts[[k]] <- data.frame(start = cursor, end = L, value = phi,
                                stringsAsFactors = FALSE)
    }
    do.call(rbind, parts[seq_len(k)])
  }

  partitions <- lapply(unitizations, expand_to_partition)

  all_values <- unique(unlist(lapply(partitions, function(p) p$value)))
  all_values <- c(phi, setdiff(all_values, phi))
  v <- length(all_values)

  # Observed coincidence matrix (eq. 32)
  ell <- matrix(0, nrow = v, ncol = v,
                dimnames = list(all_values, all_values))
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (i == j) next
      pi_i <- partitions[[i]]
      pj_j <- partitions[[j]]
      for (gi in seq_len(nrow(pi_i))) {
        for (hj in seq_len(nrow(pj_j))) {
          int_start <- max(pi_i$start[gi], pj_j$start[hj])
          int_end   <- min(pi_i$end[gi], pj_j$end[hj])
          int_len   <- max(0L, int_end - int_start + 1L)
          if (int_len > 0L) {
            ell[pi_i$value[gi], pj_j$value[hj]] <-
              ell[pi_i$value[gi], pj_j$value[hj]] + int_len
          }
        }
      }
    }
  }
  ell <- ell / (m - 1L)

  ell_row <- rowSums(ell)
  ell_col <- colSums(ell)
  ell_tot <- sum(ell)

  # Self-pairing correction (eq. 33)
  correction_by_value <- stats::setNames(rep(0, v), all_values)
  correction_total <- 0
  for (i in seq_len(m)) {
    pi_i <- partitions[[i]]
    for (g in seq_len(nrow(pi_i))) {
      seg_len <- pi_i$end[g] - pi_i$start[g] + 1L
      val <- pi_i$value[g]
      term <- if (val == phi) seg_len else seg_len^2
      correction_by_value[val] <- correction_by_value[val] + term
      correction_total <- correction_total + term
    }
  }

  eps <- matrix(0, nrow = v, ncol = v, dimnames = list(all_values, all_values))
  denom <- ell_tot - correction_total / ell_tot
  for (ci in seq_len(v)) {
    for (ki in seq_len(v)) {
      c_val <- all_values[ci]
      k_val <- all_values[ki]
      num <- ell_row[c_val] * ell_col[k_val]
      if (c_val == k_val) num <- num - correction_by_value[c_val]
      eps[c_val, k_val] <- num / denom
    }
  }

  non_phi <- setdiff(all_values, phi)

  # _u_alpha_nominal (eq. 34)
  D_o_full <- ell_tot - sum(diag(ell))
  D_e_full <- sum(eps)  - sum(diag(eps))
  alpha_nominal <- alpha_from_disagreements(D_o_full, D_e_full)

  # |_u_alpha_binary (eq. 35) -- collapse to phi vs non-phi
  ell_2x2 <- matrix(0, 2, 2, dimnames = list(c(phi, "!phi"), c(phi, "!phi")))
  ell_2x2[1, 1] <- ell[phi, phi]
  ell_2x2[1, 2] <- sum(ell[phi, non_phi])
  ell_2x2[2, 1] <- sum(ell[non_phi, phi])
  ell_2x2[2, 2] <- sum(ell[non_phi, non_phi])
  eps_2x2 <- matrix(0, 2, 2, dimnames = list(c(phi, "!phi"), c(phi, "!phi")))
  eps_2x2[1, 1] <- eps[phi, phi]
  eps_2x2[1, 2] <- sum(eps[phi, non_phi])
  eps_2x2[2, 1] <- sum(eps[non_phi, phi])
  eps_2x2[2, 2] <- sum(eps[non_phi, non_phi])
  D_o_bin <- sum(ell_2x2) - sum(diag(ell_2x2))
  D_e_bin <- sum(eps_2x2) - sum(diag(eps_2x2))
  alpha_binary <- alpha_from_disagreements(D_o_bin, D_e_bin)

  # _cu_alpha_nominal and _(k)u_alpha -- restrict to non-gap (eqs. 36-38)
  alpha_cu  <- NA_real_
  per_value <- data.frame(
    value    = character(0),
    alpha    = numeric(0),
    coverage = numeric(0),
    stringsAsFactors = FALSE
  )

  if (length(non_phi) > 0L) {
    ell_star <- ell[non_phi, non_phi, drop = FALSE]
    ell_star_row <- rowSums(ell_star)
    ell_star_col <- colSums(ell_star)
    ell_star_tot <- sum(ell_star)

    if (ell_star_tot > 0) {
      # Per-segment "pairable length" (overlap with other observers' non-gap union)
      nongap_coverage <- vector("list", m)
      for (i in seq_len(m)) {
        cov <- logical(L)
        pi_i <- partitions[[i]]
        for (g in seq_len(nrow(pi_i))) {
          if (pi_i$value[g] != phi) cov[pi_i$start[g]:pi_i$end[g]] <- TRUE
        }
        nongap_coverage[[i]] <- cov
      }

      pairable_correction_by_value <- stats::setNames(rep(0, length(non_phi)), non_phi)
      pairable_correction_total <- 0
      for (i in seq_len(m)) {
        others_cov <- logical(L)
        for (j in seq_len(m)) {
          if (j == i) next
          others_cov <- others_cov | nongap_coverage[[j]]
        }
        pi_i <- partitions[[i]]
        for (g in seq_len(nrow(pi_i))) {
          val <- pi_i$value[g]
          if (val == phi) next
          seg_positions <- pi_i$start[g]:pi_i$end[g]
          pairable_len <- sum(others_cov[seg_positions])
          term <- pairable_len^2
          pairable_correction_by_value[val] <-
            pairable_correction_by_value[val] + term
          pairable_correction_total <- pairable_correction_total + term
        }
      }

      denom_star <- ell_star_tot - pairable_correction_total / ell_star_tot
      eps_star <- matrix(0, nrow = length(non_phi), ncol = length(non_phi),
                         dimnames = list(non_phi, non_phi))
      for (ci in seq_along(non_phi)) {
        for (ki in seq_along(non_phi)) {
          c_val <- non_phi[ci]
          k_val <- non_phi[ki]
          num <- ell_star_row[c_val] * ell_star_col[k_val]
          if (c_val == k_val) num <- num - pairable_correction_by_value[c_val]
          eps_star[c_val, k_val] <- num / denom_star
        }
      }

      D_o_cu <- ell_star_tot - sum(diag(ell_star))
      D_e_cu <- sum(eps_star) - sum(diag(eps_star))
      alpha_cu <- alpha_from_disagreements(D_o_cu, D_e_cu)

      per_value <- data.frame(
        value    = non_phi,
        alpha    = NA_real_,
        coverage = NA_real_,
        stringsAsFactors = FALSE
      )
      for (ki in seq_along(non_phi)) {
        k_val <- non_phi[ki]
        ell_star_kk <- ell_star[k_val, k_val]
        ell_star_dk <- ell_star_col[k_val]
        eps_star_kk <- eps_star[k_val, k_val]
        eps_star_dk <- colSums(eps_star)[k_val]
        ell_full_dk <- ell_col[k_val]

        per_value$coverage[ki] <- if (ell_full_dk > 0) ell_star_dk / ell_full_dk else 0

        D_o_k <- ell_star_dk - ell_star_kk
        D_e_k <- eps_star_dk - eps_star_kk
        if (D_e_k == 0 || ell_star_dk == 0) {
          per_value$alpha[ki] <- if (D_o_k == 0) 1.0 else NA_real_
        } else {
          per_value$alpha[ki] <- 1 - (eps_star_dk / ell_star_dk) * D_o_k / D_e_k
        }
      }
    }
  }

  list(
    method      = "alpha_u",
    value       = alpha_nominal,
    binary      = alpha_binary,
    cu_nominal  = alpha_cu,
    ci_lower    = NA_real_,
    ci_upper    = NA_real_,
    per_value   = per_value,
    n_observers = m,
    L           = L
  )
}


# 1 - D_o / D_e, with the conventional limits for degenerate cases:
# both zero -> perfect reliability (1.0); D_e == 0 with D_o > 0 -> undefined (NA).
alpha_from_disagreements <- function(D_o, D_e) {
  if (D_e == 0) {
    if (D_o == 0) 1.0 else NA_real_
  } else {
    1 - D_o / D_e
  }
}
