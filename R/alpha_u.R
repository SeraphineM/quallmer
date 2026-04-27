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
#' @details
#' Matching proceeds sequentially: each segment is located starting from the
#' end of the previous match. Whitespace is normalised (runs of whitespace
#' collapsed to a single space) before matching, so minor formatting
#' differences between the segment text and the source are tolerated.
#'
#' @keywords internal
#' @noRd
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

    # First try exact match in the original text
    remainder <- substring(source_text, cursor)
    pos <- regexpr(seg_trimmed, remainder, fixed = TRUE)

    if (pos == -1L) {
      # Fall back: split on whitespace, escape each word for regex, rejoin
      # with \\s+ to flexibly match any whitespace in the original
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
    cursor     <- end_in_source + 1L
  }

  data.frame(start = starts, end = ends)
}


#' Compute Krippendorff's alpha for unitizing
#'
#' `r lifecycle::badge("experimental")`
#'
#' Implements the `_u_alpha` family of coefficients for comparing two or more
#' unitizations of a common continuum (Krippendorff, 2019, section 12.6).
#'
#' @param unitizations A list of data.frames, one per observer. Each must have
#'   columns `start`, `end` (1-based, inclusive character positions), and
#'   `value` (the category assigned to the segment, or `NA` for gaps).
#' @param L Integer length of the continuum.
#' @param type Character: `"nominal"` for `_u_alpha_nominal` (default),
#'   `"binary"` for `|_u_alpha_binary`, `"cu_nominal"` for `_cu_alpha_nominal`
#'   (coding reliability conditional on unitization), or `"per_value"` for
#'   per-value `_(k)u_alpha_nominal` (reliability of individual values).
#'
#' @return For `type = "nominal"`, `"binary"`, or `"cu_nominal"`: a single
#'   numeric value. For `type = "per_value"`: a data.frame with columns
#'   `value`, `alpha`, and `coverage`.
#'
#' @details
#' The computation follows Krippendorff (2019, section 12.6):
#' 1. Gaps (characters not covered by any segment) are assigned value `phi`.
#' 2. The observed coincidence matrix `ell_ck` is built from the lengths of
#'    intersections of all segment pairs across all observer pairs (eq. 32).
#' 3. The expected coincidence matrix `eps_ck` corrects for segment lengths
#'    (eq. 33).
#' 4. Alpha is `1 - D_o / D_e` where `D_o` and `D_e` are the observed and
#'    expected disagreements (eq. 34).
#'
#' For `type = "binary"`, the full coincidence matrix is collapsed to a
#' 2-by-2 matrix distinguishing only between gap (`phi`) and non-gap (`!phi`)
#' before computing alpha (section 12.6.4, eq. 35).
#'
#' For `type = "cu_nominal"`, the coincidence matrix is restricted to
#' non-gap segments only (section 12.6.5, eqs. 36--37). The expected
#' coincidences use each segment's "pairable length" — the portion that
#' overlaps with non-gap segments from other observers — rather than the
#' full segment length.
#'
#' For `type = "per_value"`, the per-value reliability `_(k)u_alpha` is
#' computed from the k-th row/column of the sub-matrix (section 12.6.6,
#' eq. 38), along with coverage (the percentage of identified segments
#' carrying that value).
#'
#' @references
#' Krippendorff, K. (2019). *Content Analysis: An Introduction to Its
#' Methodology* (4th ed.). Sage.
#'
#' @keywords internal
#' @noRd
compute_alpha_u <- function(unitizations, L,
                            type = c("nominal", "binary",
                                     "cu_nominal", "per_value")) {
  type <- match.arg(type)
  m <- length(unitizations)
  if (m < 2L) {
    cli::cli_abort("At least two unitizations are required.")
  }
  if (!is.numeric(L) || length(L) != 1L || L < 1L) {
    cli::cli_abort("{.arg L} must be a positive integer.")
  }
  L <- as.integer(L)

  # -- Step 0: Expand each unitization into a full partition of [1, L] --------
  # Fill gaps with value "phi" (a reserved token for irrelevant matter).
  phi <- "\u03c6"  # Unicode phi

  expand_to_partition <- function(df) {
    # df has columns: start, end, value
    # Returns a data.frame covering [1, L] with no gaps
    if (nrow(df) == 0L) {
      return(data.frame(start = 1L, end = L, value = phi,
                        stringsAsFactors = FALSE))
    }

    # Sort by start position
    df <- df[order(df$start), ]
    parts <- vector("list", 2L * nrow(df) + 1L)
    k <- 0L
    cursor <- 1L

    for (i in seq_len(nrow(df))) {
      seg_start <- df$start[i]
      seg_end   <- df$end[i]
      seg_val   <- as.character(df$value[i])

      # Gap before this segment
      if (seg_start > cursor) {
        k <- k + 1L
        parts[[k]] <- data.frame(start = cursor, end = seg_start - 1L,
                                 value = phi, stringsAsFactors = FALSE)
      }
      # The segment itself
      k <- k + 1L
      parts[[k]] <- data.frame(start = seg_start, end = seg_end,
                                value = seg_val, stringsAsFactors = FALSE)
      cursor <- seg_end + 1L
    }

    # Gap after last segment
    if (cursor <= L) {
      k <- k + 1L
      parts[[k]] <- data.frame(start = cursor, end = L, value = phi,
                                stringsAsFactors = FALSE)
    }

    do.call(rbind, parts[seq_len(k)])
  }

  partitions <- lapply(unitizations, expand_to_partition)

  # -- Step 1: Collect all unique values (including phi) ----------------------
  all_values <- unique(unlist(lapply(partitions, function(p) p$value)))
  # Ensure phi is first
  all_values <- c(phi, setdiff(all_values, phi))
  v <- length(all_values)

  # -- Step 2: Build observed coincidence matrix (eq. 32) ---------------------
  # ell_ck = 1/(m-1) * sum over all observer pairs (i,j) i!=j,
  #          sum over all segment pairs (g,h),
  #          of L(S_ig valued c INTERSECT S_jh valued k)
  ell <- matrix(0, nrow = v, ncol = v,
                dimnames = list(all_values, all_values))

  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (i == j) next
      pi <- partitions[[i]]
      pj <- partitions[[j]]

      for (gi in seq_len(nrow(pi))) {
        for (hj in seq_len(nrow(pj))) {
          # Intersection length
          int_start <- max(pi$start[gi], pj$start[hj])
          int_end   <- min(pi$end[gi], pj$end[hj])
          int_len   <- max(0L, int_end - int_start + 1L)

          if (int_len > 0L) {
            c_val <- pi$value[gi]
            k_val <- pj$value[hj]
            ell[c_val, k_val] <- ell[c_val, k_val] + int_len
          }
        }
      }
    }
  }

  # Normalise by 1/(m-1) -- the factor accounts for directed pairs
  # But eq. 32 sums over i and j!=i, giving m*(m-1) directed pairs.
  # The 1/(m-1) factor normalises so that ell_.. = m*L.
  ell <- ell / (m - 1L)

  # -- Step 3: Build expected coincidence matrix (eq. 33) ---------------------
  # eps_ck depends on marginal sums and a correction for segment self-pairing.
  #
  # Correction term: for each observer i and segment g,
  #   if the segment is valued c != phi: (L(S_ig))^2
  #   if the segment is valued phi:      L(S_ig)
  # These are used differently for different c,k combinations.
  #
  # Following eq. 33:
  # eps_ck = [ ell_c. * ell_.k - delta(c,k) * C_term ] / [ ell_..^2 - C_total ]
  #
  # Where the correction terms prevent segments from pairing with themselves.

  ell_row <- rowSums(ell)   # ell_c.
  ell_col <- colSums(ell)   # ell_.k
  ell_tot <- sum(ell)       # ell_..

  # Correction: sum_i sum_g L(S_ig valued=phi) for gap segments,
  # and sum_i sum_g (L(S_ig valued c!=phi))^2 for valued segments.
  # For the diagonal (c == k), we subtract segments' self-pairing contribution.

  # Build per-value correction: for each value c,
  # sum over all observers and their segments valued c of L(seg)^2
  # (for phi-valued segments, it's L(seg) -- but actually re-reading eq. 33
  # more carefully, the correction uses L(S_ig valued=phi) / (L(S_ig valued c!=phi))^2
  # structure differently).

  # Let me implement the direct approach instead: compute eps from the full
  # formula. For each (c,k) pair:
  #
  # When c == k:
  #   eps_cc = [ell_c.^2 - correction_c] / [ell_..^2 - correction_total] * ell_..
  #          (approximated by the marginal product minus self-pairing correction)
  #
  # Actually, let me implement this more carefully following the exact formula.
  # The expected coincidences are most cleanly computed as:
  #
  #   eps_ck = ell_c. * ell_.k / ell_..   (if c != k)
  #   eps_cc = (ell_c. * ell_.c - S_c) / (ell_.. - S_total / ell_..)
  #
  # where S_c = sum over all observers i and segments g valued c of L(seg)^2.
  #
  # But this isn't quite right either. Let me use the cleanest formulation from

  # the coincidence matrix approach.
  #
  # From p. 341, eq. 33, the expected coincidences are:
  #
  # eps_ck = [ ell_c. * ell_.k - {numerator correction} ] /
  #          [ ell_..           - {denominator correction} ]
  #
  # The corrections prevent an observer's segment from intersecting with itself.
  # For the general case with segments of unequal length:
  #
  # numerator correction (only when c == k):
  #   sum_i^m sum_g [ L(S_ig valued=phi) if value is phi, else (L(S_ig valued c!=phi))^2 ]
  #   but only for segments valued c.
  #
  # Actually, reading eq. 33 very carefully:
  #
  # eps_ck = [ ell_c. * ell_.k - sum_i^m sum_g { L(S_ig valued=phi) /
  #           (L(S_ig valued c!=phi))^2 } iff c=k ] /
  #          [ ell_..^2 - sum_i^m sum_g { L(S_ig valued=phi) /
  #           (L(S_ig valued c!=phi))^2 } ]
  #
  # I think the "/" in the formula is actually a conditional:
  # - For gap segments (valued phi): the term is L(S_ig)
  # - For non-gap segments (valued c != phi): the term is L(S_ig)^2
  #
  # And these terms appear in both numerator (when c==k) and denominator.

  # Let me compute the self-pairing correction per value.
  # For each value v, self_pair[v] = sum over all observers and segments valued v
  # of L(seg)^2 if v != phi, or L(seg) if v == phi.

  # Actually wait, re-reading again more carefully. The formula from the book:
  #
  # The correction term in the denominator is:
  #   sum_i^m sum_g { L(S_ig valued = phi) when segment is a gap
  #                   (L(S_ig valued c != phi))^2 when segment is valued }
  #
  # So for ALL segments across all observers:
  #   if gap: add L(seg)
  #   if valued: add L(seg)^2
  #
  # And for the numerator correction (only when c==k):
  #   same terms but restricted to segments valued c.

  # Compute self-pairing correction per segment
  correction_by_value <- stats::setNames(rep(0, v), all_values)
  correction_total <- 0

  for (i in seq_len(m)) {
    pi <- partitions[[i]]
    for (g in seq_len(nrow(pi))) {
      seg_len <- pi$end[g] - pi$start[g] + 1L
      val <- pi$value[g]
      if (val == phi) {
        term <- seg_len
      } else {
        term <- seg_len^2
      }
      correction_by_value[val] <- correction_by_value[val] + term
      correction_total <- correction_total + term
    }
  }

  # Build expected coincidence matrix
  eps <- matrix(0, nrow = v, ncol = v,
                dimnames = list(all_values, all_values))

  denom <- ell_tot - correction_total / ell_tot

  for (ci in seq_len(v)) {
    for (ki in seq_len(v)) {
      c_val <- all_values[ci]
      k_val <- all_values[ki]

      numerator_product <- ell_row[c_val] * ell_col[k_val]

      if (c_val == k_val) {
        # Subtract self-pairing correction for this value
        numerator <- numerator_product - correction_by_value[c_val]
      } else {
        numerator <- numerator_product
      }

      eps[c_val, k_val] <- numerator / denom
    }
  }

  # -- Step 4: Compute alpha (eq. 34) -----------------------------------------
  non_phi <- setdiff(all_values, phi)

  if (type == "binary") {
    # Collapse to 2x2: phi vs non-phi (section 12.6.4, eq. 35)
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

    D_o <- sum(ell_2x2) - sum(diag(ell_2x2))
    D_e <- sum(eps_2x2) - sum(diag(eps_2x2))

    if (D_e == 0) return(NA_real_)
    return(1 - D_o / D_e)
  }

  if (type == "nominal") {
    # Full nominal (eq. 34)
    D_o <- ell_tot - sum(diag(ell))
    D_e <- sum(eps) - sum(diag(eps))

    if (D_e == 0) return(NA_real_)
    return(1 - D_o / D_e)
  }

  # -- cu_nominal and per_value: sub-matrix of valued segments (section 12.6.5-6)

  # Extract observed sub-matrix (exclude phi row/column)
  ell_star <- ell[non_phi, non_phi, drop = FALSE]
  ell_star_row <- rowSums(ell_star)
  ell_star_col <- colSums(ell_star)
  ell_star_tot <- sum(ell_star)

  if (ell_star_tot == 0) return(NA_real_)

  # Compute pairable length for each non-gap segment (eq. 36):
  # the portion that overlaps with the union of all non-gap segments
  # from OTHER observers.

  # For each observer, build a boolean coverage vector of non-gap positions
  nongap_coverage <- vector("list", m)
  for (i in seq_len(m)) {
    cov <- logical(L)
    pi <- partitions[[i]]
    for (g in seq_len(nrow(pi))) {
      if (pi$value[g] != phi) {
        cov[pi$start[g]:pi$end[g]] <- TRUE
      }
    }
    nongap_coverage[[i]] <- cov
  }

  # For each observer i, the "other observers' non-gap union" is the
  # OR of all other observers' coverage vectors
  pairable_correction_by_value <- stats::setNames(rep(0, length(non_phi)), non_phi)
  pairable_correction_total <- 0

  for (i in seq_len(m)) {
    # Union of all other observers' non-gap coverage
    others_cov <- logical(L)
    for (j in seq_len(m)) {
      if (j == i) next
      others_cov <- others_cov | nongap_coverage[[j]]
    }

    pi <- partitions[[i]]
    for (g in seq_len(nrow(pi))) {
      val <- pi$value[g]
      if (val == phi) next

      # Pairable length: intersection of this segment with others' non-gap
      seg_positions <- pi$start[g]:pi$end[g]
      pairable_len <- sum(others_cov[seg_positions])
      term <- pairable_len^2

      pairable_correction_by_value[val] <-
        pairable_correction_by_value[val] + term
      pairable_correction_total <- pairable_correction_total + term
    }
  }

  # Build expected sub-matrix (eq. 36)
  denom_star <- ell_star_tot - pairable_correction_total / ell_star_tot
  eps_star <- matrix(0, nrow = length(non_phi), ncol = length(non_phi),
                     dimnames = list(non_phi, non_phi))

  for (ci in seq_along(non_phi)) {
    for (ki in seq_along(non_phi)) {
      c_val <- non_phi[ci]
      k_val <- non_phi[ki]
      num <- ell_star_row[c_val] * ell_star_col[k_val]
      if (c_val == k_val) {
        num <- num - pairable_correction_by_value[c_val]
      }
      eps_star[c_val, k_val] <- num / denom_star
    }
  }

  if (type == "cu_nominal") {
    # _cu_alpha_nominal (eq. 37)
    D_o <- ell_star_tot - sum(diag(ell_star))
    D_e <- sum(eps_star) - sum(diag(eps_star))

    if (D_e == 0) return(NA_real_)
    return(1 - D_o / D_e)
  }

  if (type == "per_value") {
    # _(k)u_alpha_nominal for each value k (eq. 38)
    results <- data.frame(
      value    = non_phi,
      alpha    = NA_real_,
      coverage = NA_real_,
      stringsAsFactors = FALSE
    )

    for (ki in seq_along(non_phi)) {
      k_val <- non_phi[ki]
      ell_star_kk <- ell_star[k_val, k_val]
      ell_star_dk <- ell_star_col[k_val]   # ℓ*_.k
      eps_star_kk <- eps_star[k_val, k_val]
      eps_star_dk <- colSums(eps_star)[k_val]  # ε*_.k

      # Coverage: ℓ*_.k / ℓ_.k (proportion of all k-valued matter that is
      # in the valued sub-matrix, i.e., not intersecting only with gaps)
      ell_full_dk <- ell_col[k_val]  # from full matrix
      results$coverage[ki] <- if (ell_full_dk > 0) ell_star_dk / ell_full_dk else 0

      # Alpha: eq. 38 with nominal metric
      D_o_k <- ell_star_dk - ell_star_kk
      D_e_k <- eps_star_dk - eps_star_kk

      if (D_e_k == 0 || ell_star_dk == 0) {
        # Perfect agreement or no data for this value
        results$alpha[ki] <- if (D_o_k == 0) 1.0 else NA_real_
      } else {
        results$alpha[ki] <- 1 - (eps_star_dk / ell_star_dk) * D_o_k / D_e_k
      }
    }

    return(results)
  }
}
