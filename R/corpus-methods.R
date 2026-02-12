# Corpus wrapper class methods
#
# The qlm_corpus wrapper class provides basic printing for quanteda corpus
# objects when quanteda is not loaded. This allows bundled data_corpus_* objects
# to print nicely without requiring quanteda as a dependency.
#
# The wrapper class approach cleanly delegates to quanteda methods when available
# and avoids namespace conflicts entirely. Methods are exported normally and
# dispatch through the standard S3 system.

#' Coerce to qlm_corpus
#'
#' Adds the qlm_corpus class wrapper to a quanteda corpus object.
#' Called internally by quallmer functions that accept corpus input.
#'
#' @param x A corpus object
#' @return The corpus with "qlm_corpus" prepended to its class
#' @keywords internal
as_qlm_corpus <- function(x) {
  if (!inherits(x, "corpus")) {
    cli::cli_abort("{.arg x} must be a corpus object, not {.cls {class(x)}}.")
  }
  if (!inherits(x, "qlm_corpus")) {
    class(x) <- c("qlm_corpus", class(x))
  }
  x
}

#' Print method for qlm_corpus objects
#'
#' Provides a simple print method for corpus objects when quanteda is not loaded.
#' When quanteda is available, delegates to its print.corpus method using
#' NextMethod(). This displays basic information about the corpus structure
#' without requiring quanteda as a dependency.
#'
#' @param x a qlm_corpus object
#' @param ... additional arguments passed to methods
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects
#'   (printing to console).
#' @export
#' @keywords internal
print.qlm_corpus <- function(x, ...) {
  if (isNamespaceLoaded("quanteda")) {
    # Remove qlm_corpus class and delegate to quanteda's print.corpus
    class(x) <- setdiff(class(x), "qlm_corpus")
    print(x, ...)
  } else {
    # Fallback implementation when quanteda is not available
    ndoc <- length(x)

    cat("Corpus consisting of ", ndoc, " document",
        if (ndoc != 1) "s" else "", ".\n", sep = "")

    # Get docvars if present
    docvars <- attr(x, "docvars")
    if (!is.null(docvars) && ncol(docvars) > 3) {
      # Skip internal quanteda docvars (docname_, docid_, segid_)
      user_docvars <- setdiff(names(docvars), c("docname_", "docid_", "segid_"))
      if (length(user_docvars) > 0) {
        cat("\nDocvars: ", paste(user_docvars, collapse = ", "), "\n", sep = "")
      }
    }

    # Show first few documents
    if (ndoc > 0) {
      cat("\n")
      show_n <- min(2, ndoc)
      doc_names <- names(x)
      if (is.null(doc_names)) doc_names <- paste0("text", seq_len(ndoc))

      for (i in seq_len(show_n)) {
        doc_text <- as.character(x[i])
        # Truncate long documents
        if (nchar(doc_text) > 70) {
          doc_text <- paste0(substr(doc_text, 1, 67), "...")
        }
        cat(doc_names[i], ": ", doc_text, "\n", sep = "")
      }

      if (ndoc > show_n) {
        cat("[ ... and ", ndoc - show_n, " more document",
            if (ndoc - show_n != 1) "s" else "", " ]\n", sep = "")
      }
    }

    invisible(x)
  }
}

#' Subset method for qlm_corpus objects
#'
#' @param x a qlm_corpus object
#' @param i index for subsetting
#' @param ... additional arguments
#'
#' @return A subsetted \code{qlm_corpus} object containing only the selected
#'   documents.
#' @export
#' @keywords internal
`[.qlm_corpus` <- function(x, i, ...) {
  if (isNamespaceLoaded("quanteda")) {
    # Remove qlm_corpus class and delegate to quanteda's [.corpus
    class(x) <- setdiff(class(x), "qlm_corpus")
    result <- x[i, ...]
    # Re-add qlm_corpus class to result
    class(result) <- c("qlm_corpus", class(result))
    result
  } else {
    # Fallback implementation when quanteda is not available
    # Basic subsetting that preserves corpus structure
    result <- unclass(x)[i]

    # Preserve corpus class with qlm_corpus wrapper
    class(result) <- c("qlm_corpus", "corpus")

    # Subset docvars if present
    docvars <- attr(x, "docvars")
    if (!is.null(docvars)) {
      attr(result, "docvars") <- docvars[i, , drop = FALSE]
    }

    # Preserve meta attribute
    attr(result, "meta") <- attr(x, "meta")

    result
  }
}
