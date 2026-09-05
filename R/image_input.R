#' Is an element of `x` a URL rather than a file path?
#'
#' The rule ellmer applies inside [ellmer::content_image_url()] for data
#' URIs, extended to http and https: a scheme means a URL, and everything
#' else is a path. A URL typed without its scheme is therefore a path, and
#' fails the existence check with its value named, before anything is sent.
#'
#' @param x A character vector.
#' @return A logical vector.
#' @keywords internal
#' @noRd
is_image_url <- function(x) {
  !is.na(x) & grepl("^(https?|data):", x)
}


#' Check image inputs before any request
#'
#' Every element must be a URL or the path of an existing file. Checked in
#' one pass so that every missing file is reported at once, and before any
#' paid call, rather than one at a time inside ellmer.
#'
#' @param x The input vector.
#' @param call The calling environment, for the error.
#'
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_image_inputs <- function(x, call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort(
      "This codebook expects image file paths or URLs (a character vector).",
      call = call
    )
  }
  is_path <- !is_image_url(x)
  missing <- is_path & (is.na(x) | !file.exists(x) | dir.exists(x))
  if (any(missing)) {
    cli::cli_abort(c(
      "{sum(missing)} image {cli::qty(sum(missing))}file{?s} {?does/do} not exist: {.path {x[missing]}}.",
      "i" = "Every element of {.arg x} must be the path of an existing file or a URL.",
      "i" = "A URL is recognised by its scheme: {.code http://}, {.code https://} or {.code data:}."
    ), call = call)
  }
  invisible(x)
}


#' Check that the codebook's resize setting can be applied
#'
#' Resizing needs magick, which ellmer only Suggests. Checked here, naming the
#' setting and the way round it, before any request is sent. A run whose
#' inputs are all URLs never resizes, so needs nothing.
#'
#' @param codebook An image codebook.
#' @param x The input vector, already checked.
#' @param call The calling environment, for the error.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
check_image_resize <- function(codebook, x, call = rlang::caller_env()) {
  resize <- check_image_file_resize(codebook$image_file_resize, "image",
                                    call = call)
  if (resize != "none" && any(!is_image_url(x))) {
    rlang::check_installed(
      "magick",
      reason = paste0(
        "to resize images before coding; the codebook has ",
        "{.code image_file_resize = \"", resize, "\"}. ",
        "Set it to {.val none} to send the files as they are."
      ),
      call = call
    )
  }
  invisible(NULL)
}


#' Turn image inputs into ellmer content
#'
#' A path is read, resized and sent inline through
#' [ellmer::content_image_file()]; a URL is passed on as it is through
#' [ellmer::content_image_url()]. The two may be mixed in one vector.
#'
#' @param x The input vector, already checked.
#' @param resize The codebook's `image_file_resize`.
#'
#' @return A list of content objects, one per element of `x`.
#' @keywords internal
#' @noRd
as_image_content <- function(x, resize) {
  lapply(x, function(el) {
    if (is_image_url(el)) {
      ellmer::content_image_url(el)
    } else {
      ellmer::content_image_file(el, resize = resize)
    }
  })
}
