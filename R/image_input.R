#' Is an element of `x` an image URL rather than a file path?
#'
#' The rule ellmer applies inside [ellmer::content_image_url()] for data
#' URIs, extended to http and https: a scheme means a URL, and everything
#' else is a path. A URL typed without its scheme is therefore a path, and
#' fails the existence check with its value named, before anything is sent.
#' The general form is `is_input_url()` in `R/input_content.R`.
#'
#' @param x A character vector.
#' @return A logical vector.
#' @keywords internal
#' @noRd
is_image_url <- function(x) {
  is_input_url(x, data = TRUE)
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
#' @param resize The codebook's `image_file_resize`, applied to paths.
#' @param detail The codebook's `image_url_detail`, applied to URLs.
#'
#' @return A list of content objects, one per element of `x`.
#' @keywords internal
#' @noRd
as_image_content <- function(x, resize, detail = "auto") {
  lapply(x, function(el) {
    if (is_image_url(el)) {
      ellmer::content_image_url(el, detail = detail)
    } else {
      ellmer::content_image_file(el, resize = resize)
    }
  })
}


#' Say when the URL detail setting cannot take effect
#'
#' `content_image_url()` has always accepted `detail`, but ellmer forwarded
#' it to no provider before tidyverse/ellmer#1133, and only OpenAI and
#' OpenAI-compatible providers read it. A codebook may still carry the
#' setting, so that the run records the intent and a later ellmer applies
#' it; what must not happen is the run passing in silence as if it had been
#' applied. Said once, before anything is sent.
#'
#' @param codebook An image codebook.
#' @param x The input vector, already checked.
#' @param chat The chat the run will use.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
say_image_url_detail <- function(codebook, x, chat) {
  detail <- codebook$image_url_detail %||% "auto"
  if (identical(detail, "auto") || !any(is_image_url(x))) {
    return(invisible(NULL))
  }
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  setting <- paste0("image_url_detail = \"", detail, "\"")
  if (!ellmer_forwards_image_detail()) {
    cli::cli_inform(c(
      "i" = paste0(
        "{.code {setting}} is set, but ellmer ",
        "{utils::packageVersion('ellmer')} does not pass it to the provider, ",
        "so the provider chooses the detail for the image URLs itself."
      ),
      "i" = "The run records the ellmer version; ellmer forwards the setting from the version that includes tidyverse/ellmer#1133."
    ))
  } else if (!is.null(provider) && !provider_reads_image_detail(provider)) {
    cli::cli_inform(c(
      "i" = paste0(
        "{.code {setting}} is set, but {provider@name} ignores it; ",
        "OpenAI and OpenAI-compatible providers use it."
      )
    ))
  }
  invisible(NULL)
}


#' Does the installed ellmer forward `detail` to the provider?
#'
#' tidyverse/ellmer#1133 gave inline images a `detail` property alongside
#' the remote ones, and it is that property the serializers read. Its
#' presence is therefore the mark of an ellmer that forwards the setting,
#' checkable without a request. To be replaced by a version check once the
#' change is released.
#'
#' @keywords internal
#' @noRd
ellmer_forwards_image_detail <- function() {
  img <- ellmer::content_image_url("data:image/png;base64,AA==")
  tryCatch(is.character(img@detail), error = function(e) FALSE)
}


#' Does this provider read the image `detail` field?
#' @keywords internal
#' @noRd
provider_reads_image_detail <- function(provider) {
  inherits(provider, c("ellmer::ProviderOpenAI", "ellmer::ProviderOpenAICompatible"))
}
