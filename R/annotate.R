#' Apply an annotation task to input data
#'
#' Automatically detects the correct task type (e.g., text, image).
#' Delegates the actual processing to the task's internal run() method.
#'
#' @param .data Input data (text, image, etc.)
#' @param task A task created with [define_task()]
#' @param ... Additional arguments passed to task$run()
#' @return Structured data frame with results
#' @export
annotate <- function(.data, task, ...) {
  if (!inherits(task, "define_task")) {
    stop("`task` must be created using define_task().")
  }

  input_type <- task$input_type

  # Simple validation — later you can extend this with classes (e.g., "magick-image", "data.frame")
  if (input_type == "text" && !is.character(.data)) {
    stop("This task expects text input.")
  }
  if (input_type == "image" && !inherits(.data, "magick-image")) {
    stop("This task expects image input.")
  }

  task$run(.data, ...)
}
