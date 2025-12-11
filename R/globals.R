# Declare global variables to avoid R CMD check notes
utils::globalVariables(c("unit_id", "coder_id", "code", "coder"))

# Null-coalescing operator (used throughout package)
#' @noRd
`%||%` <- function(x, y) if (length(x) == 0) y else x
