#' A function checks if the input is a single string.
#'
#' @description
#'   A function checks if the input is a single string.
#'   It returns a boolean.
#' @param input An object to check
#' @export
#' @return Returns a boolean
#' @examples
#' if (is_single_string(model)) {
#'   print("model gave an error or warning")
#' }
#'
is_single_string <- function(input) {
  is.character(input) & length(input) == 1
}
