#' A function that normalizes one column.
#'
#' @description
#'   A function that normalizes one column or vector.
#'   It returns the new column or vector.
#' @author Sam Loontjens
#' @param column A vector (or column) to be normalized
#' @param na.rm A boolean that regulates if the NA values are excluded. Default is FALSE.
#' @export
#' @return
#' Returns a normilized new column
#' to overwrite the old one or add to a new column.
#' @examples
#' mydata$A <- normalize_column(mydata$A)
#'
#' or
#' mydata$normA <- normalize_column(mydata$A)
#'
normalize_column <- function(column, na.rm = FALSE) {

  #normalize the vector into a new vector
  new_column <- (column- min(column, na.rm = na.rm))/(max(column, na.rm = na.rm)-min(column, na.rm = na.rm))

  #return the new vector
  return(new_column)
}
