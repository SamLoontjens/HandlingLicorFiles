#' A function that normalizes one column.
#'
#' @description
#'   A function that normalizes one column or vector.
#'   It returns the new column or vector.
#' @author Sam Loontjens
#' @param column A vector (or column) to be normalized
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
normalize_column <- function(column) {

  #normalize the vector into a new vector
  new_column <- (column- min(column))/(max(column)-min(column))

  #return the new vector
  return(new_column)
}
