#' A function that changes all columns of a dataframe to numeric.
#'
#' @description
#'   A function that changes all columns of a dataframe to numeric.
#'   It returns the new dataframe that is completely numeric.
#' @author Sam Loontjens
#' @param dataframe A dataframe to make numeric.
#' @export
#' @return
#' Returns the new dataframe that is completely numeric
#' to overwrite the old one or add to a new dataframe.
#' @examples
#' mydata <- numeric_dataframe(mydata)
#'
numeric_dataframe <- function(dataframe){

  #apply to all the columns of dataframe the function as.numeric
  dataframe <- as.data.frame(apply(dataframe, 2, as.numeric))

  return(dataframe)
}
