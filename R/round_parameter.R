#' A function that rounds one column or vector of a dataframe.
#'
#' @description
#'   A function that rounds one column or vector of a dataframe.
#'   It returns a new column or vector that is rounded.
#' @param dataframe A dataframe
#' @param parameter A column name that is the parameter to round
#' @export
#' @return Returns a new column or vector that is rounded.
#' @examples
#' mydata$Qin <- round_parameter(mydata$Qin)
#'
round_parameter <- function(dataframe, parameter){
  dataframe$rounded <- round(dataframe[parameter]/10)*10
}
