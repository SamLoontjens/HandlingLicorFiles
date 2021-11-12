#' A function that rounds the Qin of a dataframe.
#'
#' @description
#'   A function that rounds the Qin of a dataframe.
#'   It returns a dataframe with the rounded Qin.
#' @author Sam Loontjens
#' @param dataframe A dataframe to round the Qin.
#' @export
#' @return
#' Returns a dataframe with the rounded Qin
#' to overwrite the old one or to add a new dataframe.
#' @examples
#' mydata <- round_light(mydata)
#'
round_light <- function(dataframe){

  #find if there is PAR or Qin to round
  dataframe$Qin <- round_parameter(dataframe$Qin)

  return(dataframe)
}
