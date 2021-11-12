#' A function that calculates a moving average of a column or vector.
#'
#' @description
#'   A function that calculates a moving or rolling average of a
#'   column or vector.
#'   It returns the new column or vector that is averaged.
#' @param column A column or vector to be averaged.
#' @param averaging_width the amount of neighbouring values to average.
#' @export
#' @return
#' Returns the new column or vector that is moving averaged
#' to overwrite the old one or add as a new column.
#' @examples
#' mydata$A <- moving_average(mydata$A, 5)
#'
#' or
#' mydata$avrA <- moving_average(mydata$A, 5)
#'
moving_average <- function(column, averaging_width){

  #Moving or rolling average
  filter(column, rep(1/averaging_width, averaging_width), sides=1)
}
