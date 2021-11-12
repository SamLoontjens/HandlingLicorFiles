#' A function that calculates the light incuction parameters.
#'
#' @description
#'   A function that calculates the light incuction parameters.
#'   It is usefull for photosynthetic induction data.
#'   It returns a list of the lightinductionindex, startindex,
#'   timezero, PAR1 and PAR2.
#'   The lightinductionindex describes the index where the
#'   light changes for the first time.
#'   The startindex is the index where the light is stable again
#'   after the lightinductionindex.
#'   The timezero is the time where the light changes for the first time.
#'   PAR1 is the stable light intensity in the beginning.
#'   PAR2 is the stable light intensity at the end.
#'   Note, only takes increases in light into account.
#' @author Sam Loontjens
#' @param dataframe A dataframe from which to calculate the light parameters.
#' @export
#' @return
#' Returns a list of the lightinductionindex, startindex,
#' timezero, PAR1 and PAR2.
#' @examples
#' list_of_light_parameters <- calculate_light_induction_parameters(mydata, 5)
#'
calculate_light_induction_parameters <- function(dataframe, min_stepsize = 1){

  #round Qin
  dataframe <- roundlight(dataframe)

  #check where the light changes
  lightchanges <- which(diff(dataframe$Qin) >= min_stepsize) + 1
  lightinductionindex <- lightchanges[1]
  startindex <- tail(lightchanges, n=1)
  timezero <- dataframe$elapsed[startindex]

  #find PAR1 and PAR2
  PAR1 <- dataframe$Qin[lightinductionindex - 1]
  PAR2 <- dataframe$Qin[startindex]

  #returns a list of lightinductionindex, startindex, timezero, PAR1 and PAR2
  lightinductionparameters <- list(lightinductionindex = lightinductionindex,
                                   startindex = startindex, timezero = timezero,
                                   PAR1 = PAR1, PAR2 = PAR2)

  return(lightinductionparameters)
}
