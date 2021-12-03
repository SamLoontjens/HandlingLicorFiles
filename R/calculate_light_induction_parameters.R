#' A function that calculates the light incuction parameters
#' of a single file.
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
calculate_light_induction_parameters <- function(dataframe, min_stepsize = 5){

  #round Qin
  dataframe <- round_light(dataframe)

  #check where the light increases
  light_changes <- which(diff(dataframe$Qin) >= min_stepsize) + 1
  light_induction_index <- light_changes[1]
  start_index <- tail(light_changes, n=1)
  time_zero <- dataframe$elapsed[start_index]

  #find PAR1 and PAR2
  PAR1 <- dataframe$Qin[light_induction_index - 1]
  PAR2 <- dataframe$Qin[light_induction_index + 10]

  #add light_induction index, start index, time zero, PAR1 and PAR2 to the list
  light_induction_parameters <- list(light_induction_index = light_induction_index,
                                   start_index = start_index, time_zero = time_zero,
                                   PAR1 = PAR1, PAR2 = PAR2)


  #check where the light decreases (after the light induction + 10 points)
  negative_light_changes <- which(diff(dataframe$Qin[light_induction_index+10:length(dataframe$Qin)]) <= -min_stepsize) + 1 + light_induction_index+10

  #if there is any decrease in light
  if (length(negative_light_changes) >= 1) {

    #get the light decay index and time
    light_decay_index <- negative_light_changes[1]
    time_end <- dataframe$elapsed[light_decay_index]

    #find PAR3
    PAR3 <- dataframe$Qin[light_decay_index + length(negative_light_changes)]

    #add light decay index and PAR3 to the list
    light_induction_parameters <- c(light_induction_parameters,
                                    list(light_decay_index = light_decay_index,
                                         time_end = time_end,
                                         PAR3 = PAR3))
  }

  #returns a list of lightinductionindex, startindex, timezero, PAR1 and PAR2
  return(light_induction_parameters)
}
