#' A function that fits Ci of a photosynthetic induction.
#'
#' @description
#'   A function that fits Ci of a photosynthetic induction curve.
#'   It fits the data from the point where the light changes.
#'   It calculates Ci1, Ci2 and deltaCi.
#'   It uses nls to fit the data.
#'   You can check if the fit is good.
#'   It returns a list with all the parameters it used to fit the line.
#'   These include Ci1, Ci2, deltaCi and other constands.
#'   If the model gave a warning or error then the parameter values are NA.
#'
#'   It only accepts licor files that have every input parameter stable except
#'   changing the light once. Commonly made with a timed lamp autoprogram with
#'   only two steps. The dataframe has to contain Ci, elapsed and Qin.
#' @author Sam Loontjens
#' @param dataframe A dataframe from which to calculate the fit parameters.
#' @param title A string for the title of the plot
#' @param mean_width An integer of how many points to use for the means.
#' @param fit_width
#'   An integer of how many points to use for the fit.
#'   If fit_width is smaller than mean_width then it takes all points.
#' @param manual_check
#'   A boolean to regulate if you want to manual check the data
#' @export
#' @return
#'   Returns a list of the check state, Ci1, Ci2, deltaCi, other constands
#' @examples
#'   parameters <- fit_photosynthetic_induction_Ci_curve(dataframe,
#'                                                      "data from today")
#'
fit_photosynthetic_induction_Ci_curve <- function(dataframe,
                                                  mean_width = 50,
                                                  fit_width = 0,
                                                  title = "Photosynthetic induction Ci curve",
                                                  subtitle = "",
                                                  manual_check = TRUE,
                                                  save_plot = FALSE,
                                                  save_path = "output_directory_licorfiles/photosynthetic_induction_Ci_plots/"){

  #get light parameters
  lightinductionparameters <- calculate_light_induction_parameters(dataframe)
  lightinductionindex = lightinductionparameters[[1]]
  start_index = lightinductionparameters[[2]] #unused

  #if light decrease index exists then that is the end of the data
  if ("light_decay_index" %in% names(lightinductionparameters)) {
    end_index = lightinductionparameters[["light_decay_index"]] - 1
  } else {
    end_index = length(dataframe$Ci)
  }

  #calculate input parameters C1
  Ci1 <- mean(dataframe$Ci[(lightinductionindex-mean_width):lightinductionindex])

  #find the right range for fitting
  if (fit_width < mean_width) {
    fit_range = (lightinductionindex):(end_index)
  } else {
    fit_range = (lightinductionindex):(lightinductionindex+fit_width)
  }

  #calculate input parameter C3
  if (fit_width < mean_width) {
    Ci3 <- mean(dataframe$Ci[(end_index-mean_width):(end_index)])
  } else {
    Ci3 <- mean(dataframe$Ci[(lightinductionindex+fit_width-mean_width):(lightinductionindex+fit_width)])
  }

  #get t and y after the lightinductionindex
  t <- dataframe$elapsed[fit_range]-dataframe$elapsed[lightinductionindex]
  Ci <- dataframe$Ci[fit_range]

  #set start parameters
  Ci2 = min(Ci)
  k1 = 0.1
  k2 = 0.2
  t0 = 0.5 * length(fit_range)
  start_parameter_list <- list(Ci2 = Ci2, k1 = k1, k2 = k2, t0 = t0)

  #set static parameters
  static_parameter_list <- list(Ci1 = Ci1, Ci3 = Ci3)

  #make formula for fitting
  PIformulaCi <- Ci ~ (Ci1-Ci2) * exp(-k1*t) + (Ci3-Ci2)/(1+exp(-k2*(t-t0))) + Ci2

  #fit the parameters
  fit_parameters <- fit_any_curve(x = t,
                                  y = Ci,
                                  formula = PIformulaCi,
                                  variable_name = "t",
                                  list_of_start_parameters = start_parameter_list,
                                  list_of_static_parameters = static_parameter_list,
                                  title = title,
                                  subtitle = subtitle,
                                  manual_check = manual_check,
                                  save_plot = save_plot,
                                  save_path = save_path,
                                  lower_bounds = c(0, 0.01, 0.001, 0.001),
                                  upper_bounds = c(Ci3 - 0.001, 100, 100, 10000))

  #calculate deltaCi
  Ci1 <- fit_parameters[[5]]
  Ci2 <- fit_parameters[[7]]
  deltaCi <- Ci1 - Ci2

  #add the deltaCi to the list
  fit_parameters <- c(fit_parameters, list(deltaCi = deltaCi))

  return(fit_parameters)
}
