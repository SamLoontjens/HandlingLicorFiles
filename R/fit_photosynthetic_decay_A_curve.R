#' A function that fits A of a photosynthetic decay.
#'
#' @description
#'   A function that fits A of a photosynthetic decay curve.
#'   It fits the data from the point where the light changes.
#'   It calculates A2, A3.
#'   It uses nls to fit the data.
#'   You can check if the fit is good.
#'   It returns a list with all the parameters it used to fit the line.
#'   These include A1, A2, deltaA, other constands and t50.
#'   If the model gave a warning or error then the parameter values are NA.
#'
#'   It only accepts licor files that have every input parameter stable except
#'   changing the light once. Commonly made with a timed lamp autoprogram with
#'   only two steps.The dataframe has to contain A, elapsed and Qin.
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
#'   Returns a list of the check state, A1, A2, deltaA, other constands and t50
#' @examples
#'   parameters <- fit_photosynthetic_induction_A_curve(mydata,
#'                                                      "data from today")
#'
fit_photosynthetic_decay_A_curve <- function(dataframe,
                                             mean_width = 50,
                                             fit_width = 0,
                                             title = "Photosynthetic decay A curve",
                                             subtitle = "",
                                             manual_check = TRUE,
                                             save_plot = FALSE,
                                             save_path = "output_directory_licorfiles/photosynthetic_decay_A_plots/") {

  #get light parameters
  lightinductionparameters <- calculate_light_induction_parameters(dataframe)
  light_decay_index = lightinductionparameters[["light_decay_index"]]
  end_index = length(dataframe$A)

  print(lightinductionparameters)
  print(light_decay_index)
  print(end_index)

  #find the right range for fitting
  if (fit_width < mean_width) {
    fit_range = (light_decay_index):(end_index)
  } else {
    fit_range = (light_decay_index):(light_decay_index+fit_width)
  }

  #get t and A after the light_decay_index
  t <- dataframe$elapsed[fit_range]-dataframe$elapsed[light_decay_index]
  A <- dataframe$A[fit_range]

  #calculate input parameters A2
  A2 <- mean(dataframe$A[(light_decay_index-mean_width):(light_decay_index)])

  #calculate input parameters A3
  if (fit_width < mean_width) {
    A4 <- mean(dataframe$A[(end_index-mean_width):(end_index)])
  } else {
    A4 <- mean(dataframe$A[(light_decay_index+fit_width-mean_width):(light_decay_index+fit_width)])
  }

  #set start parameters
  A3 = min(A)
  k1 = 0.1
  k2 = 0.2
  t0 = 0.5 * length(fit_range)
  start_parameter_list <- list(A3 = A3, k1 = k1, k2 = k2, t0 = t0)

  #set static parameters
  static_parameter_list <- list(A2 = A2, A4 = A4)

  #make formula for fitting
  PIformulaAdecay <- as.formula(A ~ (A2-A3)*exp(-k1*t)+(A4-A3)/(1+exp(-k2*(t-t0)))+A3)

  #fit the parameters
  fit_parameters <- fit_any_curve(x = t,
                                  y = A,
                                  formula = PIformulaAdecay,
                                  variable_name = "t",
                                  list_of_start_parameters = start_parameter_list,
                                  list_of_static_parameters = static_parameter_list,
                                  title = title,
                                  subtitle = subtitle,
                                  manual_check = manual_check,
                                  save_plot = save_plot,
                                  save_path = save_path,
                                  lower_bounds = c(-200, 0.01, 0.001, 0.001),
                                  upper_bounds = c(A4 - 0.001, 100, 100, 10000))

  #return the fittted parameters
  return(fit_parameters)
}
