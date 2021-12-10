#' A function that fits multiple curves for A of a photosynthetic induction.
#'
#' @description
#'   A function that fits multiple curves for A of a photosynthetic induction.
#'   It fits the data from the point where the light changes.
#'   It calculates A1, A2 and deltaA.
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
fit_photosynthetic_induction_A_multiple_curves <- function(dataframe,
                                                            mean_width = 50,
                                                            fit_width = 0,
                                                            title = "Photosynthetic induction A curve",
                                                            subtitle = "",
                                                            show_try = FALSE,
                                                            save_plot = FALSE,
                                                            save_path = "output_directory_licorfiles/photosynthetic_induction_A_plots/") {

  #get light parameters
  lightinductionparameters <- calculate_light_induction_parameters(dataframe)
  lightinductionindex = lightinductionparameters[[1]]
  startindex = lightinductionparameters[[2]] #unused

  #if light decrease index exists then that is the end of the data
  if ("light_decay_index" %in% names(lightinductionparameters)) {
    end_index = lightinductionparameters[["light_decay_index"]] - 1
  } else {
    end_index = length(dataframe$A)
  }

  #calculate input parameters A1
  A1 <- mean(dataframe$A[(lightinductionindex-mean_width):lightinductionindex])

  #find the right range for fitting
  if (fit_width < mean_width) {
    fit_range = (lightinductionindex):(end_index)
  } else {
    fit_range = (lightinductionindex):(lightinductionindex+fit_width)
  }

  print(paste("end_index", end_index, "fit range", length(fit_range)))

  #get t and A after the lightinductionindex
  t <- dataframe$elapsed[fit_range]-dataframe$elapsed[lightinductionindex]
  A <- dataframe$A[fit_range]

  #calculate input parameters A2
  if (fit_width < mean_width) {
    A2 <- mean(dataframe$A[(end_index-mean_width):(end_index)])
  } else {
    A2 <- mean(dataframe$A[(lightinductionindex+fit_width-mean_width):(lightinductionindex+fit_width)])
  }

  #calculate deltaA
  deltaA <- A2 - A1

  #normalize data?

  #set start parameters
  k = 3
  start_parameter_list_1 <- list(k = k)
  start_parameter_list_2 <- list(k = k)
  start_parameter_list_3 <- list(a = 0.7, b = 0.7)
  list_of_start_parameters_lists <- list(start_parameter_list_1,
                                         start_parameter_list_2,
                                         start_parameter_list_3)

  #set static parameters
  static_parameter_list_1 <- list(A1 = A1, A2 = A2, deltaA = deltaA)
  static_parameter_list_2 <- list(A1 = A1, A2 = A2, deltaA = deltaA)
  static_parameter_list_3 <- list(A1 = A1, A2 = A2, deltaA = deltaA)
  list_of_static_parameters_lists <- list(static_parameter_list_1,
                                          static_parameter_list_2,
                                          static_parameter_list_3)

  #set lower bounds
  lower_bounds_1 = c(0.01)
  lower_bounds_2 = c(0.01)
  lower_bounds_3 = c(0.1, 0.01)
  list_of_lower_bounds <- list(lower_bounds_1,
                               lower_bounds_2,
                               lower_bounds_3)

  #set upper bounds
  upper_bounds_1 = c(1000)
  upper_bounds_2 = c(1000)
  upper_bounds_3 = c(1000, 0.99)
  list_of_upper_bounds <- list(upper_bounds_1,
                               upper_bounds_2,
                               upper_bounds_3)

  #make formula for fitting
  Michaelis_Menten <- as.formula(A ~ ((A2-A1)*t)/(k+t) + A1)
  exponential <- as.formula(A ~ (A2-A1)-(A2-A1)*exp(-k*t) + A1)
  qubic <- as.formula(A ~ ((a*t+(A2-A1))-((a*x+(A2-A1))^2-4*a*b*t*(A2-A1))^0.5)/(2*b) + A1)
  formula_list <- list(Michaelis_Menten,
                       exponential,
                       qubic)

  #fit the parameters
  fit_parameters <- fit_multiple_curves(x = t,
                                        y = A,
                                        formula_list = formula_list,
                                        variable_name = "t",
                                        list_of_start_parameters_lists = list_of_start_parameters_lists,
                                        list_of_static_parameters_lists = list_of_static_parameters_lists,
                                        title = title,
                                        subtitle = subtitle,
                                        save_plot = save_plot,
                                        save_path = save_path,
                                        list_of_lower_bounds = list_of_lower_bounds,
                                        list_of_upper_bounds = list_of_upper_bounds,
                                        show_try = show_try)

  #return the fittted parameters
  return(fit_parameters)
}
