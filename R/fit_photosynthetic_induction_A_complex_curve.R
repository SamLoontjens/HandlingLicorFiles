#' A function that fits A of a photosynthetic induction.
#'
#' @description
#'   A function that fits A of a photosynthetic induction curve.
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
fit_photosynthetic_induction_A_complex_curve <- function(dataframe,
                                                 mean_width = 50,
                                                 fit_width = 0,
                                                 title = "Photosynthetic induction A curve",
                                                 subtitle = "",
                                                 manual_check = TRUE,
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
  t <- (dataframe$elapsed[fit_range]-dataframe$elapsed[lightinductionindex])/60
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
  p = 0.1
  u = 0.2
  o = 0.5*pi
  start_parameter_list <- list(p = p, u = u, o = o)

  #set static parameters
  static_parameter_list <- list(A1 = A1, A2 = A2, deltaA = deltaA)

  #make formula for fitting
  PIformulaA <- as.formula(A ~ (A2-A1)*(1-exp(-p*t)*(sin(u*t+o)/sin(o)))+A1)

  #fit the parameters
  fit_parameters <- fit_any_curve(x = t,
                                  y = A,
                                  formula = PIformulaA,
                                  variable_name = "t",
                                  list_of_start_parameters = start_parameter_list,
                                  list_of_static_parameters = static_parameter_list,
                                  title = title,
                                  subtitle = subtitle,
                                  manual_check = manual_check,
                                  save_plot = save_plot,
                                  save_path = save_path,
                                  lower_bounds = c(0.001, 0.001, 1.25),
                                  upper_bounds = c(10, 0.4, 3.13))

  #return the fittted parameters
  return(fit_parameters)
}
