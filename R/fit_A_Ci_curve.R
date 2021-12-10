#' A function that fits A/Ci curve.
#'
#' @description
#'   A function
#' @author Sam Loontjens
#' @param dataframe A dataframe from which to calculate the fit parameters.
#' @param title A string for the title of the plot
#' @param manual_check
#'   A boolean to regulate if you want to manual check the data
#' @export
#' @return
#'   Returns a list of the check state, A1, A2, deltaA, other constands and t50
#' @examples
#'   parameters <- fit_photosynthetic_induction_A_curve(mydata,
#'                                                      "data from today")
#'
fit_A_Ci_curve <- function(dataframe,
                           O2 = 21,
                           title = "A/Ci curve",
                           subtitle = "",
                           manual_check = TRUE,
                           save_plot = FALSE,
                           save_path = "output_directory_licorfiles/A_Ci_plots/") {

  #testing this dataframe
  dataframe <- data.frame(A = c(33.6, 35.1, 36.1, 36.6, 33.9, 26.9, 17.5, 13.8, 10.0, 6.6, ))

  #start parameters
  Tleaf <- mean(dataframe$TleafCnd)
  Patm <- mean(dataframe$Pa)





}
