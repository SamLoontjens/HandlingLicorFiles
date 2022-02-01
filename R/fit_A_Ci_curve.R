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
  dataframe <- data.frame(A = c(33.6, 35.1, 36.1, 36.6, 33.9, 26.9, 17.5, 13.8, 10.0, 6.6, 2.6, -0.3),
                          Ci = c(1750, 1530, 1230, 921, 533, 376, 246, 199, 158, 119, 79, 49.4))
  dataframe$Pa <- 101
  dataframe$TleafCnd <- 25

  current_plot <- ggplot(dataframe, environment = environment()) + geom_point(aes(Ci, A))
  plot(current_plot)

  #start parameters
  Tleaf <- mean(dataframe$TleafCnd)
  Patm <- mean(dataframe$Pa)

  #fit parameters
  Vcmax <- 137
  J <- 175
  TPU <- 11.8
  Rd <- 2.1 # >0
  gm <- 3.48 # <30

  #calculate Ci_Pa
  dataframe$Ci_Pa <- dataframe$Ci * Patm * 0.001

  #caldulate Cc
  dataframe$Cc <- dataframe$Ci_Pa - (dataframe$A / gm)


  #rubisco limited



  return(dataframe)
}
