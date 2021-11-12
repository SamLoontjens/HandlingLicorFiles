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
fit_photosynthetic_induction_Ci_curve <- function(dataframe, title = "PI Ci curve",
                                                  mean_width = 50, fit_width = 0,
                                                  manual_check = TRUE){
  #get light parameters
  lightinductionparameters <- calculate_light_induction_parameters(dataframe)
  lightinductionindex = lightinductionparameters[[1]]
  start_index = lightinductionparameters[[2]] #unused
  end_index = length(dataframe$Ci)

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
  y <- dataframe$Ci[fit_range]

  #set start parameters
  Ci2 = min(dataframe$Ci)
  k1 = 0.1
  k2 = 0.2
  t0 = 0.5 * (end_index-lightinductionindex)

  if (manual_check) {
    #plot the raw data
    plot(dataframe$elapsed[1:end_index], dataframe$Ci[1:end_index], main = title)

    #make legend
    legend("topright", inset = 0.02, legend=c("Raw data", "Guessed line", "Fitted line"),
           col=c("black", "red", "blue"), lty=c(0, 1, 1), pch = c(1, NA, NA), cex=0.8)

    #make a guessed line
    lines(dataframe$elapsed[fit_range], (Ci1-Ci2) * exp(-k1*t) + (Ci3-Ci2)/(1+exp(-k2*(t-t0))) + Ci2, col = "red")
  }

  #try the model
  model <- tryCatch(
    {
      nls(y ~ (Ci1-Ci2) * exp(-k1*t) + (Ci3-Ci2)/(1+exp(-k2*(t-t0))) + Ci2,
          start = list(Ci2 = Ci2, k1 = 0.1, k2 = 0.2, t0 = 80),
          control = nls.control(maxiter  = 1000, warnOnly = TRUE), trace = TRUE)
    },
    error = function(e) {
      return("error")
    },
    warning = function(w){
      return("warning")
    }
  )

  #if there is an error return a list with no values
  if (is_single_string(model)) {

    if (manual_check) {
      print(paste("Fit gave an", model, "(press ENTER to continue"))
      user_input <- readline()
    }

    fit_parameters <- list(stateCi = "error", Ci1 = NA, Ci2 = NA, Ci3 = NA,
                           deltaCi = NA, k1 = NA, k2 = NA, t0 = NA)
  } else {

    #if there is no error extract the fitted parameters
    Ci2 = coef(model)[[1]]
    k1 = coef(model)[[2]]
    k2 = coef(model)[[3]]
    t0 = coef(model)[[4]]

    #calculate deltaCi
    deltaCi <- Ci1 - Ci2

    if (manual_check) {
      #make a fitted line
      lines(dataframe$elapsed[fit_range], (Ci1-Ci2) * exp(-k1*t) + (Ci3-Ci2)/(1+exp(-k2*(t-t0))) + Ci2, col = "blue")

      #ask user to check if the model is a good fit
      print("Is is a good fit? (Y/N)")
      user_input <- readline()
      if (user_input == "Y") {
        print("Fit accepted")
        fit_parameters <- list(stateCi = "accepted", Ci1 = Ci1, Ci2 = Ci2, Ci3 = Ci3,
                               deltaCi = deltaCi, k1 = k1, k2 = k2, t0 = t0)
      } else {
        print("Fit rejected")
        fit_parameters <- list(stateCi = "rejected", Ci1 = NA, Ci2 = NA, Ci3 = NA,
                               deltaCi = NA, k1 = NA, k2 = NA, t0 = NA)
      }
    } else {
      fit_parameters <- list(stateCi = "not checked", Ci1 = Ci1, Ci2 = Ci2, Ci3 = Ci3,
                             deltaCi = deltaCi, k1 = k1, k2 = k2, t0 = t0)
    }

  }

  return(fit_parameters)
}
