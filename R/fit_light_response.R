#' A function that fits a light response curve.
#'
#' @description
#'   A function that fits a light response curve.
#'   The input is a dataframe.
#'   It extracts the columns A, elapsed and Qin.
#'   (These have to be columns in the dataframe or it will result in an error)
#'   It sets the initial parameters Rd, alpha, Pmax, curvature and the
#'   light_compensation_point.
#'   If manual check is on it will plot the data.
#'   It will try to run the model.
#'   The formula is: -Rd + (alpha * PPF + Pmax - sqrt((alpha * PPF + Pmax)^2
#'   - 4 * curvature * PPF * Pmax * alpha))/(2 * curvature).
#'   If manual check is on it will ask the user if the fit is acceptable.
#'   If there is an error or warning it will return a list of NA values.
#'   If the fit is accepted it will return a list with the fitted parameters.
#' @author Sam Loontjens
#' @param mydata The dataframe that will be analysed.
#' @param title A string for the title of the plot.
#' @param manual_check A boolean that regulates if the fits are checked.
#' @export
#' @return Returns a list of the fitted light response parameters
#' @examples
#' fitted_parameters <- fit_light_response_curve(mydata)
#'
fit_light_response_curve <- function(mydata, title = "LRC",
                                     manual_check = TRUE) {

  #get data
  PPF <- mydata$Qin
  Pn <- mydata$A

  #select start parameters
  Rd = 0.66
  alpha = 0.076
  Pmax = 15.2
  curvature = 0.67
  light_compensation_point = 8.8

  if (manual_check) {
    #plot data
    plot(PPF, Pn, main = title)

    #make legend
    legend("bottomright", inset = 0.02, legend=c("Raw data", "Guessed line", "Fitted line"),
           col=c("black", "red", "blue"), lty=c(0, 1, 1), pch = c(1, NA, NA), cex=0.8)

    #make a guessed line
    lines(mydata$Qin, -Rd + (alpha * PPF + Pmax - sqrt((alpha * PPF + Pmax)^2 - 4 * curvature * PPF * Pmax * alpha))/(2 * curvature), col = "red")
  }


  #try the model
  model <- tryCatch(
    {
      nls(Pn ~ -Rd + (alpha * PPF + Pmax - sqrt((alpha * PPF + Pmax)^2 - 4 * curvature * PPF * Pmax * alpha))/(2 * curvature),
          start = list(Rd = Rd, alpha = alpha, Pmax = Pmax, curvature = curvature),
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
      print(paste("Fit gave an", model, "(press ENTER to continue)"))
      user_input <- readline()
    }

    fit_parameters <- list(stateLRC = "error", Rd = NA, alpha = NA, Pmax = NA, curvature = NA,
                           light_compensation_point = NA)
  } else {

    #if there is no error extract the fitted parameters
    Rd = coef(model)[[1]]
    alpha = coef(model)[[2]]
    Pmax = coef(model)[[3]]
    curvature = coef(model)[[4]]
    light_compensation_point = (curvature * Rd^2 - Rd * Pmax) / (Rd * alpha - alpha * Pmax)

    if (manual_check) {
      #if there is no error make fitted line with the fitted parameters
      lines(PPF, -Rd + (alpha * PPF + Pmax - sqrt((alpha * PPF + Pmax)^2 - 4 * curvature * PPF * Pmax * alpha))/(2 * curvature), col = "blue")

      #ask user to check if the model is a good fit
      print("Is is a good fit? (Y/N)")
      user_input <- readline()

      if (user_input == "Y") {
        print("Fit accepted")
        fit_parameters <- list(stateLRC = "accepted", Rd = Rd, alpha = alpha, Pmax = Pmax, curvature = curvature,
                               light_compensation_point = light_compensation_point)
      } else {
        print("Fit rejected")
        fit_parameters <- list(stateLRC = "rejected", Rd = NA, alpha = NA, Pmax = NA, curvature = NA,
                               light_compensation_point = NA)
      }
    } else {
      fit_parameters <- list(stateLRC = "not checked", Rd = Rd, alpha = alpha, Pmax = Pmax, curvature = curvature,
                             light_compensation_point = light_compensation_point)
    }
  }

  #return a list of fitted parametes
  return(fit_parameters)
}
