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
fit_photosynthetic_induction_A_curve <- function(dataframe,
                                                 title = "PI A curve",
                                                 mean_width = 50,
                                                 fit_width = 0,
                                                 manual_check = TRUE) {
  #get light parameters
  lightinductionparameters <- calculate_light_induction_parameters(dataframe)
  lightinductionindex = lightinductionparameters[[1]]
  startindex = lightinductionparameters[[2]] #unused
  end_index = length(dataframe$A)

  #calculate input parameters A1
  A1 <- mean(dataframe$A[(lightinductionindex-mean_width):lightinductionindex])

  #find the right range for fitting
  if (fit_width < mean_width) {
    fit_range = (lightinductionindex):(end_index)
  } else {
    fit_range = (lightinductionindex):(lightinductionindex+fit_width)
  }

  #get t and y after the lightinductionindex
  t <- dataframe$elapsed[fit_range]-dataframe$elapsed[lightinductionindex]
  y <- dataframe$A[fit_range]

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

  if (manual_check) {
    #plot the raw data
    plot(dataframe$elapsed[1:end_index], dataframe$A[1:end_index], main = title)

    #make legend
    legend("bottomright", inset = 0.02, legend=c("Raw data", "Guessed line", "Fitted line"),
           col=c("black", "red", "blue"), lty=c(0, 1, 1), pch = c(1, NA, NA), cex=0.8)

    #make a guessed line
    lines(dataframe$elapsed[fit_range], ((A2-A1)*t)/(k+t) + A1, col = "red")
  }

  #try the model
  model <<- tryCatch(
    {
      nls(y ~ ((A2-A1)*t)/(k+t) + A1,
          start = list(k = 3),
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

    fit_parameters <- list(stateA = "error", A1 = NA, A2 = NA, deltaA = NA,
                           k = NA)
  } else {

    #if there is no error extract the fitted parameters
    k = coef(model)[[1]]

    if (manual_check) {
      #if there is no error make fitted line with the fitted parameters
      lines(dataframe$elapsed[fit_range], ((A2-A1)*t)/(k+t) + A1, col = "blue")

      #ask user to check if the model is a good fit
      print("Is is a good fit? (Y/N)")
      user_input <- readline()

      if (user_input == "Y") {
        print("Fit accepted")
        fit_parameters <- list(stateA = "accepted", A1 = A1, A2 = A2, deltaA = deltaA,
                               k = k)
      } else {
        print("Fit rejected")
        fit_parameters <- list(stateA = "rejected", A1 = NA, A2 = NA, deltaA = NA,
                               k = NA)
      }
    } else {
      fit_parameters <- list(stateA = "not checked", A1 = A1, A2 = A2, deltaA = deltaA,
                             k = k)
    }

  }

  return(fit_parameters)
}
