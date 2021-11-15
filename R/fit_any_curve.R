#' A function that fits any curve.
#'
#' @description
#'   A function
#' @author Sam Loontjens
#' @param
#' @export
#' @return
#' @examples
#' parameters <- fit_any_curve(x = 1:10,
#'                             y = 1:10 +5 +rnorm(10),
#'                             formula = as.formula(banaan ~ k * x + p),
#'                             list_of_start_parameters = list(k = 2, p = 3))
#'
fit_any_curve <- function(x, y,
                          formula = as.formula(banaan ~ k * x + e),
                          list_of_start_parameters = list(a = 2, b = 5),
                          title = "fit",
                          manual_check = TRUE,
                          save_plot = FALSE) {

  #get the formula strings
  left_hand_side <- as.character(formula[2])
  right_hand_side <- as.character(formula[3])

  #set parameters for output
  fit_parameters <- list()
  state_string <- paste(left_hand_side, "state", sep = "_")

  #get and assign the parameters from the list
  for (i in 1:(length(list_of_start_parameters))) {
    assign(names(list_of_start_parameters[i]), list_of_start_parameters[[i]])
    print(i)
  }

  #calculate the initial y values for the guessed line
  y_initial <- eval(parse(text = right_hand_side))

  if (manual_check) {
    #plot data
    plot(x, y, main = title)

    #make legend
    legend("bottomright", inset = 0.02, legend=c("Raw data", "Guessed line", "Fitted line"),
           col=c("black", "red", "green"), lty=c(0, 1, 1), pch = c(1, NA, NA), cex=0.8)

    #make a guessed line
    lines(x, y_initial, col = "red")
  }

  #try the model
  model <- tryCatch(
    {
      nls(formula = formula,
          start = list_of_start_parameters,
          control = nls.control(maxiter  = 1000,
                                warnOnly = TRUE),
          trace = TRUE)
    },
    error = function(e) {
      return("error")
    },
    warning = function(w){
      return("warning")
    }
  )

  #if there is an error or warning return a list with NA values
  if (is_single_string(model)) {

    if (manual_check) {
      print(paste("Fit gave an", model, "(press ENTER to continue)"))
      user_input <- readline()
    }

    fit_parameters[state_string] <- "error"
    new_parameters <- replace(list_of_start_parameters, values = NA)


  } else {

    #if there is no error
    if (manual_check) {

      #make fitted line with the fitted parameters
      lines(x, fitted(model), col = "green")

      #ask user to check if the model is a good fit
      print("Is is a good fit? (Y/N)")
      user_input <- readline()

      if (user_input == "Y") {

        #if the fit is accepted
        print("Fit accepted")
        fit_parameters[state_string] <- "accepted"
        new_parameters <- split(unname(coef(model)),names(coef(model)))

      } else {

        #if the fit is rejected
        print("Fit rejected")
        fit_parameters[state_string] <- "rejected"
        new_parameters <- replace(list_of_start_parameters, values = NA)
      }
    } else {

      #if the fit is not manually checked
      fit_parameters[state_string] <- "not checked"
      new_parameters <- split(unname(coef(model)),names(coef(model)))
    }
  }

  #return a list of all the fitted parameters
  fit_parameters <- c(fit_parameters, new_parameters)
  return(fit_parameters)
}
