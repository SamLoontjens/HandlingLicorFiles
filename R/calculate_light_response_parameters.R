#' A function that calculates all light response curve parameters of a file.
#'
#' @description
#'   A function that calculates all light response curve parameters of a file.
#'   The input is a pathname.
#'   It slits the path into directories and the filename.
#'   It splits the filename into name parameters.
#'   It makes a title for the plots.
#'   It fits the data using other functions.
#'   It returns a list of the list of name parameters and the list of fitted
#'   parameters.
#' @author Sam Loontjens
#' @param pathname The licor file that will be analysed.
#' @param manual_check A boolean that regulates if the fits are checked.
#' @export
#' @return
#' Returns a list with a list of the name parameters and a list of
#' the fitted light response parameters
#' @examples
#' pathname <- "input_directory_licorfiles/light_response_data/
#'              20210226 LRC NA 75RH 400CO2 T.xlsx"
#' list_of_parameters <- calculate_light_response_parameters(pathname)
#'
calculate_light_response_parameters <- function(pathname, manual_check = TRUE) {

  #split pathname to find filename
  directory_list <- split_filepath(pathname)
  filename <- directory_list[[length(directory_list)]]

  #read the licorfile with required parameters
  parameters <- c('elapsed','A', 'Ci', 'Qin')
  sheetnumber <- 1
  print(paste("reading:", filename))
  mydata <- read_licorfile(pathname, sheetnumber, parameters)

  #make titles and fit data
  title_LRC <- paste("LRC:", filename)
  fit_parameters_LRC <- fit_light_response_curve(mydata = mydata, title = title_LRC,
                                                 manual_check = manual_check)

  #get name parameters from filename
  name_parameters <- split_licor_filename(filename)
  name_parameters <- name_parameters[names(name_parameters) != "filetype"]

  #add parameter lists together
  induction_parameter_list <- c(name_parameters, fit_parameters_LRC)

  return(induction_parameter_list)
}
