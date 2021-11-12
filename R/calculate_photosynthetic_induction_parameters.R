#' A function that calculates all photosynthetic induction parameters of a file.
#'
#' @description
#'   A function that calculates all photosynthetic induction parameters
#'   of a file.
#'   The input is a pathname.
#'   It slits the path into directories and the filename.
#'   It splits the filename into name parameters.
#'   It makes a title for the plots.
#'   It fits the data using other functions.
#'   It returns a list of the list of name parameters and the list of fitted
#'   parameters for both A and Ci.
#' @author Sam Loontjens
#' @param pathname The licor file that will be analysed.
#' @param manual_check A boolean that regulates if the fits are checked.
#' @export
#' @return
#' Returns a list with a list of the name parameters and a list of
#' the fitted photosynthetic induction parameters
#' @examples
#' pathname <- "input_directory_licorfiles/photosynthetic_induction_data/
#'              20210226 PI 50-100 75RH 400CO2 T.xlsx"
#' list_of_parameters <- calculate_photosynthetic_induction_parameters(pathname)
#'
calculate_photosynthetic_induction_parameters <- function(pathname,
                                                          manual_check = TRUE) {

  #split pathname to find filename
  directory_list <- split_filepath(pathname)
  filename <- directory_list[[length(directory_list)]]

  #read the licorfile with required parameters
  parameters <- c('elapsed','A', 'Ci', 'Qin')
  sheetnumber <- 1
  print(paste("reading:", filename))
  dataframe <- read_licorfile(pathname, sheetnumber, parameters)

  #make titles and fit data
  title_A_curve <- paste("PI A curve:", filename)
  title_Ci_curve <- paste("PI Ci curve:", filename)
  fit_parameters_A <- fit_photosynthetic_induction_A_curve(dataframe = dataframe,
                                                           title = title_A_curve,
                                                           manual_check = manual_check)
  fit_parameters_Ci <- fit_photosynthetic_induction_Ci_curve(dataframe = dataframe,
                                                             title = title_Ci_curve,
                                                             manual_check = manual_check)

  #get name parameters from filename
  name_parameters <- split_licor_filename(filename)
  name_parameters <- name_parameters[names(name_parameters) != "filetype"]

  #add parameter lists together
  induction_parameter_list <- c(name_parameters, fit_parameters_A, fit_parameters_Ci)

  return(induction_parameter_list)
}
