#' A function that calculates all photosynthetic induction parameters
#' of a single file.
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
#' @param save_plot A boolean that regulates if the plots are saved. Default is FALSE.
#' @param name_parameters
#' A character vector of the parameters in the filename.
#' Default is a list of date description, light,
#' relative humidity, CO2, species, measurement and plant.
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
                                                          manual_check = TRUE,
                                                          save_plot = FALSE,
                                                          decay_tail = FALSE,
                                                          name_parameters = c("date",
                                                                              "description",
                                                                              "light",
                                                                              "relative humidity",
                                                                              "CO2",
                                                                              "species",
                                                                              "measurement",
                                                                              "plant")) {

  #split pathname to find filename
  directory_list <- split_filepath(pathname)
  filename <- directory_list[[length(directory_list)]]

  #make the subtitle the filename without the extension
  subtitle <- strsplit(x = filename, split = "\\.")[[1]][1]

  #read the licorfile with required parameters
  parameters <- c('elapsed', 'A', 'Ci', 'Qin', 'TleafCnd', "RHcham", "CO2_r")
  sheetnumber <- 1
  dataframe <- read_licorfile(filepath = pathname,
                              sheetnumber = sheetnumber,
                              parameters = parameters)

  #check if there is a decay at the end
  lightinductionparameters <- calculate_light_induction_parameters(dataframe)
  lightinductionindex <- lightinductionparameters[[1]]

  if (("light_decay_index" %in% names(lightinductionparameters)) && decay_tail) {
    decay_tail <- TRUE
  } else {
    decay_tail <- FALSE
  }

  #fit data for A
  fit_parameters_A <- fit_photosynthetic_induction_A_curve(dataframe = dataframe,
                                                           subtitle = subtitle,
                                                           manual_check = manual_check,
                                                           save_plot = save_plot)
  #fit data for Ci
  fit_parameters_Ci <- fit_photosynthetic_induction_Ci_curve(dataframe = dataframe,
                                                             subtitle = subtitle,
                                                             manual_check = manual_check,
                                                             save_plot = save_plot)

  #get name parameters from filename
  name_parameters <- split_licor_filename(filename = filename,
                                          name_parameters = name_parameters)
  name_parameters <- name_parameters[names(name_parameters) != "filetype"]

  #get the base parameters from the file
  PAR1 <- lightinductionparameters[["PAR1"]]
  PAR2 <- lightinductionparameters[["PAR2"]]
  if ("PAR3" %in% names(lightinductionparameters)) {
    PAR3 <- lightinductionparameters[["PAR3"]]
  } else {
    PAR3 = NA
  }
  avg_leaf_temp <- mean(dataframe$TleafCnd)
  temp1 <- mean(dataframe$TleafCnd[1:50])
  temp2 <- mean(dataframe$TleafCnd[(lightinductionindex+10):(lightinductionindex+60)])
  delta_temp = temp2 - temp1
  avg_RH <- mean(dataframe$RHcham)
  avg_CO2 <- mean(dataframe$CO2_r)
  base_parameters <- list(PAR1 = PAR1,
                          PAR2 = PAR2,
                          PAR3 = PAR3,
                          temp1 = temp1,
                          temp2 = temp2,
                          delta_temp = delta_temp,
                          avg_temp = avg_leaf_temp,
                          avg_RH = avg_RH,
                          avg_CO2 = avg_CO2)

  #add parameter lists together
  induction_parameter_list <- list(name_parameters = name_parameters,
                                   base_parameters = base_parameters,
                                   fit_parameters_A = fit_parameters_A,
                                   fit_parameters_Ci = fit_parameters_Ci)
  if (decay_tail) {
    #fit data for decay A
    fit_parameters_A_decay <- fit_photosynthetic_decay_A_curve(dataframe = dataframe,
                                                               subtitle = subtitle,
                                                               manual_check = manual_check,
                                                               save_plot = save_plot)
    #fit data for decay Ci
    fit_parameters_Ci_decay <- fit_photosynthetic_decay_Ci_curve(dataframe = dataframe,
                                                               subtitle = subtitle,
                                                               manual_check = manual_check,
                                                               save_plot = save_plot)
    #add the decay parameter lists
    induction_parameter_list <- c(induction_parameter_list,
                                  list(fit_parameters_A_decay = fit_parameters_A_decay,
                                       fit_parameters_Ci_decay = fit_parameters_Ci_decay))
  }

  return(induction_parameter_list)
}
