#' A function that calculates all photosynthetic induction parameters
#' for multiple files.
#'
#' @description
#'   A function that calculates all photosynthetic induction parameters
#'   for all files.
#'   The directory it reads is
#'   "input_directory_licorfiles/photosynthetic_induction_data/".
#'   For all the files it calculates all the parameters.
#'   It returns a dataframe with all the parameters.
#'
#'   This makes it easy to drop files in the photosynthetic induction data
#'   directory and run just one line to fit all the data.
#' @author Sam Loontjens
#' @param manual_check A boolean that regulates if the fits are checked.
#' @param write_excel A boolean that regulates if the dataframe
#'                    is written to an excel file.
#' @param write_folder a string of the foldername for writing excel files.
#' @param save_plots A boolean that handles if the plots get saved. FALSE default.
#' @param name_parameters
#' A character vector of the parameters in the filename.
#' Default is a list of date description, light,
#' relative humidity, CO2, species, measurement and plant
#' @export
#' @return
#' Returns a dataframe with all the parameters.
#' @examples
#' mydata <- calculate_all_photosynthetic_induction_parameters()
#'
calculate_all_photosynthetic_induction_parameters <- function(manual_check = TRUE,
                                                              write_excel = FALSE,
                                                              write_folder = "output_directory_licorfiles/excel_files/",
                                                              save_plots = FALSE,
                                                              decay_tails = FALSE,
                                                              name_parameters = c("date",
                                                                                  "description",
                                                                                  "light",
                                                                                  "relative humidity",
                                                                                  "CO2",
                                                                                  "species",
                                                                                  "measurement",
                                                                                  "plant")) {

  #list the files that will be used
  photosynthetic_induction_file_list <- list_licorfiles("input_directory_licorfiles/photosynthetic_induction_data")

  #make an empty data frames
  name_dataframe <- data.frame()
  base_dataframe <- data.frame()
  A_dataframe <- data.frame()
  Ci_dataframe <- data.frame()
  photosynthetic_induction_dataframe <- data.frame()

  if (decay_tails) {
    A_decay_dataframe <- data.frame()
    Ci_decay_dataframe <- data.frame()
  }

  #calculate the photosynthetic induction parameters for all files
  for (filename in photosynthetic_induction_file_list) {

    #create the pathname
    input_pathname <- paste0("input_directory_licorfiles/",
                             "photosynthetic_induction_data/",
                             filename)

    #call the function to calculate the parameters
    parameterlist <- calculate_photosynthetic_induction_parameters(pathname = input_pathname,
                                                                   manual_check = manual_check,
                                                                   save_plot = save_plots,
                                                                   decay_tail = decay_tails,
                                                                   name_parameters = name_parameters)

    #merge the rows before the columns, fill with NA
    name_dataframe <- dplyr::bind_rows(name_dataframe,
                                       data.frame(parameterlist[[1]]))
    base_dataframe <- dplyr::bind_rows(base_dataframe,
                                       data.frame(parameterlist[[2]]))
    A_dataframe <- dplyr::bind_rows(A_dataframe,
                                    data.frame(parameterlist[[3]]))
    Ci_dataframe <- dplyr::bind_rows(Ci_dataframe,
                                     data.frame(parameterlist[[4]]))

    if (("fit_parameters_A_decay" %in% names(parameterlist)) && decay_tails) {

      #merge the rows before the colums, fill with NA for the decays
      A_decay_dataframe <- dplyr::bind_rows(A_decay_dataframe,
                                            data.frame(parameterlist[[5]]))
      Ci_decay_dataframe <- dplyr::bind_rows(Ci_decay_dataframe,
                                             data.frame(parameterlist[[6]]))

    } else if (decay_tails) {

      A_decay_dataframe <- dplyr::bind_rows(A_decay_dataframe,
                                            data.frame(list(A_fit = "none")))
      Ci_decay_dataframe <- dplyr::bind_rows(Ci_decay_dataframe,
                                             data.frame(list(Ci_fit = "none")))
    }
  }

  #add all dataframes together
  photosynthetic_induction_dataframe <- cbind(name_dataframe,
                                                         base_dataframe,
                                                         A_dataframe,
                                                         Ci_dataframe)
  if (decay_tails) {
    photosynthetic_induction_dataframe <- cbind(photosynthetic_induction_dataframe,
                                                           A_decay_dataframe,
                                                           Ci_decay_dataframe)
  }

  #write the dataframe to excel for further analysis
  if (write_excel) {

    #create the pathname
    output_pathname <- paste(write_folder, "/", Sys.time(), " PI fitting.xlsx", sep = "")

    #change the colons of the time
    output_pathname <- sub(":", "h", output_pathname)
    output_pathname <- sub(":", "m", output_pathname)

    #writing the xlsx
    writexl::write_xlsx(x = photosynthetic_induction_dataframe,
                        path = output_pathname,
                        col_names = TRUE,
                        format_headers = FALSE)

    #confirmation
    print(paste("Writing", output_pathname))
  }

  #return a dataframe with all the parameters
  return(photosynthetic_induction_dataframe)
}
