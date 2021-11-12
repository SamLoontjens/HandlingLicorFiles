#' A function that calculates all photosynthetic induction parameters
#' for all files.
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
#' @export
#' @return
#' Returns a dataframe with all the parameters.
#' @examples
#' mydata <- calculate_all_photosynthetic_induction_parameters()
#'
calculate_all_photosynthetic_induction_parameters <- function(manual_check = TRUE) {

  #list the files that will be used
  photosynthetic_induction_file_list <- list_licorfiles("photosynthetic_induction_data")

  #make an empty data frame
  photosynthetic_induction_dataframe <- data.frame()

  #calculate the photosynthetic induction parameters for all files
  for (filename in photosynthetic_induction_file_list) {
    parameterlist <- calculate_photosynthetic_induction_parameters(pathname = paste0("input_directory_licorfiles/",
                                                                                     "photosynthetic_induction_data/",
                                                                                     filename),
                                                                   manual_check = manual_check)
    datarow <- data.frame(parameterlist)

    #add them to the list
    photosynthetic_induction_dataframe <- rbind(photosynthetic_induction_dataframe, datarow)
  }

  #write to outputfolder?

  return(photosynthetic_induction_dataframe)
}
