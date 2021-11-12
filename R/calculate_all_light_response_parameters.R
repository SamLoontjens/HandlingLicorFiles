#' A function that calculates all light response curve parameters for all files.
#'
#' @description
#'   A function that calculates all light response curve parameters
#'   for all files.
#'   The directory it reads is
#'   "input_directory_licorfiles/light_response_data/".
#'   For all the files it calculates all the parameters.
#'   It returns a dataframe with all the parameters.
#'
#'   This makes it easy to drop files in the light response data directory
#'   and run just one line to fit all the data.
#' @author Sam Loontjens
#' @param manual_check A boolean that regulates if the fits are checked.
#' @export
#' @return
#' Returns a dataframe with all the parameters.
#' @examples
#' mydata <- calculate_all_light_response_parameters()
#'
calculate_all_light_response_parameters <- function(manual_check = TRUE) {

  #list the files that will be used
  light_response_file_list <- list_licorfiles("light_response_data")

  #make an empty data frame
  light_response_dataframe <- data.frame()

  #calculate the photosynthetic induction parameters for all files
  for (filename in light_response_file_list) {
    parameterlist <- calculate_light_response_parameters(pathname = paste0("input_directory_licorfiles/",
                                                                           "light_response_data/",
                                                                           filename),
                                                         manual_check = manual_check)
    datarow <- data.frame(parameterlist)

    #add them to the dataframe
    light_response_dataframe <- rbind(light_response_dataframe, datarow)
  }

  #write to outputfolder?

  #return a dataframe with all the parameters
  return(light_response_dataframe)
}
