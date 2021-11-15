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
#' @param write_excel A boolean that regulates if the dataframe
#'                    is written to an excel file.
#' @param write_folder a string of the foldername for writing excel files.
#' @export
#' @return
#' Returns a dataframe with all the parameters.
#' @examples
#' mydata <- calculate_all_light_response_parameters()
#'
calculate_all_light_response_parameters <- function(manual_check = TRUE,
                                                    write_excel = FALSE,
                                                    write_folder = "output_directory_licorfiles") {

  #list the files that will be used
  light_response_file_list <- list_licorfiles("light_response_data")

  #make an empty data frame
  light_response_dataframe <- data.frame()

  #calculate the photosynthetic induction parameters for all files
  for (filename in light_response_file_list) {

    #create the pathname
    input_pathname <- paste0("input_directory_licorfiles/",
                             "light_response_data/",
                             filename)

    #call the function to calculate the parameters
    parameterlist <- calculate_light_response_parameters(pathname = input_pathname,
                                                         manual_check = manual_check)

    #change the list to a dataframe row
    datarow <- data.frame(parameterlist)

    #add them to the dataframe
    light_response_dataframe <- rbind(light_response_dataframe, datarow)
  }

  #write the dataframe to excel for further analysis
  if (write_excel) {

    #create the pathname
    output_pathname <- paste(write_folder, "/", Sys.time(), " LRC fitting.xlsx", sep = "")

    #change the colons of the time
    output_pathname <- sub(":", "h", output_pathname)
    output_pathname <- sub(":", "m", output_pathname)

    #writing the xlsx
    writexl::write_xlsx(x = light_response_dataframe,
                        path = output_pathname,
                        col_names = TRUE, format_headers = FALSE)

    #confirmation
    print(paste("Write", output_pathname))
  }


  #return a dataframe with all the parameters
  return(light_response_dataframe)
}
