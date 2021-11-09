#' A function that extracts desired parameters from a dataframe.
#'
#' @description
#'   A function that extracts desired parameters from a dataframe.
#'   It returns a dataframe with only the desired parameters.
#' @author Sam Loontjens
#' @param dataframe A dataframe to extract from
#' @param parameters A list of parameter names to extract
#' @export
#' @return Returns a dataframe with only the desired parameters.
#' @examples
#' parameters_to_extract <- c("A, Ci, elapsed, Qin")
#' new_dataframe <- extract_parameters(old_dataframe, parameters_to_extract)
#'
extract_parameters <- function(dataframe, parameters){

  #extract from a given dataframe the desired parameter columns
  newdataframe <- dataframe[parameters]

  #return a dataframe with only the desired parameters
  return(newdataframe)
}
