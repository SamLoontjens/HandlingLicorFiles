#' A function that splits a filename of a licorfile.
#'
#' @description
#'   A function that splits a filename of a licorfile.
#'   It returns a list of the parameters included in the filename.
#' @author Sam Loontjens
#' @param filename A filename to split
#' @export
#' @return A list of the parameters included in the filename.
#' @examples
#' split_filepath("20210226 PI 50-100 75RH 400CO2 T.xlsx")
#'
split_licor_filename <- function(filename) {

  #split filename by dot
  splited_on_dot <- strsplit(filename, split = "\\.")[[1]]

  #split filename by space
  splited_on_space <- strsplit(splited_on_dot[1], split = " ")[[1]]

  #make a list of the parameters in the name
  parameterlist <- c("date", "description", "light", "relative humidity", "CO2",
                     "species", "measurement", "plant")

  #fill parameters that are there
  parameters <- list()

  for (i in 1:length(splited_on_space)) {
    parameters[parameterlist[i]] <- splited_on_space[i]
  }

  #fill the rest with NA
  if (length(splited_on_space) < length(parameterlist)) {

    for (i in ((length(splited_on_space)+1):length(parameterlist))) {
      parameters[parameterlist[i]] <- NA
    }
  }

  #add the filetype at the end
  parameters["filetype"] <- splited_on_dot[2]

  #return a list of name parameters
  return(parameters)
}
