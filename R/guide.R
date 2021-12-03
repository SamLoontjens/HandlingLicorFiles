#' A function that prints a guide of this package for the user.
#'
#' @description A function that prints a guide of this package for the user.
#' @author Sam Loontjens
#' @export
#' @examples guide()
#'
guide <- function(){

  function_string <- "functions;
  startup() for loading packages and creating directories.
  read_licorfile() to read licorfiles of an Li-6400 and Li-6800.
  extract_parameters() to reduce your file to only the required columns.
  calculate_lightinductionparameters() to find the time of light induction and the lightsteps.
  etc."

  naming_string <- "Naming convention;
  Reccommended filenames style:               date     name light   RH [CO2] species measurement plant.
  example light response filename:            20211103 LRC  NA      75 400   C       m2          p7
  example photosynthetic induction filename:  20211103 PI   400-600 75 400   T       m3          p4"

  #use cat() or writeLines() to print all the strings
  cat(function_string, naming_string, sep = "\n\n")

}
