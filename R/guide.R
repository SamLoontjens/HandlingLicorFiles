#' A function that prints a guide of this package for the user.
#'
#' @description A function that prints a guide for the user.
#' @author Sam Loontjens
#' @param
#' @export
#' @return Nothing
#' @examples guide()
#'
guide <- function(){

  print("functions")
  print("startup() for loading packages and creating directories")
  print("read_licorfile() to read licorfiles of an Li-6400 and Li-6800")
  print("extract_parameters() to reduce your file to only the required columns")
  print("calculate_lightinductionparameters() to find the time of light induction and the lightsteps")
  print("etc.")

  print("(Naming convention) filenames should have the style: date     name light   RH [CO2] species measurement plant")
  print("example light response filename:                     20211103 LRC  NA      75 400   C       m2          p7")
  print("example photosynthetic induction filename:           20211103 PI   400-600 75 400   T       m3          p4")

}
