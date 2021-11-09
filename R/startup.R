#' A function that runs the reccomended functions before starting analysis.
#'
#' @description
#'   A function that runs the required functions before starting analysis.
#'   It downloads and loads the required packages
#'   It sets the working directory to the path of the source file
#'   It creates directories where you can drop files for analysis
#'   Prints "startup complete" when finished
#' @export
#' @return Returns TRUE if the startup was completed
#' @examples startup()
#'
startup <- function(){

  #load all required packages
  packages = c("ggplot2", "readxl", "rstudioapi")
  install_load_packages(packages)

  #Set working directory to source directory
  setwd(dirname(getActiveDocumentContext()$path))

  #Create directories that are used
  create_directories()

  print("startup complete")

  return(TRUE)
}
