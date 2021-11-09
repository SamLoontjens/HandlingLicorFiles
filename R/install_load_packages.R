#' A function that installs packages.
#'
#' @description A function that installs packages if they are not yet installed.
#'   Then it loads them with the library function.
#' @param packages A list of packages that have to be installed and loaded
#' @export
#' @return Nothing
#' @examples install_load_packages()
#'
install_load_packages <- function(packages = c("ggplot2", "readxl",
                                               "rstudioapi")){

  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}
