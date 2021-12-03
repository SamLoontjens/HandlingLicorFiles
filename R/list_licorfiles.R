#' A function that lists the licorfiles in the input directory
#'
#' @description
#'   A function that lists the licorfiles in the input directory.
#'   You can specify more directories to be more specific
#' @param directory A directory name in the input directory
#'   to read for licorfiles
#' @export
#' @return Returns a list of licorfile names in the given directory
#' @examples list_licorfiles("photosynthetic_induction_data")
#'
list_licorfiles <- function(pathname = "input_directory_licorfiles/"){

  ##TODO#list all the licorfiles in that directory

  #idea: do something to only list licorfiles and not the rest of the files?
  list_of_licorfiles <- list.files(path = pathname)

  #Returns a list of licorfiles in the given directory
  return(list_of_licorfiles)
}
