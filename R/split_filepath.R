#' A function that splits a filepath.
#'
#' @description
#'   A function that splits a filepath.
#'   It returns a list of the directories and the filename.
#' @author Sam Loontjens
#' @param pathname A pathname to split
#' @export
#' @return A list with directory names and a filename at the end
#' @examples
#' split_filepath("/input_directory_licorfiles/photosynthetic_induction_data/
#'                20210226 PI 50-100 75RH 400CO2 T.xlsx")
#'
split_filepath <- function(pathname) {

  #split pathname by forwardslash
  splited_on_forwardslash <- strsplit(pathname, split = "/")[[1]]

  #add the items to a list
  pathlist <- list()

  for (i in 1:(length(splited_on_forwardslash)-1)) {
    dirname <- paste0("dir", as.character(i))
    pathlist[dirname] <- splited_on_forwardslash[i]
  }

  #the last item is the filename
  filename <- splited_on_forwardslash[length(splited_on_forwardslash)]
  pathlist["filename"] <- filename

  #return a list with all directory names and a filename at the end
  return(pathlist)
}
