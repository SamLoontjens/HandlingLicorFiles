#' A function that renames duplicate dataframe column names
#'
#' @description
#'   A function that renames duplicate dataframe column names
#'   Makes it so the database can be used for ggplot which requires
#'   all columns to have a unique name
#' @author Sam Loontjens
#' @param dataframe A dataframe
#' @export
#' @return Returns a dataframe with unique names
#' @examples
#'
unique_dataframe_names <- function(dataframe) {

  #make two empty vectors
  new_names <- c()
  duplicate_names <- c()

  #loop through all the names
  index = 1
  for (i in names(dataframe)) {

    #if the name already occured
    if (i %in% names(dataframe)[1:index-1]) {

      #check how many times is occured
      if (i %in% duplicate_names) {
        counter <- table(duplicate_names)[[i]] + 1
      } else {
        counter <- 1
      }

      #add a _counter to the sting name and add it to the list
      new_names = c(new_names, paste(i, "_", counter, sep = ""))

      #add the dubplicate name to the duplicate list
      duplicate_names <- c(duplicate_names, i)

    } else {
      #if it is no duplicate add it to the list
      new_names = c(new_names, i)

    }

    #add one to the index
    index = index + 1
  }

  #change the column names to the new names
  colnames(dataframe) <- new_names

  #return the dataframe with the new names
  return(dataframe)
}
