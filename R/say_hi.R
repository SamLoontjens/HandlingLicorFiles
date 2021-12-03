#' A function to say hi to Sam.
#'
#' @description A function that says hi to Sam and asks for a cookie.
#' @author Sam Loontjens
#' @param snack A string that describes a nice snack
#' @export
#' @return Nothing
#' @examples say_hi("Banana")
#'
say_hi <- function(snack = "cookie") {

  #print a nice string for Sam
  printline <- paste("Say hi to Sam Loontjens and give him a", snack, "!")
  print(printline)
}
