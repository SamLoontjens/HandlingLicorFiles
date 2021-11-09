#' A function that reads a licorfile.
#'
#' @description
#'   A function that reads a licorfile.
#'   It returns a clean dataframe with only the desired parametes.
#' @author Sam Loontjens
#' @param filename A string with the filename to read.
#' @param sheetnumber An integer which sheet to read from the file.
#' @param parameters A list of desired parameters to extract.
#' @export
#' @return Returns a dataframe with only the desired parameters.
#' @examples
#' parameters_to_extract <- c("A, Ci, elapsed, Qin")
#' mydata <- read_licorfile("20210226 PI 50-100 75RH 400CO2 T.xlsx",
#'                          parameters_to_extract)
#'
read_licorfile <- function(filename, sheetnumber = 1, parameters = TRUE){

  #read licor file
  mydata = data.frame(read_excel(filename, sheet = sheetnumber, col_names = FALSE))

  #find header
  headerrow = which(mydata[1] == 'obs' | mydata[1] == 'Obs')
  header = mydata[c(headerrow), ]

  #skip head
  skiplines = headerrow + 1
  mydata <- mydata[-c(1:skiplines), ]

  #set header
  colnames(mydata) = header

  #check if it is a Li6400 or Li6800
  if (header[1] == 'obs') {
    licormachine <- "LI-6800"
  } else if (header[1] == 'Obs') {
    licormachine <- "LI-6400"
  } else {
    licormachine <- "unkown"
  }

  #print if it is a 6400 or a 6800 file
  print(licormachine)

  #for a Li6400 files
  if (licormachine == "LI-6400") {
    #remove remarks
    remarklist <- which(mydata[1] == 'Remark=')
    mydata <- mydata[-remarklist, ]
    #change PARi to Qin
    names(mydata)[names(mydata) == 'PARi'] <- 'Qin'
    #change Photo to A
    names(mydata)[names(mydata) == 'Photo'] <- 'A'
    #calculate elapsed
    mydata$elapsed <- (as.numeric(mydata$FTime) - as.numeric(mydata$FTime[1]))
  }

  #Select parameters if they are included
  mydata <- extract_parameters(mydata, parameters)

  #change all columns to numeric
  mydata <- numeric_dataframe(mydata)

  return(mydata)
}
