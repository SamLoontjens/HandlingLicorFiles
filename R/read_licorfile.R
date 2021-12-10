#' A function that reads a licorfile.
#'
#' @description
#'   A function that reads a licorfile.
#'   It returns a clean dataframe with only the desired parametes.
#'
#'   No dots in the file path accepted, except one for the file type.
#'   The functions of this package expect the parameter names of a Li-6800, so
#'   change_names = TRUE is recommended.
#'   Li-6800 manual page 226
#'   Li-6400 manual page 21 (81 of 1324)
#' @author Sam Loontjens
#' @param filepath A string with the filepath to read.
#' @param sheetnumber An integer which sheet to read from the file.
#' @param parameters
#'   A list of desired parameters to extract. Default is everything.
#' @param change_names
#'   A boolean that regulates if the names from the Li-6400 files are changed
#'   to the Li-6800 format. Default is TRUE.
#' @export
#' @return Returns a dataframe with only the desired parameters.
#' @examples
#' parameters_to_extract <- c("A, Ci, elapsed, Qin")
#' dataframe <- read_licorfile("20210226 PI 50-100 75RH 400CO2 T.xlsx",
#'                          parameters_to_extract)
#'
read_licorfile <- function(filepath,
                           sheetnumber = 1,
                           parameters = TRUE,
                           change_names = TRUE) {

  #print which file to read
  print(paste("Reading:", filepath))

  #split the file path
  split_path <- split_filepath(filepath)
  split_name <- split_licor_filename(split_path[["filename"]])

  #read the licor file
  if (is.na(split_name[["filetype"]])) {
    print("filetype: NA (.txt)")
    dataframe <- data.frame(read.delim(file = filepath,
                                   sep = "\t",
                                   header = FALSE,
                                   stringsAsFactors = FALSE,
                                   dec = ".",
                                   skip = 15))

  } else if (split_name[["filetype"]] == "xlsx") {
    print("filetype: .xlsx")
    dataframe <- data.frame(readxl::read_xlsx(filepath,
                                           sheet = sheetnumber,
                                           col_names = FALSE))

  } else if (split_name[["filetype"]] == "xls") {
    print("filetype: .xls")
    try
    dataframe <- data.frame(readxl::read_xls(path.expand(filepath),
                                           sheet = sheetnumber,
                                           col_names = FALSE))

  } else if (split_name[["filetype"]] == "csv") {
    print("filetype: .csv")
    dataframe <- data.frame(readr::read_csv(file = filepath, col_names = FALSE))

  } else {
    #if it is none of the above file types give an error
    stop("Wrong file type: File is not a licorfile")

  }


  #find header
  headerrow <- which(dataframe[1] == 'obs' | dataframe[1] == 'Obs')
  header <- dataframe[c(headerrow), ]

  #skip head
  skiplines <- which(dataframe[1] == '1') - 1
  #old# skiplines <- headerrow + 1
  dataframe <<- dataframe[-c(1:skiplines), ]

  #set header
  colnames(dataframe) <- header

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

    #remove remarks, namely all rows that are not an integer
      #OLD# remarklist <- which(dataframe[1] == 'Remark=')
      #OLD# if (length(remarklist) >= 1) {
      #OLD#   dataframe <- dataframe[-remarklist, ]
      #OLD# }
    remarklist <- suppressWarnings(which(is.na(as.numeric(dataframe[[1]])) == TRUE))
    print(length(remarklist))
    if (length(remarklist) > 0) {
      dataframe <- dataframe[-remarklist, ]
    }

    #change names to new 6800 format
    if (change_names) {
      #change Photo to A
      names(dataframe)[names(dataframe) == 'Photo'] <- 'A'
      #change Trmmol to E
      names(dataframe)[names(dataframe) == 'Trmmol'] <- 'E'
      #change Cond to gsw
      names(dataframe)[names(dataframe) == 'Cond'] <- 'gsw'
      #change Tleaf to TleafCnd
      names(dataframe)[names(dataframe) == 'Tleaf'] <- 'TleafCnd'
      #change CO2S to CO2_s
      names(dataframe)[names(dataframe) == 'CO2S'] <- 'CO2_s'
      #change CO2R to CO2_r
      names(dataframe)[names(dataframe) == 'CO2R'] <- 'CO2_r'
      #change H2OS to H2O_s
      names(dataframe)[names(dataframe) == 'H2OS'] <- 'H2O_s'
      #change H2OR to H2O_r
      names(dataframe)[names(dataframe) == 'H2OR'] <- 'H2O_r'
      #change PARi to Qin
      names(dataframe)[names(dataframe) == 'PARi'] <- 'Qin'
      #change Vpdl to VPDleaf
      names(dataframe)[names(dataframe) == 'VpdL'] <- 'VPDleaf'
      #change Press to Pa
      names(dataframe)[names(dataframe) == 'Press'] <- 'Pa'

      #calculate RHcham
      delta_Pa <- 0
      dataframe$VPcham <- as.numeric(dataframe$H2O_s) * (as.numeric(dataframe$Pa) + delta_Pa) / 1000
      dataframe$SVPcham <- 0.61365 * exp((17.502 * as.numeric(dataframe$Tair)) / (240.97 + as.numeric(dataframe$Tair)))
      dataframe$RHcham <- dataframe$VPcham / dataframe$SVPcham * 100

      #calculate elapsed
      dataframe$elapsed <- (as.numeric(dataframe$FTime) - as.numeric(dataframe$FTime[1]))
    }
  }

  #Select parameters if they are included
  dataframe <- extract_parameters(dataframe, parameters)

  #change all columns to numeric
  dataframe <- numeric_dataframe(dataframe)

  return(dataframe)
}
