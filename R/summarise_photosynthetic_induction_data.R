#' A function that combines and summarises photosynthetic induction data.
#'
#' @description
#'   ##NOTE: Not completed and tested yet. Needs to be updated for more functionality##
#'   A function that combines the desired parameters of multiple photosynthetic
#'   induction licorfiles and summarises them to calculate the mean and standard
#'   error for a plot.
#' @author Sam Loontjens
#' @param manual_check
#'   A boolean that regulates if the fits are checked.
#'   Default is TRUE.
#' @param write_excel
#'   A boolean that regulates if the dataframe is written to an excel file.
#'   Default is TRUE.
#' @param summarise
#'   A boolean that regulates if the data is summarised.
#'   Default is TRUE.
#' @param save_plot
#'   A boolean that handles if the plot get saved.
#'   Default is TRUE.
#' @export
#' @return
#' Returns a list of dataframes with the combined data and summarised data.
#' @examples
#' mydata <- summarise_photosynthetic_induction_data()
#'
summarise_photosynthetic_induction_data <- function(manual_check = TRUE,
                                                    write_excel = TRUE,
                                                    summarise = TRUE,
                                                    save_plot = TRUE) {
  #make empty data frame
  dataframe_total <- data.frame()

  #check all the files
  file_list <- list_licorfiles("input_directory_licorfiles/photosynthetic_induction_data")

  #calculate the photosynthetic induction parameters for all files
  for (filename in file_list) {

    #create the pathname
    filepath <- paste0("input_directory_licorfiles/",
                       "photosynthetic_induction_data/",
                       filename)

    #read the file
    parameters <- c('elapsed', 'A', 'Ci', 'Qin', 'gsw')
    dataframe <- read_licorfile(filepath = filepath, parameters = parameters)

    #get light parameters
    lightinductionparameters <- calculate_light_induction_parameters(dataframe)
    lightinductionindex <- lightinductionparameters[[1]]
    PAR1 = lightinductionparameters[[4]]
    PAR2 = lightinductionparameters[[5]]

    #if light decrease index exists then that is the end of the data
    if ("light_decay_index" %in% names(lightinductionparameters)) {
      end_index = lightinductionparameters[["light_decay_index"]] - 1
    } else {
      end_index = length(dataframe$gsw)
    }

    #plot data
    gsw <- dataframe[["gsw"]][(lightinductionindex-50):end_index]
    elapsed <- dataframe[["elapsed"]][(lightinductionindex-50):end_index]-dataframe[["elapsed"]][[lightinductionindex]]
    plot(x = elapsed, y = gsw)

    #check manually if the plot is right
    if (manual_check) {
      user_input <- readline(prompt = "(y/n):")
    } else {
      user_input <- "y"
    }


    if (user_input == "y") {
      #create a temporary dataframe with all the parameters
      #round the elapsed so that it can be used for averaging the data
      dataframe_part <- data.frame(
        gsw = gsw,
        elapsed = plyr::round_any(elapsed, 2),
        PAR1 = PAR1,
        PAR2 = PAR2,
        deltaPAR = PAR2 - PAR1
      )

      #add the temporary dataframe to the total dataframe if the plot is accepted
      dataframe_total <- bind_rows(dataframe_total, dataframe_part)

    }
  }

  #add the total dataframe to the output of this function
  output_list <- list(dataframe_total)

  #write the total dataframe as an xlsx excel file
  if (write_excel) {
    writexl::write_xlsx(x = dataframe_total,
                        path = "output_directory_licorfiles/excel_files/set-set_A_all.xlsx",
                        col_names = TRUE,
                        format_headers = FALSE)
  }

  #calculate the means and standard errors for a certain group
  if (summarise) {
    tgc <- Rmisc::summarySE(dataframe_total,
                            measurevar="A",
                            groupvars=c("elapsed", "PAR1", "deltaPAR"))

    #set certain parameteres to factors
    tgc$PAR1 <- as.factor(tgc$PAR1)
    tgc$deltaPAR <- as.factor(tgc$deltaPAR)

    #add the summarised dataframe to the output of this function
    output_list <- c(output_list, list(tgc))

    #write the summarised dataframe as an xlsx excel file
    if (write_excel) {
      writexl::write_xlsx(x = tgc,
                          path = "output_directory_licorfiles/excel_files/set-set_A_average.xlsx",
                          col_names = TRUE,
                          format_headers = FALSE)
    }

    #make a plot of the summarised data
    if(make_plot) {
      current_plot <- ggplot(tgc, aes(elapsed, A)) +
        geom_line(mapping = aes(colour = PAR1)) +
        geom_ribbon(mapping = aes(ymin = A - se, ymax = A + se, fill = PAR1), alpha = 0.5) +
        facet_grid(. ~ deltaPAR) +
        xlim(-50, 1600) #+ ylim(0, 0.75)
      plot(current_plot)

      #save the plot
      if(save_plot) {
        ggsave(filename = "plot.png")
      }
    }
  }

  #return a list of the created dataframes
  return(output_list)
}
