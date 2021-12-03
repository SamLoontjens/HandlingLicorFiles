#' Create directories where you can drop files for analysis.
#'
#' @description Create directories where you can drop files for analysis.
#'   You can drop licorfiles in the
#'   input_directory_licorfiles/light_response_data for lightresponsecurves.
#'   You can drop licorfiles in the
#'   input_directory_licorfiles/photosynthetic_induction_data for
#'   induction curves.
#' @author Sam Loontjens
#' @export
#' @return Nothing
#' @examples create_directories()
#'
create_directories <- function(){

  #Create input directory for all the licorfiles that have to be analysed
  dir.create("input_directory_licorfiles",
             showWarnings = FALSE)

  dir.create("input_directory_licorfiles/light_response_data",
             showWarnings = FALSE)
  dir.create("input_directory_licorfiles/photosynthetic_induction_data",
             showWarnings = FALSE)

  #Create output directory if files have to be written
  dir.create("output_directory_licorfiles",
             showWarnings = FALSE)

  dir.create("output_directory_licorfiles/excel_files",
             showWarnings = FALSE)
  dir.create("output_directory_licorfiles/plots",
             showWarnings = FALSE)

  dir.create("output_directory_licorfiles/light_response_plots",
             showWarnings = FALSE)

  dir.create("output_directory_licorfiles/photosynthetic_induction_A_plots",
             showWarnings = FALSE)
  dir.create("output_directory_licorfiles/photosynthetic_induction_Ci_plots",
             showWarnings = FALSE)

  dir.create("output_directory_licorfiles/photosynthetic_decay_A_plots/",
             showWarnings = FALSE)
  dir.create("output_directory_licorfiles/photosynthetic_decay_Ci_plots/",
             showWarnings = FALSE)

}
