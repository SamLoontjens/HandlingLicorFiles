% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_photosynthetic_induction_parameters.R
\name{calculate_photosynthetic_induction_parameters}
\alias{calculate_photosynthetic_induction_parameters}
\title{A function that calculates all photosynthetic induction parameters
of a single file.}
\usage{
calculate_photosynthetic_induction_parameters(
  pathname,
  manual_check = TRUE,
  save_plot = FALSE,
  decay_tail = FALSE,
  name_parameters = c("date", "description", "light", "relative humidity", "CO2",
    "species", "measurement", "plant")
)
}
\arguments{
\item{pathname}{The licor file that will be analysed.}

\item{manual_check}{A boolean that regulates if the fits are checked.}

\item{save_plot}{A boolean that regulates if the plots are saved. Default is FALSE.}

\item{name_parameters}{A character vector of the parameters in the filename.
Default is a list of date description, light,
relative humidity, CO2, species, measurement and plant.}
}
\value{
Returns a list with a list of the name parameters and a list of
the fitted photosynthetic induction parameters
}
\description{
A function that calculates all photosynthetic induction parameters
  of a file.
  The input is a pathname.
  It slits the path into directories and the filename.
  It splits the filename into name parameters.
  It makes a title for the plots.
  It fits the data using other functions.
  It returns a list of the list of name parameters and the list of fitted
  parameters for both A and Ci.
}
\examples{
pathname <- "input_directory_licorfiles/photosynthetic_induction_data/
             20210226 PI 50-100 75RH 400CO2 T.xlsx"
list_of_parameters <- calculate_photosynthetic_induction_parameters(pathname)

}
\author{
Sam Loontjens
}
