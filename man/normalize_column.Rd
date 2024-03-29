% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize_column.R
\name{normalize_column}
\alias{normalize_column}
\title{A function that normalizes one column.}
\usage{
normalize_column(column, na.rm = FALSE)
}
\arguments{
\item{column}{A vector (or column) to be normalized}

\item{na.rm}{A boolean that regulates if the NA values are excluded. Default is FALSE.}
}
\value{
Returns a normilized new column
to overwrite the old one or add to a new column.
}
\description{
A function that normalizes one column or vector.
  It returns the new column or vector.
}
\examples{
mydata$A <- normalize_column(mydata$A)

or
mydata$normA <- normalize_column(mydata$A)

}
\author{
Sam Loontjens
}
