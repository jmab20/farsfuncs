% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{make the file name for the FARs file}
\usage{
make_filename(year)
}
\arguments{
\item{year}{, year}
}
\value{
a string with the name of file
}
\description{
using the year complete the name for the fars file to be read
}
\details{
there is not directory, only name of file, so the default directory is the working directory
Throws an error if non numeric value is provided as Year
}
\examples{
make_filename(2013)

}
