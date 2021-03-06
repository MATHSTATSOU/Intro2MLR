% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myreadxl.R
\name{myreadxl}
\alias{myreadxl}
\title{Read in all excel files}
\usage{
myreadxl(dird = "D:/MATH4773-5773/DATA/Excel/")
}
\arguments{
\item{dird}{This is the directory structure to your excel folder}
}
\value{
A named list of all the data read into R, each component of the list is a data frame
}
\description{
This will work well with reading in all excel files in a particular directory
}
\examples{
\dontrun{v <-myreadxl()}
}
