% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hovmoller.R
\name{hovmoller.stars}
\alias{hovmoller.stars}
\title{Computes the data structure for the Hovmoller plots using a stars object as input}
\usage{
\method{hovmoller}{stars}(x, lat_or_lon = "lat", xlen = NULL, ...)
}
\arguments{
\item{x}{A stars object or a dataframe. Arguments differ according to the input type.}

\item{lat_or_lon}{Needs to be either \code{lat} or \code{lon}. \code{lat} plots the latitudinal
Hovmoller plat, while \code{lon} plots the longitudinal Hovmoller plot.}

\item{xlen}{The length of the xaxis for latitude/longitude.}

\item{...}{Other arguments that need to be used for datafames  or currently ignored.}
}
\description{
This function creates the data structure for Hovmoller plots for either latitude or longitude.
}
\examples{
library(stars)
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
prec <- read_ncdf(prec_file)
hov <- hovmoller(prec)
hov

}
