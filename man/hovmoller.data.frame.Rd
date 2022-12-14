% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hovmoller.R
\name{hovmoller.data.frame}
\alias{hovmoller.data.frame}
\title{Computes the data structure for the Hovmoller plots using a dataframe as input}
\usage{
\method{hovmoller}{data.frame}(
  x,
  lat_or_lon = "lat",
  xlen = NULL,
  lat_or_lon_col,
  t_col,
  z_col,
  ...
)
}
\arguments{
\item{x}{A stars object or a dataframe. Arguments differ according to the input type.}

\item{lat_or_lon}{Needs to be either \code{lat} or \code{lon}. \code{lat} plots the latitudinal
Hovmoller plat, while \code{lon} plots the longitudinal Hovmoller plot.}

\item{xlen}{The length of the xaxis for latitude/longitude.}

\item{lat_or_lon_col}{The column or the column name corresponding to the latitude/longitude.}

\item{t_col}{The time column. Time must be a set of discrete integer values.}

\item{z_col}{The The quantity of interest that will be plotted. Eg. temperature.}

\item{...}{Other arguments that need to be used for datafames  or currently ignored.}
}
\description{
This function creates the data structure for Hovmoller plots for either latitude or longitude.
}
\examples{
library(dplyr)
data(NOAA_df_1990)
Tmax <- filter(NOAA_df_1990,
  proc == "Tmax" &
  month \%in\% 5:9 &
  year == 1993)
Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
hovmoller(lat_or_lon = "lat",
          x = Tmax,
          lat_or_lon_col = 'lat',
          t_col = 't',
          z_col = 'z')

}
