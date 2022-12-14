% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_means.R
\name{spatial_means.stars}
\alias{spatial_means.stars}
\title{Computes spatial empirical means using a stars object as input}
\usage{
\method{spatial_means}{stars}(x, ...)
}
\arguments{
\item{x}{A stars object or a dataframe. Arguments differ according to the input type.}

\item{...}{Other arguments that need to be used for datafames  or currently ignored.}
}
\description{
This function computes spatial empirical means by latitude and longitude averaged over time.
}
\examples{
# Example 1
library(stars)
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
prec <- read_ncdf(prec_file)
sp_means <- spatial_means(prec)
sp_means


# Example 2
tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
x <- read_stars(tif)
sp_means <- spatial_means(x)
sp_means

}
