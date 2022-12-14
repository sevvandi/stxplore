% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canonical_correlation.R
\name{canonical_correlation.stars}
\alias{canonical_correlation.stars}
\title{Computes transformed variables from Canonical Correlation Analysis using a stars object}
\usage{
\method{canonical_correlation}{stars}(x1, x2, ...)
}
\arguments{
\item{x1}{The first dataframe or stars object.}

\item{x2}{The second dataframe or stars objext. The dimensions of both datasets need to be the same.}

\item{...}{Other arguments that need to be used for datafames or currently ignored.}
}
\description{
Computes Canonical Correlation Analysis (CCA) using 2 datasets.
}
\examples{
library(stars)
tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
x <- read_stars(tif)
x1 <- x[[1]][1:50, 1:50]
x2 <- x[[1]][51:100, 1:50]
stx1 <- st_as_stars(x1)
stx2 <- st_as_stars(x2)
canonical_correlation(stx1, stx2)
}
