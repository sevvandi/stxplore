% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cancor_eof.R
\name{cancor_eof.data.frame}
\alias{cancor_eof.data.frame}
\title{Performs Canonical Correlation Analysis  using Empirical Orthogonal Functions from a lagged dataset using a dataframe}
\usage{
\method{cancor_eof}{data.frame}(x, lag = 7, n_eof = 10, values_df, ...)
}
\arguments{
\item{x}{The dataframe or stars object. If it is a dataframe, then it should have the locations.}

\item{lag}{Specifies the lag to be used.}

\item{n_eof}{The number of EOFs to be used.}

\item{values_df}{The dataframe of dimension \code{length(times) x length(locations)} containing the quantity of interest.}

\item{...}{Other arguments that need to be used for datafames  or currently ignored.}
}
\description{
Performs CCA using EOF analysis using in a dataframe
}
\examples{
cancor_eof(x = SSTlonlatshort,
           lag = 7,
           n_eof = 8,
           values_df = SSTdatashort)

}
