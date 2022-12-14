% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emp_orth_fun.R
\name{emp_orth_fun.data.frame}
\alias{emp_orth_fun.data.frame}
\title{Computes empirical orthogonal functions using a dataframe}
\usage{
\method{emp_orth_fun}{data.frame}(x, values_df, ...)
}
\arguments{
\item{x}{The dataframe or stars object. If it is a dataframe, then it should have the locations.}

\item{values_df}{The dataframe of dimension \code{length(times) x length(locations)} containing the quantity of interest.}

\item{...}{Other arguments that need to be used for datafames  or currently ignored.}
}
\description{
Computes empirical orthogonal functions of the data.
}
\examples{
data(SSTlonlatshort)
data(SSTdatashort)
data(SSTlandmaskshort)
# Take first 396 months (33 years) and delete land
delete_rows <- which(SSTlandmaskshort  ==  1)
SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
             SSTdatashort)

}
