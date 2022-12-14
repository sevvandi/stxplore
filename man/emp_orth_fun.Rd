% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_emp_orth_fun.R
\name{emp_orth_fun}
\alias{emp_orth_fun}
\title{Computes empirical orthogonal functions}
\usage{
emp_orth_fun(x, ...)
}
\arguments{
\item{x}{The dataframe or stars object. If it is a dataframe, then it should have the locations.}

\item{...}{Other arguments that need to be used for datafames  or currently ignored.}
}
\description{
Computes empirical orthogonal functions of the data.
}
\examples{
# Dataframe example
data(SSTlonlatshort)
data(SSTdatashort)
data(SSTlandmaskshort)
# Take first 396 months (33 years) and delete land
delete_rows <- which(SSTlandmaskshort  ==  1)
SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
             SSTdatashort)

}
