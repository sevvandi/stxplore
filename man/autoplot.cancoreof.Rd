% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot_cancor_eof.R
\name{autoplot.cancoreof}
\alias{autoplot.cancoreof}
\title{Plots Canonical Correlation Analysis from cancor_eof function.}
\usage{
\method{autoplot}{cancoreof}(
  object,
  line_plot = TRUE,
  space_plot = TRUE,
  palette = "Spectral",
  xlab = "Time",
  ...
)
}
\arguments{
\item{object}{The output of the function `cancor_eof'.}

\item{line_plot}{If set to \code{TRUE}, then the line plot is included.}

\item{space_plot}{If set to \code{TRUE}, the space splot is included.}

\item{palette}{The color palette to use for plotting.}

\item{xlab}{The label on the x-axis for the line plot.}

\item{...}{Other arguments currently ignored.}
}
\description{
Plots CCA using EOF analysis using a lagged dataset.
}
\examples{
cca <- cancor_eof(x = SSTlonlatshort,
                 n_eof = 8,
                 values_df = SSTdatashort)
autoplot(cca)
autoplot(cca, line_plot = FALSE)
autoplot(cca, space_plot = FALSE)
}
