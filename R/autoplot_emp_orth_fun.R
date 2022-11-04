#' Plots empirical orthogonal function given from emp_orth_fun
#'
#' Plots either the empirical orthogonal functions or the PC time series for EOFs.
#'
#' @inheritParams spatial_snapshots
#' @param object The output of the function `emp_orth_fun'.
#' @param EOF_num The Empirical Orthogonal Function (EOF) to plot.
#' @param only_EOF If \code{TRUE}, only the spatial EOF function would be plotted.
#' @param only_TS If \code{TRUE}, only the PC time series would be plotted. If both are set to \code{FALSE},
#'                 both plots would be displayed. Both cannot be set to \code{TRUE}.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' data(SSTlonlatshort)
#' data(SSTdatashort)
#' data(SSTlandmaskshort)
# Take first 396 months (33 years) and delete land
#' delete_rows <- which(SSTlandmaskshort  ==  1)
#' SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
#' emp1 <- emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
#'                      SSTdatashort)
#' autoplot(emp1,
#'          EOF_num = 1)
#'
#' autoplot(emp1,
#'          EOF_num = 3,
#'          only_EOF = TRUE)
#'
#' autoplot(emp1,
#'          EOF_num = 2,
#'          only_TS = TRUE)
#'
#' @export
autoplot.emporthfun <- function(object,
                                EOF_num = 1,
                                palette = "Spectral",
                                only_EOF = FALSE,
                                only_TS = FALSE,
                               ...){
  EOF <- NULL
  EOFs <- object$eofs
  TS <- object$pcts
  nPC <- object$pcts$nPC

  variable <- paste("EOF",EOF_num, sep="")
  lon <- 'lon'
  lat <- 'lat'
  # Plot EOF
  eof_p <- ggplot(EOFs)  +  geom_tile(ggplot2::aes_string(x = lon, y = lat, fill = variable))  +
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    theme_bw()  +
    xlab(expression(paste("Longitude", degree)))  +
    ylab(expression(paste("Latitude", degree))) +
    ggtitle(paste("EOF_",EOF_num, sep=""))

  # Plot PC of that EOF
  varriable2 <- paste("X", EOF_num, sep="")
  TS_EOF <- subset(TS, EOF == varriable2)
  pcts_eof <- ggplot(TS_EOF, aes(x=t, y=nPC)) + geom_line(size = 0.8) +
    xlab("Time (t)") +
    ylab("Normalized Principal Component") +
    ggtitle(paste('PC Time series for EOF_', EOF_num, sep=""))

  if(only_EOF){
    eof_p
  }else if(only_TS){
    pcts_eof
  }else{
    gridExtra::grid.arrange(eof_p, pcts_eof)
  }

}

