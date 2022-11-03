#' Plots the semi-variogram computed from the semivariogram algorithm
#'
#' A semivariogram plot
#'
#' @param object The output from the semivariogram function.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' library(dplyr)
#' data(locs)
#' data(Times)
#' data(Tmax)
#' temp_part <- with(Times, paste(year, month, day, sep = "-"))
#' temp_part <- data.frame(date = as.Date(temp_part)[913:943])
#' Tmax <- Tmax[913:943, ]
#' semiv <- semivariogram(locs,
#'         temp_part,
#'         Tmax,
#'         latitude_linear = FALSE,
#'         longitude_linear = FALSE,
#'         missing_value = -9999,
#'         width = 50,
#'         cutoff = 1000,
#'         tlagmax = 7 )
#' autoplot(semiv)
#'
#' @export
autoplot.semivariogramobj<- function(object, ...){
  # display the empirical semi-variogram
  vv <- object$variogram
  color_pal <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(16))
  plot(vv, col = color_pal)
}
