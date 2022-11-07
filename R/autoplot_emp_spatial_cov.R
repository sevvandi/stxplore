#' Plots empirical spatial covariance using emp_spatial_cov function
#'
#' You can specify up to 4 latitude or longitude strips in your plot.
#' This lets you split the spatial domain for better visualization.
#'
#' @param object The output of the function `emp_spatial_cov'.
#' @param xlab The label for x-axis.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'                proc == "Tmax" &
#'                month %in% 5:9 &
#'                year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' esv <- emp_spatial_cov(Tmax,
#'                        lat_col = "lat",
#'                        lon_col = "lon",
#'                        t_col ="t",
#'                        z_col = "z",
#'                        lat_or_lon_strips = "lon",
#'                        num_strips = 4,
#'                        lag = 1)
#' autoplot(esv)
#' @export
autoplot.spatialcov <- function(object,
                                xlab = "Latitude",
                                ...){

  Lag_cov <- object$lag_cov
  spat_df <- object$spatial_df
  num_strips <- object$call$num_strips

  if(num_strips == 1){
    plot_cov_strips(Lag_cov, spat_df, xlab = xlab)
  }else if(num_strips == 2){
    op <- par(mfrow = c(1,2))
    on.exit(par(op))
    plot_cov_strips(Lag_cov, spat_df, xlab = xlab)
  }else if(num_strips == 3){
    op <- par(mfrow = c(1,3))
    on.exit(par(op))
    plot_cov_strips(Lag_cov, spat_df, xlab = xlab)
  }else if(num_strips == 4){
    op <- par(mfrow = c(2,2), mai = c(0.8, 0.8, 0.1, 0.3))
    on.exit(par(op))
    plot_cov_strips(Lag_cov, spat_df, xlab = xlab)
  }
}
