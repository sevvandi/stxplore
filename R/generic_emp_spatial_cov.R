#' Generic function to compute empirical spatial covariance
#'
#' Computes empirical spatial covariance by removing trends and examining residuals. It can compute lag-0 or log-1
#' empirical covariance either by latitude or longitude. You can split up the spatial domain by latitude or
#' longitude and plot the covariance for each longitudinal/latitudinal strips.
#'
#' @inheritParams spatial_snapshots
#' @param lat_or_lon_strips Takes the values \code{lat} or \code{lon}. The value \code{lat} produces latitudinal strips,
#'       i.e., covariance plots over longitude for different latitudinal strips. The value \code{lon} produces longitudinal
#'       strips, i.e., covariance plots over latitude for different longitudinal strips.
#' @param quadratic_time If \code{TRUE}  a linear model with quadratic time is fitted and residuals computed. If \code{FALSE}
#'       the model is fitted with linear space and time coefficients.
#' @param quadratic_space  If \code{TRUE}  a linear model with quadratic space is fitted and residuals computed. If \code{FALSE}
#'       the model is fitted with linear space and time coefficients.
#' @param num_strips The number of latitudinal/longitudinal strips to produce. This is used when plotting using autoplot.
#' @param lag Lag can be either 0 or 1.
#'
#' @examples
#' # Dataframe example
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' emp_spatial_cov(Tmax,
#'                 lat_col = "lat",
#'                 lon_col = "lon",
#'                 t_col ="t",
#'                 z_col = "z",
#'                 lat_or_lon_strips = "lon",
#'                 num_strips = 4,
#'                 lag = 1)
#'
#' @importFrom graphics par
#' @importFrom stats cov lm
#' @export emp_spatial_cov
emp_spatial_cov <- function(x,
                            lat_or_lon_strips = "lon",
                            quadratic_time = FALSE,
                            quadratic_space = FALSE,
                            num_strips = 1,
                            lag = 0,
                            ...){
  UseMethod("emp_spatial_cov")
}
