#' Generic function to compute the data structure for Hovmoller plots
#'
#' This function creates the data structure for Hovmoller plots for either latitude or longitude.
#' This function can take either a stars object or a dataframe. Input arguments differ for each case.
#'
#' @inheritParams spatial_snapshots
#' @param lat_or_lon Needs to be either \code{lat} or \code{lon}. \code{lat} plots the latitudinal
#' Hovmoller plat, while \code{lon} plots the longitudinal Hovmoller plot.
#' @param xlen The length of the xaxis for latitude/longitude.
#'
#' @examples
#' # dataframe examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' hovmoller(lat_or_lon = "lat",
#'           x = Tmax,
#'           lat_or_lon_col = 'lat',
#'           t_col = 't',
#'           z_col = 'z')
#'
#' # stars examples
#' library(stars)
#' @importFrom rlang .data
#' @export hovmoller
hovmoller <- function(x,
                      lat_or_lon ="lat",
                      xlen = NULL,
                      ...){
  UseMethod("hovmoller")
}
