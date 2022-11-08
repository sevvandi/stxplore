#' Generic function to plot spatial snapshots of data through time
#'
#' This function can take either a stars object or a dataframe. Input arguments differ
#' for each case.
#'
#' @param x A stars object or a dataframe. Arguments differ according to the input type.
#' @param xlab The x label.
#' @param ylab The y label.
#' @param title The graph title.
#' @param palette The color palette. Default is \code{Spectral}.
#' @param legend_title The title for the legend.
#' @param ... Other arguments that need to be used for datafames to specify columns.
#'
#'
#' @seealso spatial_snapshots.data.frame
#' @seealso spatial_snapshots.stars
#'
#' @return A ggplot object.
#'
#' @examples
#' library(dplyr)
#' # Dataframe example
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' Tmax_days <- subset(Tmax, t %in% c(1, 15, 30))
#' spatial_snapshots(Tmax_days,
#'   lat_col = 'lat',
#'   lon_col = 'lon',
#'   t_col = 't',
#'   z_col = 'z',
#'   title = "Maximum Temperature for 3 days ")
#'
#' # stars example
#' library(stars)
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' spatial_snapshots(x)
#'
#' @export
#' @importFrom rlang .data
spatial_snapshots <- function(x,
                              xlab = "x",
                              ylab = "y",
                              title = "",
                              palette = "Spectral",
                              legend_title = "z",
                              ...){
  UseMethod("spatial_snapshots")
}
