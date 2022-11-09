#' Ridgeline plots grouped by an attribute.
#'
#' Plots ridgeline plots grouped by latitude/longitude or time.
#' This function can take either a stars object or a dataframe.
#' Input arguments differ for each case.
#'
#' @inheritParams spatial_snapshots
#' @param num_grps The number of levels for the ridgeline plot.
#'
#' @examples
#' # Dataframe example
#' library(dplyr)
#' data(NOAA_df_1990)
#' TmaxJan <- filter(NOAA_df_1990,
#'                  proc == "Tmax" &
#'                  year == 1993 &
#'                  month == 1)
#' ridgeline(TmaxJan,
#'       group_col = 'lat',
#'       z_col = 'z',
#'       xlab = 'Maximum Temperature',
#'       ylab = 'Latitude Intervals')
#'
#' # stars examples
#' library(stars)
#' library(units)
#'
#' # stars Example 1
#' tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
#' x <- read_stars(tif)
#' dim(x)
#' ridgeline(x, group_dim = 1)
#' ridgeline(x, group_dim = 2)
#'
#'
#' # stars Example 2
#' tif = system.file("tif/lc.tif", package = "stars")
#' x <- read_stars(tif)
#' ridgeline(x, group_dim = 1)
#' ridgeline(x, group_dim = 2)
#'
#' @importFrom ggplot2 scale_fill_viridis_c stat
#' @importFrom ggridges geom_density_ridges_gradient
#' @export
ridgeline <- function(x,
                      num_grps = 10,
                      xlab = "Value",
                      ylab = "Group Intervals",
                      title = "",
                      legend_title = "z",
                      ...){
  UseMethod("ridgeline")
}



