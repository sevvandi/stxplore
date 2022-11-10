#' Generic function to compute spatial empirical means
#'
#' This function computes spatial empirical means by latitude and longitude averaged over time.
#' This function can take either a stars object or a dataframe. Input arguments differ for each case.
#'
#' @inheritParams spatial_snapshots
#' @inheritParams spatial_snapshots.data.frame
#'
#' @examples
#' # dataframe example
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' spatial_means(Tmax,
#'        lat_col = "lat",
#'        lon_col = "lon",
#'        t_col = "t",
#'        z_col = "z")
#'
#' # stars examples
#' # Example 1
#' library(stars)
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' sp_means <- spatial_means(prec)
#' sp_means
#'
#'
#' # Example 2
#' tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
#' x <- read_stars(tif)
#' sp_means <- spatial_means(x)
#' sp_means
#' @export spatial_means
spatial_means <- function(x,
                          ...){
  UseMethod("spatial_means")
}
