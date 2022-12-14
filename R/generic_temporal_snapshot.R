#' Generic function to plot temporal snapshots of data for specific spatial locations
#'
#' This function can take either a stars object or a dataframe. Input arguments differ
#' for each case.
#'
#'@inheritParams spatial_snapshots
#'
#'@examples
#'# Dataframe example
#'library(dplyr)
#'data(NOAA_df_1990)
#'Tmax <- filter(NOAA_df_1990,
#'              proc == "Tmax" &
#'              month %in% 5:9 &
#'              year == 1993)
#'Tmax_ID <- unique(Tmax$id)
#'Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#'ids <- sample(Tmax_ID, 10)
#'temporal_snapshots(Tmax,
#'                   t_col = 't',
#'                   z_col = 'z',
#'                   id_col = 'id',
#'                   id_sample = ids)
#'
#'
#'# stars example
#'library(stars)
#'tif = system.file("tif/L7_ETMs.tif", package = "stars")
#'x <- read_stars(tif)
#'xvals <- c(288876.0,289047.0)
#'yvals <- c(9120405, 9120006)
#'temporal_snapshots(x,
#'                   xvals = xvals,
#'                   yvals = yvals)
#'@export
temporal_snapshots <- function(x,
                              xlab = "x",
                              ylab = "y",
                              title = "",
                              ...){
  UseMethod("temporal_snapshots")
}
