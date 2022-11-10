#' Generic function to compute temporal empirical means
#'
#' This function computes temporal empirical means averaged per time unit.
#' This function can take either a stars object or a dataframe.
#' Input arguments differ for each case.
#'
#' @inheritParams temporal_snapshots.data.frame
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
#' temporal_means(Tmax,
#'        t_col = 'date',
#'        z_col = 'z',
#'        id_col = 'id')
#'
#' # stars example
#' library(stars)
#' library(dplyr)
#' library(units)
#' # Example
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' temporal_means(prec)
#' @export temporal_means
temporal_means <- function(x,
                           ...){
  UseMethod("temporal_means")
}
