#' Plots spatial empirical means computed from spatial_means function
#'
#' This function computes spatial empirical means by latitude and longitude averaged over time.
#'
#' @inheritParams autoplot.temporalmeans
#' @param ylab The label for y axis.
#'
#' @examples
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' sem <- spatial_means(Tmax,
#'        lat_col = "lat",
#'        lon_col = "lon",
#'        t_col = "t",
#'        z_col = "z")
#' autoplot(sem, ylab="Mean Max Temp")
#' @export
autoplot.spatialmeans <- function(object,
                                   ylab = "Mean Value",
                                  ...){

  lat <- lon <- mu_emp <- NULL

  spat_av <- object$spatial_avg
  lat_means <- ggplot(spat_av) +
    geom_point(aes(lat, mu_emp)) +
    theme_bw() +
    xlab("Latitude") +
    ylab(ylab)
  lon_means <- ggplot(spat_av) +
    geom_point(aes(lon, mu_emp)) +
    theme_bw() +
    xlab("Longitude") +
    ylab(ylab)

  ll_means <- gridExtra::grid.arrange(lat_means, lon_means, nrow = 1, ncol = 2, top = "Spatial Empirical Means")
  ll_means
}
