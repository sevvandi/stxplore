#' Plots spatial empirical means computed from spatial_means function
#'
#' This function computes spatial empirical means by latitude and longitude averaged over time.
#'
#' @inheritParams spatial_snapshots
#' @param object The output from the `spatial_means' function.
#' @param xlab1 The xlabel for the first plot.
#' @param xlab2 The xlabel for the second plot.
#' @param ... Other arguments currently ignored.
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
#' sem <- spatial_means(Tmax,
#'        lat_col = "lat",
#'        lon_col = "lon",
#'        t_col = "t",
#'        z_col = "z")
#' autoplot(sem, ylab="Mean Max Temp")
#'
#'
#' # stars example
#' library(stars)
#' tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
#' x <- read_stars(tif)
#' sp_means <- spatial_means(x)
#' autoplot(sp_means)
#' @export
autoplot.spatialmeans <- function(object,
                                  ylab = "Mean Value",
                                  xlab1 = "Latitude",
                                  xlab2 = "Longitude",
                                  title = "Spatial Empirical Means",
                                  ...){

  x <- y <- mu_emp <- NULL

  spat_av <- object$spatial_avg
  lat_means <- ggplot(spat_av) +
    geom_point(aes(y, mu_emp)) +
    theme_bw() +
    xlab(xlab1) +
    ylab(ylab)
  lon_means <- ggplot(spat_av) +
    geom_point(aes(x, mu_emp)) +
    theme_bw() +
    xlab(xlab2) +
    ylab(ylab)


  if(!is.null(object$data_stars)){
    xx <- object$data_stars
    st_fig <- ggplot() +
      geom_stars(data = xx) +
      scale_fill_distiller(palette = "Spectral",
                         guide = "colourbar")

    ll_means <- gridExtra::grid.arrange(lat_means, lon_means, st_fig, layout_matrix = matrix(c(1, 3, 2, 3), nrow = 2), top = title)
  }else{
    ll_means <- gridExtra::grid.arrange(lat_means, lon_means, nrow = 1, ncol = 2, top = title)

  }



  ll_means
}
