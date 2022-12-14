#' Hovmoller plots for latitude or longitude
#'
#' This function creates a Hovmoller plot for either latitude or longitude.
#'
#' @inheritParams spatial_snapshots
#' @param object The output of the function `hovmoller'.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' # dataframe example
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' hov <- hovmoller(lat_or_lon = "lat",
#'           x = Tmax,
#'           lat_or_lon_col = 'lat',
#'           t_col = 't',
#'           z_col = 'z')
#' autoplot(hov, legend_title = 'Temperature')
#'
#'
#' # stars example
#' library(stars)
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' hov <- hovmoller(prec)
#' autoplot(hov, ylab = "Time", xlab = "Latitude")
#' @export
autoplot.hovmoller <- function(object,
                               ylab = "Day",
                               xlab = NULL,
                               title = "",
                               palette = "Spectral",
                               legend_title="z",
                               ...){

  lat_or_lon <- t <- z <- NULL
  df2_Hov <- object$hov_df
  df2_Hov$z <- as.numeric(df2_Hov$z)
  df2_Hov <- as.data.frame(df2_Hov)

  if(is.null(xlab)){
    xlab <- object$xlab
  }

  # Produce a tiled plot where each tile corresponding to
  # latitude and time is filled with z
  ggplot(df2_Hov) +
    geom_tile(aes(x = lat_or_lon, y = t, fill = z)) +             # produce tiles with fill colour, z
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    ylab(ylab) +
    xlab(xlab) +                                           # label the x-axis
    guides(fill=guide_legend(title=legend_title)) +        # specify legend title
    theme_bw()                                             # black and white theme

}

