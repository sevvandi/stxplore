#' Hovmoller plots for latitude or longitude
#'
#' This function creates a Hovmoller plot for either latitude or longitude.
#'
#' @inheritParams st_ssnap
#' @param lat_or_lon Needs to be either \code{lat} or \code{lon}. \code{lat} plots the latitudinal
#' Hovmoller plat, while \code{lon} plots the longitudinal Hovmoller plot.
#' @param lat_or_lon_col The column or the column name corresponding to the latitude/longitude.
#' @param xlen The length of the xaxis for latitude/longitude.
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' st_hov(lat_or_lon = "lat",
#'    df = Tmax,
#'    lat_or_lon_col = 'lat',
#'    t_col = 't',
#'    z_col = 'z',
#'    legend_title = 'Temperature')
#'
#' @export st_hov
#' @importFrom rlang .data
st_hov <- function(lat_or_lon ="lat",
                   df,
                   lat_or_lon_col,
                   t_col,
                   z_col,
                   ylab = "Day",
                   title = "",
                   palette = "Spectral",
                   legend_title="z",
                   xlen = NULL){
  if(lat_or_lon =="lat"){
    xlab = "Latitude"
  }else if(lat_or_lon == "lon"){
    xlab = "Longitude"
  }else{
    stop("The argument lat_or_lon can only be either lat or lon.")
  }

  if(missing(df)){
    stop("Empty dataframe df. Please give a proper input.")
  }

  if(missing(lat_or_lon_col)){
    stop("Latitude or longitude column not specified. Use lat_or_lon_col to specify latitude/longitude.")
  }

  if(missing(t_col)){
    stop("Time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  lat_or_lon <- df[ ,lat_or_lon_col]
  z <- df[ ,z_col]
  t <- df[ ,t_col]

  df2 <- data.frame(lat_or_lon = lat_or_lon, z = z, t = t)

  if(is.null(xlen)){
    xlen <- min(25, length(unique(lat_or_lon)))
  }


  lim_latlon <- range(df2$lat_or_lon)                              # latitude/longitude range
  lim_t <- range(df2$t)                                            # time range
  latlon_axis <- seq(lim_latlon[1], lim_latlon[2], length = xlen)

  df2_grid <- df2
  dists <- abs(outer(df2_grid$lat_or_lon, latlon_axis, "-"))
  latlon1 <- latlon_axis[apply(dists, 1, which.min)]

  df2_grid <- data.frame(lat_or_lon = latlon1, z = df2$z, t = df2$t )
  df2_Hov <- df2_grid %>%
    dplyr::group_by(lat_or_lon, t) %>%
    dplyr::summarise(z = mean(z))

  if(length(unique(df2_Hov$lat_or_lon)) < xlen){
    print("Consider reducing xlen to eliminate empty patches.")
  }

  # Produce a tiled plot where each tile corresponding to
  # latitude and time is filled with z
  ggplot(df2_Hov) +
    geom_tile(aes(x = lat_or_lon, y = t, fill = z)) +             # produce tiles with fill colour, z
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    scale_y_reverse() +                                    # reverse the scale so red is high z
    ylab(ylab) +
    xlab(xlab) +                                           # label the x-axis
    guides(fill=guide_legend(title=legend_title)) +        # specify legend title
    theme_bw()                                             # black and white theme

}
