#' Plots spatial snapshots of data through time
#'
#' This function plots a set of spatial snapshots through time. General usage involves latitude and
#' longitude. However, x and y coordinates can be given instead of longitude and latitude. If x and
#' y are given instead of longitude and latitude, the country borders will not be shown.
#'
#' @param df The input data in a dataframe.
#' @param lat_col The column or the column name giving the latitude. The y coordinate can be used instead of latitude.
#' @param lon_col The column or the column name giving the longitude. The x coordinate can be used instead of longitude.
#' @param t_col The time column. Time must be a set of discrete integer values.
#' @param z_col The The quantity of interest that will be plotted. Eg. temperature.
#' @param xlab The x label.
#' @param ylab The y label.
#' @param ifxy If \code{TRUE} then the country borders are not drawn as longitude and latitude are unknown.
#' @param title The graph title.
#' @param palette The color palette. Default is \code{Spectral}.
#' @param legend_title The title for the legend.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' Tmax_days <- subset(Tmax, t %in% c(1, 15, 30))
#' st_ssnap(Tmax_days,
#'   lat_col = 'lat',
#'   lon_col = 'lon',
#'   t_col = 't',
#'   z_col = 'z',
#'   title = "Maximum Temperature for 3 days ")
#' @export
#' @importFrom ggplot2 aes coord_fixed facet_wrap geom_path geom_point ggplot ggtitle geom_tile scale_y_reverse
#' @importFrom ggplot2 scale_fill_distiller guides guide_legend labs scale_color_manual
#' @importFrom ggplot2 map_data scale_colour_distiller theme_bw xlab ylab geom_line theme unit
#' @importFrom rlang .data
st_ssnap <- function(df,
                     lat_col,
                     lon_col,
                     t_col,
                     z_col,
                     xlab = "Longitude",
                     ylab = "Latitude",
                     ifxy = FALSE,
                     title = "",
                     palette = "Spectral",
                     legend_title = "z"){
  if(missing(df)){
    stop("Empty dataframe df. Please give a proper input.")
  }


  if(missing(lat_col)){
    stop("Latitude column not specified. Use lat_col to specify latitude.")
  }

  if(missing(lon_col)){
    stop("Longitude column not specified. Use lon_col to specify longitude.")
  }

  if(missing(t_col)){
    stop("Time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  lat <- df[ ,lat_col]
  lon <- df[ ,lon_col]
  z <- df[ ,z_col]
  t <- df[ ,t_col]

  df2 <- data.frame(lat = lat, lon = lon, z = z, t = t)

  bbox <- ggmap::make_bbox(lat = lat, lon = lon, data = df2)
  if(!ifxy){
    ggplot(data = df2) +                                      # plot points
      geom_point(aes(x = lon, y = lat,                       # lon and lat with colour defined by z
                     colour = z),size = 2) +                 # and size of points larger
      scale_colour_distiller(palette = palette,
                             guide = "colourbar",
                             legend_title) +
      xlab(xlab) +  ylab(ylab) +                             # label x and y axes
      geom_path(aes(x = .data$long, y = .data$lat, group = .data$group),       # add in a map of the world
                data = map_data("world")) +
      facet_wrap(~t) +                                       # separate plots based on time (facets)
      coord_fixed(xlim = c(bbox[1], bbox[3]),                # zoom in to the bounding box
                  ylim = c(bbox[2], bbox[4])) +
      theme_bw() +                                           # black and white theme
      labs(color=z_col) +
      ggtitle(title)
  }else{
    ggplot(data = df2) +                                      # plot points
      geom_point(aes(x = lon, y = lat,                       # lon and lat with colour defined by z
                     colour = z),size = 2) +                 # and size of points larger
      scale_colour_distiller(palette = palette,
                             guide = "colourbar",
                             legend_title) +
      xlab(xlab) +  ylab(ylab) +                             # label x and y axes
      facet_wrap(~t) +                                       # separate plots based on time (facets)
      coord_fixed(xlim = c(bbox[1], bbox[3]),                # zoom in to the bounding box
                  ylim = c(bbox[2], bbox[4])) +
      theme_bw() +                                           # black and white theme
      labs(color=z_col) +
      ggtitle(title)+
      guides(fill = guide_legend(title = legend_title))
  }

}
