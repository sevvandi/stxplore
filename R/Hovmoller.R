#' Computes the data structure for the Hovmoller plots using a dataframe as input
#'
#' This function creates the data structure for Hovmoller plots for either latitude or longitude.
#'
#' @inheritParams hovmoller
#' @inheritParams spatial_snapshots.data.frame
#' @param lat_or_lon Needs to be either \code{lat} or \code{lon}. \code{lat} plots the latitudinal
#' Hovmoller plat, while \code{lon} plots the longitudinal Hovmoller plot.
#' @param lat_or_lon_col The column or the column name corresponding to the latitude/longitude.
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' hovmoller(lat_or_lon = "lat",
#'           x = Tmax,
#'           lat_or_lon_col = 'lat',
#'           t_col = 't',
#'           z_col = 'z')
#'
#' @export
hovmoller.data.frame <- function(x,
                                 lat_or_lon ="lat",
                                 xlen = NULL,
                                 lat_or_lon_col,
                                 t_col,
                                 z_col,
                                 ...){
  if(lat_or_lon =="lat"){
    xlab = "Latitude"
  }else if(lat_or_lon == "lon"){
    xlab = "Longitude"
  }else{
    stop("The argument lat_or_lon can only be either lat or lon.")
  }

  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
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

  df <- x
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

  structure(list(
    hov_df = df2_Hov,
    data = df2,
    xlab = xlab,
    call = match.call()
  ), class='hovmoller')


}




#' Computes the data structure for the Hovmoller plots using a stars object as input
#'
#' This function creates the data structure for Hovmoller plots for either latitude or longitude.
#'
#' @inheritParams hovmoller
#'
#' @examples
#' library(stars)
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' hov <- hovmoller(prec)
#' hov
#'
#' @export
hovmoller.stars <- function(x,
                            lat_or_lon ="lat",
                            xlen = NULL,
                            ...){
  lat_or_lon_col <- NULL

  if(lat_or_lon =="lat"){
    xlab = "Latitude"
    lat_or_lon_col <- 2
  }else if(lat_or_lon == "lon"){
    xlab = "Longitude"
    lat_or_lon_col <- 1
  }else{
    stop("The argument lat_or_lon can only be either lat or lon.")
  }

  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }

  if(length(dim(x)) !=3){
    stop("You need a 3-dimensional stars object to produce Hovmoller plots!")
  }

  df <- dplyr::as_tibble(x)
  df <- as.data.frame(df)

  lat_or_lon <- df[ ,lat_or_lon_col]
  z <- df[ ,4]
  t <- df[ ,3]

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

  structure(list(
    hov_df = df2_Hov,
    data = df2,
    xlab = xlab,
    call = match.call()
  ), class='hovmoller')


}

