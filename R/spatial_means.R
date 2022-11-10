#' Computes spatial empirical means using a dataframe as input
#'
#' This function computes spatial empirical means by latitude and longitude averaged over time.
#'
#' @inheritParams spatial_snapshots
#' @inheritParams spatial_snapshots.data.frame
#' @inheritParams spatial_means
#'
#' @examples
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
#' @export
spatial_means.data.frame <- function(x,
                   lat_col,
                   lon_col,
                   t_col,
                   z_col,
                   ...){
  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
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

  df <- x
  y <- df[ ,lat_col]
  x <- df[ ,lon_col]
  z <- df[ ,z_col]
  t <- df[ ,t_col]
  value <- mu_emp <- NULL

  df2 <- data.frame(y = y, x = x, value = z, t = t)

  spat_av <- dplyr::group_by(df2, x, y) %>%           # group by lon-lat
    dplyr::summarise(mu_emp = mean(value))                     # mean in each lon-lat box

  if(dim(df2)[1] == dim(spat_av)[1]){
    message("Only one observation of each location is present. The mean is the original value itself!")
  }
  ## Spatial Empirical Means

  structure(list(
    spatial_avg = spat_av,
    data = df2,
    call = match.call()
  ), class='spatialmeans')

}


#' Computes spatial empirical means using a stars object as input
#'
#' This function computes spatial empirical means by latitude and longitude averaged over time.
#'
#' @inheritParams spatial_means
#'
#' @examples
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
#'
#' @export
spatial_means.stars <- function(x,
                                ...){
  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }

  if(length(dim(x)) > 3){
    stop("The stars object can have x, y and time values (3 dimensions) or x & y values (2 dimensions)
         It doesn't make sense to average over more dimensions.")
  }

  df <- dplyr::as_tibble(x)

  x <- y <- value <- t <- NULL
  if(dim(df)[2] == 3){
    # X and Y only
    colnames(df) <- c("x", "y", "value")
  }else{
    # X, Y and Time y
    colnames(df) <- c("x", "y", "t", "value")
  }


  mu_emp <- NULL

 spat_av <- dplyr::group_by(df, x, y) %>%           # group by lon-lat
    dplyr::summarise(mu_emp = mean(value))                     # mean in each lon-lat box

 if(dim(df)[1] == dim(spat_av)[1]){
   message("Only one observation of each location is present. The mean is the original value itself!")
 }
  ## Spatial Empirical Means

  structure(list(
    spatial_avg = spat_av,
    data = df,
    call = match.call()
  ), class='spatialmeans')

}
