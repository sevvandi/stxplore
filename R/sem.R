#' Plots spatial empirical means
#'
#' This function plots two graphs, spatial empirical means by latitude and longitude.
#'
#' @inheritParams st_ssnap
#'
#' @examples
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' st_sem(Tmax,
#'        lat_col = "lat",
#'        lon_col = "lon",
#'        t_col = "t",
#'        z_col = "z",
#'        ylab="Mean Max Temp")
#'
#' @export st_sem
st_sem <- function(df,
                   lat_col,
                   lon_col,
                   t_col,
                   z_col,
                   ylab = "Mean Value"
                   ){
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
  mu_emp <- NULL

  df2 <- data.frame(lat = lat, lon = lon, z = z, t = t)

  spat_av <- dplyr::group_by(df2, lat, lon) %>%           # group by lon-lat
    dplyr::summarise(mu_emp = mean(z))                     # mean in each lon-lat box

  lat_means <- ggplot(spat_av) +
    geom_point(aes(lat, mu_emp)) +
    theme_bw() +
    xlab("Latitude") +
    ylab(ylab)
  lon_means <- ggplot(spat_av) +
    geom_point(aes(lon, mu_emp)) +
    theme_bw() +
    xlab("Longitude") +
    ylab("Mean Value")

  ## Spatial Empirical Means

  ll_means <- gridExtra::grid.arrange(lat_means, lon_means, nrow = 1, ncol = 2, top = "Spatial Empirical Means")
  ll_means
}
