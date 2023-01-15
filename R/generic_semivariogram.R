#' Generic function to compute the semi-variogram
#'
#' Computes the semi-variogram given the locations, times and the quantity of interest.
#' This function can take either a stars object or a dataframe. Input arguments differ for each case.
#'
#' @param x The dataframe or stars object. If it is a dataframe, then it should have the locations.
#' @param latitude_linear If \code{TRUE}  a linear model is fitted with latitude as a covariate is fitted.
#' @param longitude_linear If \code{TRUE}  a linear model is fitted with longitude as a covariate is fitted.
#' @param missing_value If a certain value such as -9999 denotes the missing values for given locations and times.
#' @param width A parameter to the \code{gstat::variogram} function. The width of the distance intervals to be considered.
#' @param cutoff A parameter to the \code{gstat::variogram} function. The spatial separation distance.
#' @param tlagmax A parameter to the \code{gstat::variogram} function. The maximum time lag.
#' @param ... Other arguments that need to be used for datafames  or currently ignored.
#'
#' @examples
#' # Dataframe example
#' library(dplyr)
#' data(locs)
#' data(Times)
#' data(Tmax)
#' temp_part <- with(Times, paste(year, month, day, sep = "-"))
#' temp_part <- data.frame(date = as.Date(temp_part)[913:943])
#' Tmax <- Tmax[913:943, ]
#' semivariogram(locs,
#'         temp_part,
#'         Tmax,
#'         latitude_linear = FALSE,
#'         longitude_linear = FALSE,
#'         missing_value = -9999,
#'         width = 50,
#'         cutoff = 1000,
#'         tlagmax = 7
#' )
#'
#' # Stars example
#' library(stars)
#' # Create a stars object from a data frame
#' precip_df <- NOAA_df_1990[NOAA_df_1990$proc == 'Precip', ] %>%
#'   filter(date >= "1992-02-01" & date <= "1992-02-28")
#' precip <- precip_df[ ,c('lat', 'lon', 'date', 'z')]
#' st_precip <- st_as_stars(precip, dims = c("lon", "lat", "date"))
#' semivariogram(st_precip)
#'
#' @export semivariogram
semivariogram <- function(x,
                          latitude_linear = TRUE,
                          longitude_linear = TRUE,
                          missing_value = -9999,
                          width = 80,
                          cutoff = 1000,
                          tlagmax = 6,
                          ...){
  UseMethod("semivariogram")
}
