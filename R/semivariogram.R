#' Computes the semi-variogram using a dataframe as input
#'
#' Computes the semi-variogram given the locations, times and the quantity of interest.
#'
#' @inheritParams semivariogram
#' @param times_df The dataframe containing the dates in \code{Date} format.
#' @param values_df The dataframe of dimension \code{length(times) x length(locations)} containing the quantity of interest.
#'
#' @examples
#' library(dplyr)
#' data(locs)
#' data(Times)
#' data(Tmax)
#' temp_part <- with(Times, paste(year, month, day, sep = "-"))
#' temp_part <- data.frame(date = as.Date(temp_part)[913:943])
#' Tmax <- Tmax[913:943, ]
#' semivariogram(locs,
#'               latitude_linear = FALSE,
#'               longitude_linear = FALSE,
#'               missing_value = -9999,
#'               width = 50,
#'               cutoff = 1000,
#'               tlagmax = 7,
#'               times_df = temp_part,
#'               values_df = Tmax
#' )
#'
#' @export
semivariogram.data.frame <- function(x,
                                     latitude_linear = TRUE,
                                     longitude_linear = TRUE,
                                     missing_value = -9999,
                                     width = 80,
                                     cutoff = 1000,
                                     tlagmax = 6,
                                     times_df,
                                     values_df,
                                     ...){
  if(missing(x)){
    stop("Empty dataframe x. Please give a dataframe with latitude and longitude.")
  }

  locations_df <- x

  if(missing(times_df)){
    stop("Empty dataframe times_df. Please give a dataframe with times.")
  }

  if(missing(values_df)){
    stop("Empty dataframe values_df. Please give an N x T dataframe with values of the quantity you are interested in.  ")
  }

  if(!setequal(colnames(locations_df), c("id", "lon", "lat"))){
    stop("The column names of locations_df needs to be id, lon and lat. Please update locations_df.")
  }

  if(dim(times_df)[2] !=1 ){
    stop("The dataframe times_df needs to have only 1 column of dates.")
  }

  colnames(times_df) <- "date"
  if(!lubridate::is.Date(times_df$date)){
    stop("The dataframe times_df doesn't contain any dates.")
  }

  if(!identical(dim(values_df), c(dim(times_df)[1], dim(locations_df)[1]) )){
    stop("The dataframe values_df needs to have T x N dimensions, T = number of times N = number of locations.")
  }

  names(values_df)  <- locations_df$id
  values_df <-  cbind(times_df,  values_df)
  id <- NULL

  # Define spatial and temporal components
  spat_part <- sp::SpatialPoints(coords = locations_df[, c("lon", "lat")])
  temp_part <- as.Date(times_df$date)

  values_long <- tidyr::pivot_longer(values_df, cols = 2:dim(values_df)[2])
  colnames(values_long) <- c("date", "id", "z")
  values_long$id <- as.integer(values_long$id)
  values_long <- dplyr::arrange(values_long, date, id)

  STObj3 <- spacetime::STFDF(sp = spat_part,
                            time = temp_part,
                            data = values_long)

  # Assign a projection
  sp::proj4string(STObj3) <- sp::CRS("+proj=longlat +ellps=WGS84")

  # Label missing values
  STObj3$z[STObj3$z == missing_value] <- NA

  if(latitude_linear & longitude_linear){
    formula <- z ~ 1 + lat + lon
  }else if(latitude_linear){
    formula <- z ~ 1 + lat
  }else if(longitude_linear){
    formula <- z ~ 1 + lon
  }else{
    formula <- z ~ 1
  }

  vv <- gstat::variogram(object = formula,            # fixed effect component
                  data = STObj3,                      #
                  width = width,                      #
                  cutoff = cutoff,                    # only consider pts < cutoff km apart
                  tlags = 0.01:tlagmax)               # 0-tlagmaxdays days (will create an NA if start at 0)

 structure(list(
    variogram = vv,
    loc_data = locations_df,
    times_data = times_df,
    values_data = values_df,
    call = match.call()
  ), class='semivariogramobj')
}


#' Computes the semi-variogram using a stars object
#'
#' Computes the semi-variogram using a stars object. A 3 dimensional stars object is used with
#' the first 2 for locations and the third for time.
#'
#' @inheritParams semivariogram
#'
#' @examples
#' library(dplyr)
#' data(locs)
#' data(Times)
#' data(Tmax)
#' temp_part <- with(Times, paste(year, month, day, sep = "-"))
#' temp_part <- data.frame(date = as.Date(temp_part)[913:943])
#' Tmax <- Tmax[913:943, ]
#' semivariogram(locs,
#'               latitude_linear = FALSE,
#'               longitude_linear = FALSE,
#'               missing_value = -9999,
#'               width = 50,
#'               cutoff = 1000,
#'               tlagmax = 7,
#'               times_df = temp_part,
#'               values_df = Tmax
#' )
#'
#' @export
semivariogram.stars <- function(x,
                                latitude_linear = TRUE,
                                longitude_linear = TRUE,
                                missing_value = -9999,
                                width = 80,
                                cutoff = 1000,
                                tlagmax = 6,
                                ...){
   if(missing(x)){
     stop("Empty stars object x. Please give a stars object with 3 dimensions.")
   }

   return(x)
}

#
#   if(!setequal(colnames(locations_df), c("id", "lon", "lat"))){
#     stop("The column names of locations_df needs to be id, lon and lat. Please update locations_df.")
#   }
#
#   if(dim(times_df)[2] !=1 ){
#     stop("The dataframe times_df needs to have only 1 column of dates.")
#   }
#
#   colnames(times_df) <- "date"
#   if(!lubridate::is.Date(times_df$date)){
#     stop("The dataframe times_df doesn't contain any dates.")
#   }
#
#   if(!identical(dim(values_df), c(dim(times_df)[1], dim(locations_df)[1]) )){
#     stop("The dataframe values_df needs to have T x N dimensions, T = number of times N = number of locations.")
#   }
#
#   names(values_df)  <- locations_df$id
#   values_df <-  cbind(times_df,  values_df)
#   id <- NULL
#
#   # Define spatial and temporal components
#   spat_part <- sp::SpatialPoints(coords = locations_df[, c("lon", "lat")])
#   temp_part <- as.Date(times_df$date)
#
#   values_long <- tidyr::pivot_longer(values_df, cols = 2:dim(values_df)[2])
#   colnames(values_long) <- c("date", "id", "z")
#   values_long$id <- as.integer(values_long$id)
#   values_long <- dplyr::arrange(values_long, date, id)
#
#   STObj3 <- spacetime::STFDF(sp = spat_part,
#                              time = temp_part,
#                              data = values_long)
#
#   # Assign a projection
#   sp::proj4string(STObj3) <- sp::CRS("+proj=longlat +ellps=WGS84")
#
#   # Label missing values
#   STObj3$z[STObj3$z == missing_value] <- NA
#
#   if(latitude_linear & longitude_linear){
#     formula <- z ~ 1 + lat + lon
#   }else if(latitude_linear){
#     formula <- z ~ 1 + lat
#   }else if(longitude_linear){
#     formula <- z ~ 1 + lon
#   }else{
#     formula <- z ~ 1
#   }
#
#   vv <- gstat::variogram(object = formula,            # fixed effect component
#                          data = STObj3,                      #
#                          width = width,                      #
#                          cutoff = cutoff,                    # only consider pts < cutoff km apart
#                          tlags = 0.01:tlagmax)               # 0-tlagmaxdays days (will create an NA if start at 0)
#
#   structure(list(
#     variogram = vv,
#     loc_data = locations_df,
#     times_data = times_df,
#     values_data = values_df,
#     call = match.call()
#   ), class='semivariogramobj')
# }

#'
