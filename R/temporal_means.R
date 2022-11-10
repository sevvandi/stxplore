#' Computes temporal empirical means using a dataframe as input
#'
#' This function computes temporal empirical means averaged per time unit.
#'
#' @inheritParams temporal_snapshots.data.frame
#' @inheritParams spatial_snapshots.data.frame
#'
#' @examples
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' temporal_means(Tmax,
#'        t_col = 'date',
#'        z_col = 'z',
#'        id_col = 'id')
#' @export
temporal_means.data.frame <- function(x,
                                      t_col,
                                      z_col,
                                      id_col,
                                      ...){
  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
  }

  if(missing(t_col)){
    stop("Date/time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  if(missing(id_col)){
    stop("Location id column is not specified. Use id_col to specify location id.")
  }

  df <- x
  z <- df[ ,z_col]
  t <- df[ ,t_col]
  id <- df[ ,id_col]
  meanz <- NULL
  time <- NULL

  df2 <- data.frame(z = z, time = t, id = id)

  df_av <- dplyr::group_by(df2, time) %>%
    dplyr::summarise(meanz = mean(z))

  structure(list(
    data = df2,
    averages = df_av,
    call = match.call()
  ), class='temporalmeans')
}


#' Computes temporal empirical means using a stars object as input
#'
#' This function computes temporal empirical means averaged per time unit.
#'
#' @inheritParams temporal_snapshots.data.frame
#' @inheritParams spatial_snapshots.data.frame
#'
#' @examples
#' library(stars)
#' library(dplyr)
#' library(units)
#' # Example
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' temporal_means(prec)
#' @export
temporal_means.stars <- function(x,
                                 ...){

  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }

  if(length(dim(x)) !=3){
    stop("To compute temporal means, you need a 3D raster! Not more or less dimensions. ")
  }

  df <- dplyr::as_tibble(x)

  id <- x <- y <- time <- value <- NULL

  descriptive_name <- colnames(df)[4]
  colnames(df) <- c("x", "y", "time", "value")
  df2 <- df %>%
    dplyr::mutate(id = paste(x, y, sep ="-")) %>%
    dplyr::select(value, time, id)

  df_av <- dplyr::group_by(df, time) %>%
    dplyr::summarise(meanz = mean(value))

  structure(list(
    data = df2,
    averages = df_av,
    call = match.call()
  ), class='temporalmeans')
}

