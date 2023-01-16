#' Computes empirical spatial covariance using a dataframe as input
#'
#' Computes empirical spatial covariance by removing trends and examining residuals. It can compute lag-0 or log-1
#' empirical covariance either by latitude or longitude. You can split up the spatial domain by latitude or
#' longitude and plot the covariance for each longitudinal/latitudinal strips.
#'
#' @inheritParams spatial_snapshots
#' @inheritParams spatial_snapshots.data.frame
#' @inheritParams emp_spatial_cov
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month %in% 5:9 &
#'   year == 1993)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' emp_spatial_cov(Tmax,
#'                 lat_or_lon_strips = "lon",
#'                 num_strips = 4,
#'                 lag = 1,
#'                 lat_col = "lat",
#'                 lon_col = "lon",
#'                 t_col ="t",
#'                 z_col = "z")
#'
#' @importFrom graphics par
#' @importFrom stats cov lm
#' @export
emp_spatial_cov.data.frame <- function(x,
                                       lat_or_lon_strips = "lon",
                                       quadratic_time = FALSE,
                                       quadratic_space = FALSE,
                                       num_strips = 1,
                                       lag = 0,
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

  if(!num_strips %in% c(1,2,3,4)){
    stop("Parameter num_strips can only take integer values from 1 to 4. ")
  }

  if(!lag %in% c(0,1,2)){
    stop("Parameter lag can only be 0, 1, or 2. ")
  }

  df <- x
  lat <- df[ ,lat_col]
  lon <- df[ ,lon_col]
  z <- df[ ,z_col]
  t <- df[ ,t_col]
  residuals <- group <- space <- NULL

  if(lat_or_lon_strips == "lon"){
    xlab <- "Latitude"
    space <- lat
    group <- lon
  }else if(lat_or_lon_strips == "lat"){
    xlab <- "Longitude"
    space <- lon
    group <- lat
  }else{
    stop("Parameter lat_or_lon_strips can only take values lat or lon.")
  }

  df2 <- data.frame(space = space, group = group, z = z, t = t)

  if( quadratic_time & quadratic_space){
    mod <- lm(z ~ space + t + I(t^2) + I(space^2), data = df2)
  }else if(quadratic_time){
    mod <- lm(z ~ space + t + I(t^2), data = df2)
  }else{
    mod <- lm(z ~ space + t, data = df2)
  }

  df2$residuals <- residuals(mod)

  df_sw <- df2 %>% dplyr::select(space, group, residuals, t) %>%    # select columns
    tidyr::pivot_wider(names_from = t, values_from = residuals) %>%  # make time-wide
    dplyr::arrange(group) %>%
    dplyr::select(-space, -group) %>%                               # drop co-ordinate info
    t()



  if(lag == 0){
    # Empirical log-0  Covariance Matrices
    Lag_cov <- cov(df_sw, use = 'pairwise.complete.obs')
  }else if(lag == 1){
    # Empirical lag-1 Covariance Matrices
    Lag_cov <- cov(df_sw[-1, ], df_sw[-nrow(df_sw), ],
                    use = 'pairwise.complete.obs')
  }else if(lag == 2){
    Lag_cov <- cov(df_sw[-c(1:2),] , df_sw[-c(nrow(df_sw)-1):nrow(df_sw), ],
                    use = 'pairwise.complete.obs')
  }


  # Extract spatial locations
  spat_df <- dplyr::filter(df2, t == min(t)) %>%         # lon/lat co-ordinates of stations
    dplyr::select(group, space) %>%                      # select lon/lat only
    dplyr::arrange(group, space)                         # sort ascending by lon/lat

  spat_df$n <- 1:nrow(spat_df)                       # assign an index to each station
  lim_group <- range(spat_df$group)
  group_strips <- seq(lim_group[1], lim_group[2],          # create 4 longitude/latitude strip boundaries
                    length = (num_strips + 1))
  spat_df$group_strip <- cut(spat_df$group, group_strips,  # bin lon/lat into respective bins
                           labels = FALSE,           # don't assign num labels
                           include.lowest = TRUE)    # include edges


  structure(list(
    spatial_df = spat_df,
    lag_cov = Lag_cov,
    data = df2,
    num_strips = num_strips,
    call = match.call()
  ), class='spatialcov')

}


#' Computes empirical spatial covariance using a stars object
#'
#' Computes empirical spatial covariance by removing trends and examining residuals. It can compute lag-0 or log-1
#' empirical covariance either by latitude or longitude. You can split up the spatial domain by latitude or
#' longitude and plot the covariance for each longitudinal/latitudinal strips.
#'
#' @inheritParams spatial_snapshots
#' @inheritParams spatial_snapshots.data.frame
#' @inheritParams emp_spatial_cov
#'
#' @examples
#' \dontrun{
#' library(stars)
#' library(dplyr)
#' library(units)
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' prec2 <- prec %>%
#'          slice(time,  1:3)
#' emp_spatial_cov(prec2,
#'                 lat_or_lon_strips = "lon",
#'                 num_strips = 4,
#'                 lag = 1)
#' }
#' @importFrom graphics par
#' @importFrom stats cov lm
#' @export
emp_spatial_cov.stars <- function(x,
                                  lat_or_lon_strips = "lon",
                                  quadratic_time = FALSE,
                                  quadratic_space = FALSE,
                                  num_strips = 1,
                                  lag = 0,
                                  ...){
  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }


  df <- dplyr::as_tibble(x)
  colnames(df)[dim(df)[2]] <- 'z'
  df <- as.data.frame(df)
  inds <- which(!is.na(df$z))
  df <- df[inds, ]
  emp_spatial_cov.data.frame(x = df,
                             lat_or_lon_strips = lat_or_lon_strips,
                             quadratic_time = quadratic_time,
                             quadratic_space = quadratic_space,
                             num_strips = num_strips,
                             lag = lag,
                             lat_col = 2,
                             lon_col = 1,
                             t_col = 3,
                             z_col = 4,
                             ...)
}
