#' Generic function to perform Canonical Correlation Analysis  using Empirical Orthogonal Functions from a lagged dataset
#'
#' Performs CCA using EOF analysis using a lagged dataset.
#'
#' @inheritParams emp_orth_fun
#' @param lag Specifies the lag to be used.
#' @param n_eof The number of EOFs to be used.
#'
#' @examples
#' # Dataframe example
#' cancor_eof(x = SSTlonlatshort,
#'            lag = 7,
#'            n_eof = 8,
#'            values_df = SSTdatashort)
#'
#' # Stars example
#' library(dplyr)
#' library(stars)
#' # Create a stars object from a data frame
#' precip_df <- NOAA_df_1990[NOAA_df_1990$proc == 'Precip', ] %>% filter(date >= "1992-02-01" & date <= "1992-02-28")
#' precip <- precip_df[ ,c('lat', 'lon', 'date', 'z')]
#' st_precip <- st_as_stars(precip, dims = c("lon", "lat", "date"))
#' cancor_eof(st_precip)
#' @export cancor_eof
cancor_eof <- function(x,
                       lag,
                       n_eof,
                       ...){
  UseMethod("cancor_eof")
}
