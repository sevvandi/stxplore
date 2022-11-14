#' Computes empirical orthogonal functions
#'
#' Computes empirical orthogonal functions of the data.
#'
#' @inheritParams semivariogram.data.frame
#'
#' @examples
#' data(SSTlonlatshort)
#' data(SSTdatashort)
#' data(SSTlandmaskshort)
#' # Take first 396 months (33 years) and delete land
#' delete_rows <- which(SSTlandmaskshort  ==  1)
#' SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
#' emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
#'              SSTdatashort)
#'
#' @export emp_orth_fun
emp_orth_fun <- function(x,
                         values_df){

  if(missing(x)){
    stop("Empty dataframe x. Please give a dataframe with latitude and longitude.")
  }

  locations_df <- x

  if(missing(values_df)){
    stop("Empty dataframe values_df. Please give an N x T dataframe with values of the quantity you are interested in.  ")
  }

  EOF <- nPC <- NULL

  # Put data into space-time format
  Z  <- t(values_df)

  spat_mean <-  apply(values_df, 1, mean)
  nT  <- ncol(values_df)

  # Subtract and standardise and compute the SVD
  Zt <- 1/sqrt(nT - 1)*(Z  - outer(rep(1,nT), spat_mean))
  E <- svd(Zt)

  # Extract the matrix V which contains the EOFs in space-time format and append
  # lat lon to it.
  V <- E$v
  colnames(E$v)  <- paste0("EOF",  1:ncol(values_df))        # label each column
  EOFs <- cbind(locations_df, E$v)

  # Extract the principal component time series,
  # U in wide-table format
  TS1  <- data.frame(E$u) %>%	                               # convert U matrix to data frame
    dplyr::mutate(t  =  1:nrow(E$u))
  TS <- TS1 %>% tidyr::pivot_longer(cols = 1:(dim(TS1)[2]-1),
                             names_to ="EOF",
                             values_to ="PC")                           # put all columns (except times)
  # into long-table format with
  # EOF-PC as a key-value pair

  TS$nPC <- TS$PC * sqrt(nT-1)                             # Normalize the time series


  structure(list(
    eofs = EOFs,
    pcts = TS,
    data_loc = locations_df,
    data_vals = values_df,
    call = match.call()
  ), class='emporthfun')

}
