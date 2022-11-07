#' Performs Canonical Correlation Analysis  using Empirical Orthogonal Functions from a lagged dataset
#'
#' Performs CCA using EOF analysis using a lagged dataset.
#'
#' @inheritParams emp_orth_fun
#' @param lag Specifies the lag to be used.
#' @param n_eof The number of EOFs to be used.
#'
#' @examples
#' cancor_eof(SSTlonlatshort,
#'            SSTdatashort,
#'            lag = 7,
#'            n_eof = 8)
#'
#' @export cancor_eof
cancor_eof <- function(locations_df,
                          values_df,
                          lag = 7,
                          n_eof = 10){

  # option 4 - using the same dataset - using eof lagged
  if(missing(locations_df)){
    stop("Empty dataframe locations_df. Please give a dataframe with latitude and longitude.")
  }

  if(missing(values_df)){
    stop("Empty dataframe values_df. Please give an N x T dataframe with values of the quantity you are interested in.  ")
  }
  print("Canonical Correlation Analysis is carried out using a lagged dataset.")

  SW1 <- Variable <- lat <- lon <- ts <- EOF <- nPC <- NULL

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

  nEOF <-  n_eof
  EOFset1  <- E$u[1:(nT-lag),  1:nEOF]  * sqrt(nT - 1)
  EOFset2  <- E$u[(lag+1):nT,  1:nEOF]  * sqrt(nT - 1)

  cc  <-  cancor(EOFset1, EOFset2)

  # Compute the  series of the first canonical variables
  CCA_df  <- data.frame(t  =  rep(1:(nT-lag), 2),
                        ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
                                 (EOFset2 %*%  cc$ycoef[,1])[,1]),
                        Variable = c(rep("CCA1", nT-lag), rep("CCA2", nT-lag)))

  # Visualizing the Linear Weights
  EOFs_CCA <- data.frame(lon = EOFs$lon, lat = EOFs$lat)
  EOFs_CCA$SW1 <- as.numeric(as.matrix(EOFs[,3:(2+n_eof)]) %*% cc$xcoef[,1])
  EOFs_CCA$SW2 <- as.numeric(as.matrix(EOFs[,3:(2+n_eof)]) %*% cc$ycoef[,1])



  # Compute the  series of the first canonical variables
  CCA_df  <- data.frame(t  =  rep(1:(nT-lag), 2),
                        ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
                                 (EOFset2 %*%  cc$ycoef[,1])[,1]),
                        Variable = c(rep("CCA1", nT-lag), rep("CCA2", nT-lag)))



  structure(list(
    cancor_df = CCA_df,
    eofs_df = EOFs_CCA,
    locations_df = locations_df,
    values_df = values_df,
    call = match.call()
  ), class='cancoreof')


}
