#' Plots Canonical Correlation Analysis plots using Empirical Orthogonal Functions from a lagged dataset
#'
#' Plots CCA plots using EOF analysis using a lagged dataset.
#'
#' @inheritParams st_eof
#' @param lag Specifies the lag to be used.
#' @param n_eof The number of EOFs to be used.
#' @param xlab The x label for the line plot.
#' @param line_plot If set to \code{TRUE}, then the line plot is included.
#' @param space_plot If set to \code{TRUE}, the space splot is included.
#'
#' @examples
#' st_cancor_eof(SSTlonlatshort,
#'               SSTdatashort,
#'               n_eof = 8)
#'
#' @export st_cancor_eof
st_cancor_eof <- function(locations_df,
                          values_df,
                          lag = 7,
                          n_eof = 10,
                          xlab = "Year",
                          line_plot = TRUE,
                          space_plot = TRUE,
                          palette = 'Spectral'){

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



  # Now plot
  sst_cca <- ggplot(CCA_df, aes(x=t, y=ts, colour = Variable)) + geom_line(size = 0.8) +
    scale_color_manual(values = c("dark blue", "dark red")) +
    ylab("CCA  variables")  +
    xlab(xlab)  +
    theme_bw() +
    ggtitle("Canonical Correlation Analysis")


  # Plotting Weights as Spatial Maps: First Canoncial Variable

  spwts_p <- ggplot(EOFs_CCA, aes(lon, lat)) +
    geom_tile(aes(fill = SW1)) +
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    scale_y_reverse() + ylab("Latitude") +
    xlab("Longitude") +
    ggtitle("Spatial Weights: Canonical Variable")  +
    theme_bw()

  if(line_plot & space_plot){
    gridExtra::grid.arrange(sst_cca, spwts_p)
  }else if(line_plot){
    sst_cca
  }else if(space_plot){
    spwts_p
  }

}
