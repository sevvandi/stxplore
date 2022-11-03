#' Plots empirical orthogonal functions
#'
#' Plots empirical orthogonal functions of the data.
#'
#' @inheritParams semivariogram
#' @inheritParams spatial_snapshots
#' @param EOF_num The Empirical Orthogonal Function (EOF) to plot.
#' @param only_EOF If \code{TRUE}, only the spatial EOF function would be plotted.
#' @param only_TS If \code{TRUE}, only the PC time series would be plotted. If both are set to \code{FALSE},
#'                 both plots would be displayed. Both cannot be set to \code{TRUE}.
#'
#' @examples
#' data(SSTlonlatshort)
#' data(SSTdatashort)
#' data(SSTlandmaskshort)
#' # Take first 396 months (33 years) and delete land
#' delete_rows <- which(SSTlandmaskshort  ==  1)
#' SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
#' st_eof(SSTlonlatshort[-delete_rows,  ],
#'        SSTdatashort,
#'        EOF_num = 3)
#'
#' g1 <- st_eof(SSTlonlatshort[-delete_rows, ],
#'              SSTdatashort,
#'              EOF_num = 1,
#'              only_EOF = TRUE)
#' g2 <- st_eof(SSTlonlatshort[-delete_rows, ],
#'              SSTdatashort,
#'              EOF_num = 2,
#'              only_EOF = TRUE)
#' g3 <- st_eof(SSTlonlatshort[-delete_rows, ],
#'              SSTdatashort,
#'              EOF_num = 3,
#'              only_EOF = TRUE)
#' gridExtra::grid.arrange(g1, g2, g3)
#' @export st_eof
st_eof <- function(locations_df,
                   values_df,
                   EOF_num = 1,
                   palette = "Spectral",
                   only_EOF = FALSE,
                   only_TS = FALSE){

  if(missing(locations_df)){
    stop("Empty dataframe locations_df. Please give a dataframe with latitude and longitude.")
  }

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

  variable <- paste("EOF",EOF_num, sep="")
  lon <- 'lon'
  lat <- 'lat'
  # Plot EOF
  eof_p <- ggplot(EOFs)  +  geom_tile(ggplot2::aes_string(x = lon, y = lat, fill = variable))  +
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    theme_bw()  +
    xlab(expression(paste("Longitude", degree)))  +
    ylab(expression(paste("Latitude", degree))) +
    ggtitle(paste("EOF_",EOF_num, sep=""))

  # Plot PC of that EOF
  varriable2 <- paste("X", EOF_num, sep="")
  TS_EOF <- subset(TS, EOF == varriable2)
  pcts_eof <- ggplot(TS_EOF, aes(x=t, y=nPC)) + geom_line(size = 0.8) +
    xlab("Time (t)") +
    ylab("Normalized Principal Component") +
    ggtitle(paste('PC Time series for EOF_', EOF_num, sep=""))

  if(only_EOF){
    eof_p
  }else if(only_TS){
    pcts_eof
  }else{
    gridExtra::grid.arrange(eof_p, pcts_eof)
  }

}
