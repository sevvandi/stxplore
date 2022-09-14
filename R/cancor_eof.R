# st_cancor_eof <- function(df1,
#                           eof = TRUE,
#                           lag = 7,
#                           neof = 10,
#                           xlab = "Year",
#                           line_plot = TRUE,
#                           space_plot = TRUE){
#   # option 1 - using 2 separate datasets - using raw
#   # option 2 - using 2 separate datasets - using eof
#   # option 3 - using the same dataset - raw
#   # option 4 - using the same dataset - using eof lagged
#   if(missing(df1)){
#     stop("Empty dataframe locations_df. Please give a dataframe with latitude and longitude.")
#   }
#   if(is.null(df2)){
#     print("Canonical Correlation Analysis is carried out using a lagged dataset.")
#   }else{
#     if(nrow(df1)!= nrow(df2)){
#       stop("Datasets df1 and df2 need to have the same number of rows. ")
#     }
#   }
#
#   if(!eof){
#     cc <- CCA::cancor(df1, df2)
#
#     nn <- dim(df1)[1]
#     # Compute the  series of the first canonical variables
#     CCA_df  <- data.frame(t  =  rep(1:nn, 2),
#                           ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
#                                    (EOFset2 %*%  cc$ycoef[,1])[,1]),
#                           Variable = c(rep("CCA1", nn), rep("CCA2", nn)))
#   }else{
#     # perform EOF
#     EOF <- nPC <- NULL
#
#     # Put data into space-time format
#     Z  <- t(df)
#
#     sp_mean <-  apply(df, 1, mean)
#     nn  <- ncol(df)
#
#     # Subtract and standardise and compute the SVD
#     Zn <- 1/sqrt(nn - 1)*(Z  - outer(rep(1,nn), sp_mean))
#     E <- svd(Zn)
#
#     # Extract the matrix V which contains the EOFs in space-time format and append
#     # lat lon to it.
#     V <- E$v
#     colnames(E$v)  <- paste0("EOF",  1:ncol(SSTdata))        # label each column
#     EOFs <- cbind(SSTlonlat[-delete_rows,  ], E$v)
#     head(EOFs[, 1:3])
#
#     nEOF <-  neof
#     EOFset1  <- E$u[1:(nn-lag),  1:nEOF]  * sqrt(nn - 1)
#     EOFset2  <- E$u[(lag+1):nn,  1:nEOF]  * sqrt(nn - 1)
#
#     cc  <-  CCA::cancor(EOFset1, EOFset2)
#
#     # Compute the  series of the first canonical variables
#     CCA_df  <- data.frame(t  =  rep(1:(nn-lag), 2),
#                           ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
#                                    (EOFset2 %*%  cc$ycoef[,1])[,1]),
#                           Variable = c(rep("CCA1", nn-lag), rep("CCA2", nn-lag)))
#
#     # Visualizing the Linear Weights
#
#     EOFs_CCA <- data.frame(lon = EOFs$lon, lat = EOFs$lat)
#     EOFs_CCA$SW1 <- as.numeric(as.matrix(EOFs[,3:12]) %*% cc$xcoef[,1])
#     EOFs_CCA$SW2 <- as.numeric(as.matrix(EOFs[,3:12]) %*% cc$ycoef[,1])
#     dim(EOFs_CCA)
#     names(EOFs_CCA)
#   }
#
#   # Compute the  series of the first canonical variables
#   CCA_df  <- data.frame(t  =  rep(1:(nn-lag), 2),
#                         ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
#                                  (EOFset2 %*%  cc$ycoef[,1])[,1]),
#                         Variable = c(rep("CCA1", nn-lag), rep("CCA2", nn-lag)))
#
#
#
#   # Now plot
#   sst_cca <- ggplot(CCA_df, aes(x=t, y=ts, colour = Variable)) + geom_line(size = 0.8) +
#     scale_color_manual(values = c("dark blue", "dark red")) +
#     ylab("CCA  variables")  +
#     xlab(xlab)  +
#     theme_bw() +
#     ggtitle("Canonical Correlation Analysis")
#
#
#   sst_cca
#
#
#
#   # Plotting Weights as Spatial Maps: First Canoncial Variable
#
#   spwts_p <- ggplot(EOFs_CCA, aes(lon, lat)) +
#     geom_tile(aes(fill = SW1)) +
#     fill_scale(palette = "Spectral" ,"SW1") +
#     scale_y_reverse() + ylab("Latitude") +
#     xlab("Longitude") +
#     ggtitle("Spatial Weights: Canonical Variable")  +
#     theme_bw()
#
#   spwts_p
# }
