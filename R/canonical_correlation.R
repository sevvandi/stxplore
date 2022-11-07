#' Computes transformed variables from Canonical Correlation Analysis
#'
#' Computes Canonical Correlation Analysis (CCA) using 2 datasets.
#'
#' @param df1 The first dataset.
#' @param df2 The second dataset. The dimensions of both datasets need to be the same.
#'
#' @examples
#' df1 <- SSTdatashort[1:100, ]
#' df2 <- SSTdatashort[401:500, ]
#' canonical_correlation(df1, df2)
#'
#' @importFrom stats cancor
#' @export canonical_correlation
canonical_correlation <- function(df1, df2){
  # option 1 - using 2 separate datasets - using raw
  if(missing(df1)){
    stop("Empty dataframe df1. Please give a dataframe with N1 rows and N2 columns.")
  }

  if(missing(df2)){
    stop("Empty dataframe df2. Please give a dataframe with N1 rows and N2 columns.")
  }

  if(!identical(dim(df1),dim(df2))){
    stop("The datasets need to have the same dimensions.")
  }

  t <- ts <- Variable <- NULL

  nn <- dim(df1)[1]
  dd <- dim(df1)[2]
  if(nn < dd){
    print('Rows less than columns. Using the transpose to perform CCA')
    df1 <- t(df1)
    df2 <- t(df2)
  }

  nn <- dim(df1)[1]
  cc <- cancor(df1, df2)

  df1cc <- as.matrix(df1) %*% cc$xcoef[ ,1]
  df2cc <- as.matrix(df2) %*% cc$ycoef[ ,1]
  # Compute the  series of the first canonical variables
  CCA_df  <- data.frame(t  =  rep(1:nn, 2),
                        ts  =  c(df1cc, df2cc),
                        Variable = c(rep("CCA1-df1", nn), rep("CCA1-df2", nn)))


  structure(list(
    cancor_df = CCA_df,
    df1 = df1,
    df2 = df2,
    call = match.call()
  ), class='cancor')

}
