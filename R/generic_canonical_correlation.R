#' Generic function to computes transformed variables from Canonical Correlation Analysis
#'
#' Computes Canonical Correlation Analysis (CCA) using 2 datasets.
#'
#' @param x1 The first dataframe or stars object.
#' @param x2 The second dataframe or stars objext. The dimensions of both datasets need to be the same.
#' @param ... Other arguments that need to be used for datafames or currently ignored.
#'
#' @examples
#' # Dataframe example
#' df1 <- SSTdatashort[1:100, ]
#' df2 <- SSTdatashort[401:500, ]
#' canonical_correlation(df1, df2)
#'
#' @importFrom stats cancor
#' @export canonical_correlation
canonical_correlation <- function(x1,
                                  x2,
                                  ...){
  UseMethod("canonical_correlation")
}
