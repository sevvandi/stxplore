#' Plots Canonical Correlation Analysis variables from cannonical_correlation function.
#'
#' Plots Canonical Correlation Analysis (CCA) variables using 2 datasets.
#'
#' @param object  The output of the function `cannonical_correlation'.
#' @param xlab The xlabel to appear on CCA plot.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' df1 <- SSTdatashort[1:100, ]
#' df2 <- SSTdatashort[401:500, ]
#' cca <- canonical_correlation(df1, df2)
#' autoplot(cca, , xlab = "Longitude")
#'
#' @importFrom stats cancor
#' @export
autoplot.cancor <- function(object,
                            xlab = "Time",
                            ...){

  t <- ts <- Variable <- NULL
  CCA_df <- object$cancor_df

  # Now plot
  sst_cca <- ggplot(CCA_df, aes(x=t, y=ts, colour = Variable)) + geom_line(size = 0.8) +
    scale_color_manual(values = c("dark blue", "dark red")) +
    ylab("CCA  variables")  +
    xlab(xlab)  +
    theme_bw() +
    ggtitle("Canonical Correlation Analysis")

  sst_cca

}
