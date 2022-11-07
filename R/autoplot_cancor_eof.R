#' Plots Canonical Correlation Analysis from cancor_eof function.
#'
#' Plots CCA using EOF analysis using a lagged dataset.
#'
#' @param object The output of the function `cancor_eof'.
#' @param line_plot If set to \code{TRUE}, then the line plot is included.
#' @param space_plot If set to \code{TRUE}, the space splot is included.
#' @param palette The color palette to use for plotting.
#' @param xlab The label on the x-axis for the line plot.
#' @param ... Other arguments currently ignored.
#'
#'
#' @examples
#' cca <- cancor_eof(SSTlonlatshort,
#'                  SSTdatashort,
#'                  n_eof = 8)
#' autoplot(cca)
#' autoplot(cca, line_plot = FALSE)
#' autoplot(cca, space_plot = FALSE)
#' @export
autoplot.cancoreof <- function(object,
                               line_plot = TRUE,
                               space_plot = TRUE,
                               palette = 'Spectral',
                               xlab = "Time",
                               ...){

  SW1 <- t <- ts <- Variable <- lon <- lat <- NULL
  CCA_df <- object$cancor_df
  EOFs_CCA <- object$eofs_df

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
