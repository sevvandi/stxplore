#' Plots temporal empirical means computed from temporal_means algorithm
#'
#' A line plot of temporal means with observed data in the background.
#'
#' @inheritParams temporal_snapshot
#' @param object The output of the function `temporal_means'.
#' @param legend_title The title for the legend.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' temp_means <- temporal_means(Tmax,
#'               t_col = 'date',
#'               z_col = 'z',
#'               id_col = 'id')
#' autoplot(temp_means)
#' @export
autoplot.temporal_means <- function(object,
                                    ylab = "Mean Value",
                                    xlab ="Month",
                                    legend_title ="",
                                    title = "Temporal Empirical Means",
                                    ...){

  df <- object$data
  df_av <- object$averages

  meanz <- date <- NULL

  p <- ggplot() +
       geom_line(data = df,
              aes(x = df[ ,2],
                  y = df[ ,1],
                  group = df[ ,3],
                  colour = "blue"),
              alpha = 0.04) +
    geom_line(data = df_av,
              aes(x = date,
                  y = meanz,
                  colour = "black")) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    scale_color_manual(name = legend_title,
                       values = c(blue = "blue", black = "black"),
                       labels = c("Observed", "Average"),
                       guide = "legend") +
    ggtitle(title)
  p

}


