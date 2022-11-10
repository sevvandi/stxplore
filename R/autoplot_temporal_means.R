#' Plots temporal empirical means computed from temporal_means function
#'
#' A line plot of temporal means with observed data in the background.
#'
#' @inheritParams temporal_snapshots
#' @param object The output of the function `temporal_means'.
#' @param legend_title The title for the legend.
#' @param ... Other arguments currently ignored.
#'
#' @examples
#' # dataframe example
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
#'
#' # stars example
#' library(stars)
#' library(dplyr)
#' library(units)
#' # Example
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' temp_means <- temporal_means(prec)
#' autoplot(temp_means)
#' @export
autoplot.temporalmeans <- function(object,
                                    ylab = "Value",
                                    xlab ="Time",
                                    legend_title ="",
                                    title = "Temporal Empirical Means",
                                    ...){

  df <- as.data.frame(object$data)
  df_av <- as.data.frame(object$averages)

  num_obs <- dim(df)[1]
  if(num_obs > 50000){  # More than 50000 data points
    set.seed(1)
    ss <- sample(num_obs, 50000)
    df <- df[ss, ]
  }

  meanz <- time <- NULL

  p <- ggplot() +
       geom_line(data = df,
              aes(x = df[ ,2],
                  y = df[ ,1],
                  group = df[ ,3],
                  colour = "blue"),
              alpha = 0.04) +
    geom_line(data = df_av,
              aes(x = time,
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


