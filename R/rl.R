#' Ridgeline plots grouped by an attribute.
#'
#' Plots ridgeline plots grouped by latitude/longitude or time.
#'
#' @inheritParams spatial_snapshots
#' @param group_col The column name of the group column.
#' @param num_grps The number of levels for the ridgeline plot.
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' TmaxJan <- filter(NOAA_df_1990,
#'                  proc == "Tmax" &
#'                  year == 1993 &
#'                  month == 1)
#' st_rl(TmaxJan,
#'       group_col = 'lat',
#'       z_col = 'z',
#'       xlab = 'Maximum Temperature',
#'       ylab = 'Latitude Intervals')
#'
#' @importFrom ggplot2 scale_fill_viridis_c stat
#' @importFrom ggridges geom_density_ridges_gradient
#' @export st_rl
st_rl <- function(df,
                  group_col,
                  z_col,
                  num_grps = 10,
                  xlab = "Value",
                  ylab = "Group Intervals",
                  title = "",
                  legend_title = "z"){

  if(missing(df)){
    stop("Empty dataframe df. Please give a proper input.")
  }

  if(missing(group_col)){
    stop("Group column not specified. Use group_col to specify group.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  ctgrp <- x <- NULL
  grp <- df[ ,group_col]
  z <- df[ ,z_col]


  df2 <- data.frame(grp = grp, z = z)
  df2$ctgrp <- cut(df2$grp, breaks = num_grps)

  ggplot(df2, aes(x = z, y = ctgrp, group = ctgrp, fill = stat(x) )) +
    geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = legend_title,  option = "C") +
    ylab(ylab) +
    xlab(xlab)
}
