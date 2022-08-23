#' Plots temporal snapshots of data for specific spatial locations
#'
#' This function plots temporal snapshos for specific spatial locations. The location id sample
#' need to be given as a function argument.
#'
#'@inheritParams st_ssnap
#'@param id_col The column of the location id.
#'@param id_sample The sample of location ids to be plotted
#'
#'@examples
#'library(dplyr)
#'data(NOAA_df_1990)
#'Tmax <- filter(NOAA_df_1990,
#'              proc == "Tmax" &
#'              month %in% 5:9 &
#'              year == 1993)
#'Tmax_ID <- unique(Tmax$id)
#'Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#'ids <- sample(Tmax_ID, 10)
#'st_tsnap(Tmax, 't', 'z', 'id', ids)
#'@export

st_tsnap <- function(df, t_col, z_col, id_col, id_sample,  xlab="Time", ylab ="Value", title = ""){


  id <- df[ ,id_col]
  rows <- which(id %in% id_sample)
  id <- df[rows, id_col]
  z <- df[rows ,z_col]
  t <- df[rows ,t_col]

  df2 <- data.frame(t = t, z = z, id = id)

  ggplot(df2) +
    geom_line(aes(x = t, y = z)) +                         # line plot of z against t
    facet_wrap(~id) +                                      # facet by station
    xlab(xlab) +                                           # x label
    ylab(ylab) +                                           # y label
    theme(panel.spacing = unit(1, "lines")) +              # facet spacing
    theme_bw() +                                           # black and white theme
    ggtitle(title)                                         # add title
}
