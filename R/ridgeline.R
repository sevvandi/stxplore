#' Ridgeline plots grouped by an attribute using a dataframe as an input.
#'
#' Plots ridgeline plots grouped by latitude/longitude or time.
#'
#' @inheritParams ridgeline
#' @inheritParams spatial_snapshots.data.frame
#' @param group_col The column name of the group column.
#'
#' @examples
#' library(dplyr)
#' data(NOAA_df_1990)
#' TmaxJan <- filter(NOAA_df_1990,
#'                  proc == "Tmax" &
#'                  year == 1993 &
#'                  month == 1)
#' ridgeline(TmaxJan,
#'       group_col = 'lat',
#'       z_col = 'z',
#'       xlab = 'Maximum Temperature',
#'       ylab = 'Latitude Intervals')
#'
#' @importFrom ggplot2 scale_fill_viridis_c stat
#' @importFrom ggridges geom_density_ridges_gradient
#' @export
ridgeline.data.frame <- function(x,
                                 num_grps = 10,
                                 xlab = "Value",
                                 ylab = "Group Intervals",
                                 title = "",
                                 legend_title = "z",
                                 group_col,
                                 z_col,
                                 ...){

  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
  }

  if(missing(group_col)){
    stop("Group column not specified. Use group_col to specify group.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  df <- x
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



#' Ridgeline plots grouped by an attribute using a stars object as input.
#'
#' Plots ridgeline plots grouped by latitude/longitude or time.
#'
#' @inheritParams ridgeline
#' @param group_dim The dimension for the grouping variable.
#'
#' @examples
#' library(stars)
#' library(dplyr)
#' library(units)
#'
#' # Example 1
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' ridgeline(prec, group_dim = 1)
#'
#'
#' # Example 2
#' tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
#' x <- read_stars(tif)
#' dim(x)
#' ridgeline(x, group_dim = 1)
#' ridgeline(x, group_dim = 2)
#'
#'
#' # Example 3
#' tif = system.file("tif/lc.tif", package = "stars")
#' x <- read_stars(tif)
#' ridgeline(x, group_dim = 1)
#' ridgeline(x, group_dim = 2)
#'
#' @importFrom ggplot2 scale_fill_viridis_c stat
#' @importFrom ggridges geom_density_ridges_gradient
#' @export
ridgeline.stars <- function(x,
                            num_grps = 10,
                            xlab = "Value",
                            ylab = "Group Intervals",
                            title = "",
                            legend_title = "z",
                            group_dim,
                            ...){

  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }

  if(missing(group_dim)){
    stop("Dimension to group by is not specified. Use group_dim to specify group.")
  }

  xx <- x
  df <- dplyr::as_tibble(x)
  ctgrp <- x <- NULL
  grp <- dplyr::pull(df, group_dim)
  value_dim <- dim(df)[2]
  z <- dplyr::pull(df,value_dim)


  df2 <- data.frame(grp = grp, z = z)
  df2$ctgrp <- cut(df2$grp, breaks = num_grps)

  ggplot(df2, aes(x = z, y = ctgrp, group = ctgrp, fill = stat(x) )) +
    geom_density_ridges_gradient(rel_min_height = 0.01) +
    scale_fill_viridis_c(name = legend_title,  option = "C") +
    ylab(ylab) +
    xlab(xlab)
}

