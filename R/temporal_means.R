#' Computes temporal empirical means
#'
#' This function computes temporal empirical means averaged per time unit.
#'
#' @inheritParams temporal_snapshots.data.frame
#' @inheritParams spatial_snapshots.data.frame
#'
#' @examples
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' temporal_means(Tmax,
#'        t_col = 'date',
#'        z_col = 'z',
#'        id_col = 'id')
#' @export temporal_means
temporal_means <- function(x,
                   t_col,
                   z_col,
                   id_col){
  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
  }

  if(missing(t_col)){
    stop("Date/time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  if(missing(id_col)){
    stop("Location id column is not specified. Use id_col to specify location id.")
  }

  df <- x
  z <- df[ ,z_col]
  t <- df[ ,t_col]
  id <- df[ ,id_col]
  meanz <- NULL
  date <- NULL

  df2 <- data.frame(z = z, date = t, id = id)

  df_av <- dplyr::group_by(df2, date) %>%
    dplyr::summarise(meanz = mean(z))

  structure(list(
    data = df2,
    averages = df_av,
    call = match.call()
  ), class='temporalmeans')
}



