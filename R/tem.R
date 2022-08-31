#' Plots temporal empirical means
#'
#' This function plots temporal empirical means averaged per time unit.
#'
#' @inheritParams st_tsnap
#' @param legend_title The title for the legend.
#'
#' @examples
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' st_tem(Tmax,
#'        t_col = 'date',
#'        z_col = 'z',
#'        id_col = 'id')
#' @export st_tem
st_tem <- function(df,
                   t_col,
                   z_col,
                   id_col,
                   ylab = "Mean Value",
                   xlab ="Month",
                   legend_title ="",
                   title = "Temporal Empirical Means"
                  ){
  if(missing(df)){
    stop("Empty dataframe df. Please give a proper input.")
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


  z <- df[ ,z_col]
  t <- df[ ,t_col]
  id <- df[ ,id_col]
  meanz <- NULL
  date <- NULL

  df2 <- data.frame(z = z, date = t, id = id)

  df_av <- dplyr::group_by(df2, date) %>%
    dplyr::summarise(meanz = mean(z))


  ## Temporal Empirical Means
  ggplot() +
    geom_line(data = df2,
              aes(x = date,
                  y = z,
                  group = id,
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

}

