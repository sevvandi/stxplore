#' Computes empirical orthogonal functions
#'
#' Computes empirical orthogonal functions of the data.
#'
#' @inheritParams semivariogram.data.frame
#'
#' @examples
#' # Dataframe example
#' data(SSTlonlatshort)
#' data(SSTdatashort)
#' data(SSTlandmaskshort)
#' # Take first 396 months (33 years) and delete land
#' delete_rows <- which(SSTlandmaskshort  ==  1)
#' SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
#' emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
#'              SSTdatashort)
#'
#' @export emp_orth_fun
emp_orth_fun <- function(x,
                         ...){
  UseMethod("emp_orth_fun")
}
