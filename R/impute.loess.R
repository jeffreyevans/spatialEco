#' @title Impute loess
#' @description Imputes missing data or smooths using Loess regression 
#'
#' @param y       A vector to impute
#' @param s       Smoothing parameter () 
#' @param smooth  (FALSE/TRUE) Smooth data, else only replace NA's
#' 
#' @details  
#' Performs a local polynomial regression to smooth data or to 
#' impute NA values. The minimal number of non-NA observations to reliably
#' impute/smooth values is 6. There is not a reliably way to impute NA's
#' on the tails of the distributions so if the missing data is in the
#' first or last position of the vector it will remain NA. Please note
#' that smooth needs to be TRUE to return a smoothed vector, else only
#' NA's will be imputed.   
#'
#' @return 
#' A vector the same length as x with NA values filled or the data smoothed (or both).
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'
#' @examples 
#' data(cor.data)
#' d <- cor.data[[1]][,2]
#'   plot(d, type="l")
#'   lines(impute.loess(d, s=0.3, smooth=TRUE), lwd=2, col="red")
#'  
#' # add some NA's
#' d <- d[1:100]
#'   d[sample(30:70, 5)] <- NA 
#'   d
#'   
#' impute.loess(d, s=0.2)
#'   
#' @export impute.loess
impute.loess <- function(y, s = 0.2, smooth = FALSE) {
  x.length = length(y)
  if(length(y[!is.na(y)]) < 6) {
    warning("Fewer than 6 real-value observations, assigning NA")
      y <- rep(NA, x.length)
  } else {
    x <- 1:x.length
      p <- suppressWarnings(stats::loess(y ~ x, span = s, 
                            data.frame(x = x, y = y)))
    if (smooth == TRUE) {
      y <- stats::predict(p, x)
    } else {
      na.idx <- which(is.na(y))
        if(length(na.idx) > 1) {
          y[na.idx] <- stats::predict(p, data.frame(x = na.idx))
        }
    }
  }
  return(y)
}
