#' @title shift
#' @description Shift a vector by specified positive or negative lag
#'       
#' @param x     A vector
#' @param lag   Number of lagged offsets, default is 1
#' @param pad   Value to fill the lagged offset with, default is NA
#' 
#' @return a vector, length equal to x, with offset length filled with pad values
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' x <- 1:10
#' 
#' shift(x, 1)     # shift positive (from beginning of vector) by 1
#' shift(x, -1)    # shift negative (from end of vector) by 1
#' shift(x, 5, 0)  # Shift by 5 and fill (pad) with 0
#'
#' @export shift
shift <- function(x, lag=1, pad = NA) {
  if(!is.na(pad)) 
    message(paste0("Padding shifted values with - ", pad))
  n <- length(x)
  y <- rep(pad, length(x))
    if (lag < 0) {
	  message(paste0("Shifting ", abs(lag), " positions from end of vector")) 
      y[1:(n-abs(lag))] <- x[(abs(lag)+1):n]
    } else if (lag > 0) {
	  message(paste0("Shifting ", lag, " positions from beginning of vector"))
      y[(lag+1):length(x)] <- x[1:(length(x)-lag)]
    }
  return(y)
}
