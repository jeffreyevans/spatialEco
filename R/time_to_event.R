#' @title Time to event
#' @description Returns the time (sum to position) to a specified value 
#'       
#' @param x       A vector, representing time-series, to evaluate 
#' @param y       Threshold value tor return position for
#' @param dir     Direction of evaluation c("LR", "RL")
#' @param int     FALSE | TRUE - Evaluate as integer (rounds to 0 decimal places)
#' @param up.to   FALSE | TRUE - Return value before event
#'
#' @return A vector value representing the time to event 
#'
#' @description
#' The time to event represents the sum of positions, in the vector,
#' until the specified value is found ie., (0,0,1) would be 3 or, 
#' 2 with up.to=TRUE. The int argument allows for rounding a continuous  
#' variable. Since it may be difficult to fine an exact match to a floating 
#' point value rounding mitigates the problem. If you want a specific rounding 
#' value (eg., 1 decimal place) you can apply it to x first then pass it to 
#' the function.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#'
#' library(raster)
#' 
#' # Binomial instance
#' time_to_event(c(0,0,0,0,1,0,0,0,1,0))
#' time_to_event(c(0,0,0,0,1,0,0,0,1,0), up.to = TRUE)
#' time_to_event(c(0,0,0,0,1,0,0,0,1,0), dir="RL")
#' 
#' r <- do.call(raster::stack, replicate(20,raster::raster(matrix(sample(
#'              c(0,1), 1000, replace=TRUE), 100, 100))))             
#'   ( t2e <- calc(r, fun=time_to_event) )
#' 
#' # Continuous threshold instance
#' ( x <- runif(100, 0,7) ) 
#' time_to_event(x, y = 5, int=TRUE)
#' 
#' r <- do.call(raster::stack, replicate(20,raster::raster(matrix(
#'              runif(1000,0,7), 100, 100))))
#'   t2e <- function(x) { time_to_event(x, y=5, int=TRUE) }              
#'   ( t2e <- calc(r, fun=time_to_event) )
#'
#' @export
time_to_event <- function(x, y = 1, dir = c("LR", "RL"), int = FALSE,
                          up.to = FALSE) {
    if(dir[1] == "RL") x <- rev(x)
	  if(int) x <- round(x,0)
        x.idx <- rle(x)$values
        e.idx <- rle(x)$lengths 
    if(x[1] == y) {
      if(up.to) e = 0 else e = 1
    } else {
	  e <- sum(e.idx[1:(which(x.idx >= y)[1]-1)])
    } 
  if(up.to) return(e) else return( e + 1 )
}
