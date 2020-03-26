#' @title Local minimum and maximum
#' @description 
#' Calculates the local minimums and maximums in a numeric vector, 
#' indicating inflection points in the distribution.  
#' 
#' @param x A numeric vector 
#' @param dev Deviation statistic (mean or median)
#' @param plot plot the minimum and maximum values with the 
#'             distribution (TRUE/FALSE)
#' @param add.points Should all points of x be added to 
#'                   plot (TRUE/FALSE)
#' @param ... Arguments passed to plot
#' 
#' @return A list object with:  
#' * minima - minimum local values of x
#' * maxima - maximum local values of x
#' * mindev - Absolute deviation of minimum from specified deviation 
#'          statistic (dev argument) 
#' * maxdev - Absolute deviation of maximum from specified deviation 
#'          statistic (dev argument) 
#' @md
#'
#' @note 
#' Useful function for identifying inflection or enveloping points in 
#' a distribution
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples                                                  
#' x <- rnorm(100,mean=1500,sd=800) 
#' ( lmm <- local.min.max(x, dev=mean, add.points=TRUE, 
#'                        main="Local Minima and Maxima") )
#'
#' # return only local minimum values
#'    local.min.max(x)$minima 
#'                                            
#' @export
local.min.max <- function(x, dev=mean, plot=TRUE, add.points=FALSE,  ...) {
  x <- stats::na.omit(x)
    r <- rle(x) 
  minima <- which(rep(x=diff(sign(diff(c(-Inf, r$values, -Inf)))) == 2, times=r$lengths))
  maxima <- which(rep(x=diff(sign(diff(c(-Inf, r$values, -Inf)))) == -2, times=r$lengths)) 
    if (plot == TRUE) {				 
      plot(x,type="l", ...)
        graphics::points(x[minima]~minima,pch=19,col="blue") 
          graphics::points(x[maxima]~maxima,pch=19,col="red")
		    graphics::abline(h=dev(x, na.rm=TRUE), col="grey")
		if (add.points == TRUE) graphics::points(x, col="grey")
	  graphics::legend("topleft", legend=c("Minima","Maxima"), pch=c(19,19), 
	                   col=c("blue","red"), bg="white")
    }
	return( list(minima=x[minima], maxima=x[maxima],
				 devmin=abs(dev(x) - x[minima]), 
				 devmax=abs(dev(x) - x[maxima])) )
 }	
 