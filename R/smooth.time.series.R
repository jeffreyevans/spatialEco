#' @title Smooth Raster Time-series
#' @description Smooths pixel-level data in raster time-series and can impute 
#'              missing (NA) values.
#'
#' @param x            A terra SpatRaster with > 8 layers 
#' @param f            Smoothing parameter (see loess span argument)
#' @param smooth.data  (FALSE/TRUE) Smooth all of the data or just impute NA values 
#' @param ...          Additional arguments passed to terra::app (for 
#'                     writing results to disk)
#' 
#' @details  
#' This function uses a LOESS regression to smooth the time-series. If the data is 
#' smoothed, (using the smooth.data = TRUE argument) it will be entirely replaced by  
#' a loess estimate of the time-series (estimated distribution at the pixel-level). 
#' Alternately, with smooth.data = FALSE, the function can be used to impute missing 
#' pixel data (NA) in raster time-series (stacks/bricks).
#' The results can dramatically be effected by the choice of the smoothing 
#' parameter (f) so caution is warranted and the effect of this parameter tested.  
#' 
#' @return 
#' A terra SpatRaster containing imputed or smoothed data.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \donttest{
#' library(terra)
#'   random.raster <- function(rows=50, cols=50, l=20, min=0, max=1){ 
#'     do.call(c, replicate(l, rast(matrix(runif(rows * cols, min, max), 
#' 	        rows , cols))))
#'   }
#' r <- random.raster()
#' 
#' #### Smooth time-series using raster stack/brick 
#' r.smooth <- smooth.time.series(r, f = 0.4, smooth.data = TRUE)  
#' 
#' # extract pixel 100 for plotting
#' y <- as.numeric(r[100])
#' ys <- as.numeric(r.smooth[100])
#' 
#' # plot results	
#' plot(y, type="l")
#'   lines(ys, col="red")
#'     legend("bottomright", legend=c("original","smoothed"),
#'          lty=c(1,1), col=c("black","red"))	
#' }
#' @seealso \code{\link[stats]{loess}} for details on the loess regression  
#' @seealso \code{\link[terra]{app}} for details on additional (...) arguments 
#' @seealso \code{\link[spatialEco]{impute.loess}} for details on imputation model
#'  
#' @export smooth.time.series
smooth.time.series <- function(x, f = 0.80, smooth.data = FALSE, ...) { 
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if(terra::nlyr(x) < 5)
    stop("Not enough observations (time-steps) for imputation or smoothing")  
  if(terra::nlyr(x) < 8)
      warning("function is intended for fitting a distribution 
	           in multi-temporal data\n      < 8 observations is questionable\n")	
  impute.loess <- function(y, x.length = NULL, s = f, 
                           sdata = smooth.data, na.rm, ...) {
         if (is.null(x.length)) {
            x.length = length(y)
         }
  	   if(length(y[!is.na(y)]) < 8) {
  	     warning("Fewer than 8 real-value observations, assigning NA")
  		   y <- rep(NA, x.length)
  	   } else {			   
           x <- 1:x.length
             p <- suppressWarnings( stats::loess(y ~ x, span = s, 
  		                            data.frame(x = x, y = y)) )
         if (sdata == TRUE) {
           y <- stats::predict(p, x)
         } else {
           na.idx <- which(is.na(y))
             if (length(na.idx) > 1) {
               y[na.idx] <- stats::predict(p, data.frame(x = na.idx))
             }
          } 
  	   }
     return(y)
   }   
  return( terra::app(x, fun = impute.loess, ...) ) 
}
