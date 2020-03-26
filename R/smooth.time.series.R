#' @title Smooth Raster Time-series
#' @description Smooths pixel-level data in raster time-series and can impute 
#'              missing (NA) values.
#'
#' @param x            A raster stack/brick or sp object with a @data slot 
#' @param f            Smoothing parameter (see loess span argument)
#' @param smooth.data  (FALSE/TRUE) Smooth all of the data or just impute NA values 
#' @param ...          Additional arguments passed to raster calc (for 
#'                     writing results to disk)
#' 
#' @return 
#' A raster stack or brick pr data.frame object with imputed NA values or smoothed data.  
#'
#' @details  
#' This function uses a LOESS regression to smooth the time-series (using the 
#' smooth.data = TRUE argument). If the data is smoothed, it will be replaced by 
#' a loess estimate of the time-series (estimated distribution at the pixel-level). 
#' The results can dramatically be effected by the choice of the smoothing 
#' parameter (f) so caution is warranted and the effect of this parameter tested. 
#' Alternately, with smooth.data = FALSE, the function can be used to impute missing 
#' pixel data (NA) in raster time-series (stacks/bricks). 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \dontrun{
#'  random.raster <- function(r=50, c=50, l=10, min=0, max=1){ 
#'    do.call(stack, replicate(l, raster(matrix(runif(r*c, min, max),r,c))))
#'  }
#'  r <- random.raster()
#'
#'  # Smooth time-series 
#'  r.smooth <- smooth.time.series(r, f = 0.2, smooth.data = TRUE)  
#'  
#'  # sp SpatialPixelsDataFrame example
#'  r <- as(r, "SpatialPixelsDataFrame")
#'  r@data <- smooth.time.series(r, f = 0.2, smooth.data = TRUE)
#'  r <- stack(r) # coerce back to raster stack object
#'
#' }
#' @seealso \code{\link[stats]{loess}} for details on the loess regression  
#' @seealso \code{\link[raster]{calc}} for details on additional (...) arguments 
#'  
#' @export smooth.time.series
smooth.time.series <- function(x, f = 0.80, smooth.data = FALSE, ...) { 
  if(any(class(x) != c("RasterStack","RasterBrick", "SpatialPixelsDataFrame", 
                       "SpatialGridDataFrame")))
    stop("x must be a raster stack, brick of sp raster class object")	
  impute.loess <- function(y, x.length = NULL, s = 0.2, 
                           sdata = FALSE, na.rm, ...) {		 
         if (is.null(x.length)) {
            x.length = length(y)
         }
  	   if(length(y[!is.na(y)]) < 8) {
  	     warning("Fewer than 8 real-value observations, assigning NA")
  		   y <- rep(NA, x.length)
  	   } else {			   
           x <- 1:x.length
             p <- stats::loess(y ~ x, span = s, 
  		             data.frame(x = x, y = y))
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
  if(any(class(x) == c("RasterStack", "RasterBrick"))) { 
    if(raster::nlayers(x) < 8)
      warning("function is intended for imputing missing values 
	           in multi-temporal data\n      < 8 observations is questionable\n")
    #return( raster::calc(x, fun=impute.loess, ...) )
	return( raster::overlay(x, fun = impute.loess, unstack = TRUE, forcefun = FALSE, ...) )
  } else if(any(class(x) == c("SpatialPixelsDataFrame","SpatialGridDataFrame"))) {
      if(raster::ncol(x) < 8)
        warning("function is intended for imputing missing values 
	             in multi-temporal data\n      < 8 observations is questionable\n")
    return( x@data <- as.data.frame(t(apply(x@data, MARGIN=1, FUN = impute.loess, 
	                               x.length = ncol(x)))) ) 
  }  
}
