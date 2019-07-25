#' @title Smooth Raster Time-series
#' @description Smooths pixel-level data in raster time-series and can impute missing (NA) values.
#'
#' @param x            A raster stack or brick class object
#' @param f            Smoothing parameter (see loess span argument)
#' @param smooth.data  (FALSE/TRUE) Smooth all of the data or just impute NA values 
#' @param ...          Additional arguments passed to raster calc (for writing results to disk)
#' 
#' @return A raster stack or brick object with imputed NA values or smoothed data.  
#'
#' @details  
#' This function uses a LOESS regression to smooth the time-series (using the smooth.data = TRUE argument). 
#' If the data is smoothed, it will be replaced by a loess estimate of the time-series (estimated distribution 
#' at the pixel-level). The results can dramatically be effected by the choice of the smoothing parameter (f) 
#' so caution is warranted and the effect of this parameter tested. Alternately, with smooth.data = FALSE,
#' the function can be used to impute missing pixel data (NA) in raster time-series (stacks/bricks). 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \dontrun{
#'  # fill NA values
#'  lai.new <- smooth.time.series(lai)  
#'
#'  # Smooth time-series with a relaxed smoothing parameter
#'  lai.smooth <- smooth.time.series(lai, f = 0.2, smooth.data = TRUE)  
#' }
#'
#' @seealso \code{\link[stats]{loess}} for details on the loess regression  
#' @seealso \code{\link[raster]{calc}} for details on additional (...) arguments 
#'  
#' @export smooth.time.series
smooth.time.series <- function(x, f = 0.80, smooth.data = FALSE, ...) { 
  if(class(x) != "RasterStack" & 
     class(x) != "RasterBrick" & 
     class(x) != "SpatialPixelsDataFrame" & 
     class(x) != "SpatialGridDataFrame")
    stop("x must be a raster stack, brick of sp raster class object")
	
  impute.loess <- function(y, x.length = NULL, s = f, sdata = smooth.data, 
                           na.rm, ...) {
    if(is.null(x.length)) { x.length = length(y) }
      options(warn=-1)  
	#if( length(na.omit(x)) < 2 )  
      x <- 1:x.length
  	  p <- stats::loess(y ~ x, span = s, data.frame(x=x, y=y))
  	if(sdata == TRUE) {
  	  y <- stats::predict(p, x)
  	} else {
      na.idx <- which( is.na(y) )
        if( length(na.idx) > 1 ) {
          y[na.idx] <- stats::predict(p, data.frame(x=na.idx))
  	    }
    }  
    return(y)
  }
  if(class(x) == "RasterStack" | class(x) == "RasterBrick") { 
    if(raster::nlayers(x) < 5)
      warning("function is intended for imputing missing values 
	           in multi-temporal data\n      < 5 observations is questionable\n")
    #return( raster::calc(x, fun=impute.loess, ...) )
	return( raster::overlay(x, fun=impute.loess, unstack=TRUE, forcefun=FALSE, ...) )
  } else if(class(x) == "SpatialPixelsDataFrame" | class(x) == "SpatialGridDataFrame") {
      if(raster::ncol(x) < 5)
        warning("function is intended for imputing missing values 
	             in multi-temporal data\n      < 5 observations is questionable\n")
    return( x@data <- as.data.frame(t(apply(x@data, MARGIN=1, FUN = impute.loess, 
	                               x.length = ncol(x)))) ) 
  }
  
}
