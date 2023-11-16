#' @title Raster moments
#' @description Calculates focal statistical moments of a raster  
#' 
#' @param x         A terra SpatRaster object
#' @param type      The global statistic to represent the local deviation  
#'                  options are: "min", "min", "mean", "median", "var, "sd", 
#'                  "mad", "kurt", "skew", "quantile"  
#' @param s         Size of matrix (focal window), can be single value or two 
#'                  values defining the [x,y] dimensions of the focal matrix
#' @param p         if type="quantile", the returned percentile. 
#' @param ...       Additional arguments passed to terra::focal 
#' 
#' @details
#' This is a simple wrapper for the terra focal function, returning local statistical moments 
#'
#' @return A terra SpatRaster object representing the local distributional moment
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \donttest{
#' library(terra)
#' r <- rast(nrows=500, ncols=500, xmin=571823, xmax=616763, 
#'             ymin=4423540, ymax=4453690)
#'   crs(r) <- "epsg:9001"
#' r[] <- runif(ncell(r), 1000, 2500)
#' 
#' # Calculate 10th percentile for 3x3 window
#' r.p10 <- raster.moments(r, type="quantile", p=0.10) 
#' }
#'
#' @export raster.moments
raster.moments <- function(x, type = "mean", s = 3, p = 0.75, ...) {  
   if (!inherits(x, "SpatRaster")) 
	  stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if( length(s) == 1) s = c(s[1],s[1])
    m <- matrix(1, nrow=s, ncol=s)
	if(type == "quantile") {
	  pct <- function(x, q = p) { stats::quantile(x, probs=q, na.rm=TRUE) } 
	  return( terra::focal(x, w = m, fun = pct, ...) )
	} else {
	  return( terra::focal(x, w = m, fun = match.fun(type), na.rm = TRUE, ...) )
    }	
}  
