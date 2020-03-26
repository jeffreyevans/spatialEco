#' @title Raster moments
#' @description Calculates focal statistical moments of a raster  
#' 
#' @param x         raster object
#' @param type      The global statistic to represent the local deviation  
#'                  options are: "min", "min", "mean", "median", "var, "sd", 
#'                  "mad", "kurt", "skew", "quantile"  
#' @param s         Size of matrix (focal window), can be single value or two 
#'                  values defining the [x,y] dimensions of the focal matrix
#' @param p         if type="quantile", the returned percentile.  
#' 
#' @return raster class object of the local distributional moment
#'
#' @note
#'  This is a simple wrapper for the focal function, returning local 
#'  statistical moments 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \donttest{
#'   library(raster)
#'   r <- raster(nrows=100, ncols=100, xmn=571823, xmx=616763, 
#'               ymn=4423540, ymx=4453690)
#'   proj4string(r) <- crs("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")
#'   r[] <- runif(ncell(r), 1000, 2500)
#'   r <- focal(r, focalWeight(r, 150, "Gauss") )
#' 
#' # Calculate 10th percentile for 3x3 window
#' r.p10 <- raster.moments(r, type="quantile", p=0.10) 
#' }
#'
#' @export raster.moments
raster.moments <- function(x, type = "mean", s = 3, p = 0.75) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
  if( length(s) == 1) s = c(s[1],s[1])
    m <- matrix(1, nrow=s, ncol=s)
	if(type == "quantile") {
	  pct <- function(x, q = p) { stats::quantile(x, probs=q, na.rm=TRUE) } 
	  return( raster::focal(x, w = m, fun = pct) )
	} else {
	  return( raster::focal(x, w = m, fun = match.fun(type), na.rm = TRUE) )
    }	
}  
