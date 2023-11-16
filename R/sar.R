#' @title Surface Area Ratio
#' @description Calculates the Berry (2002) Surface Area Ratio based on slope
#' 
#' @param x      A terra SpatRaster object
#' @param s      cell resolution (default is NULL and not needed if projection 
#'               is in planar units)
#' @param scale  (TRUE/FALSE) Scale (row standardize) results 
#' 
#' @details  
#' SAR is calculated as: resolution^2 * cos( (degrees(slope) * (pi / 180)) )
#'
#' @return A terra SpatRaster class object of the Surface Area Ratio
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references 
#' Berry, J.K. (2002). Use surface area for realistic calculations. Geoworld 15(9):20-1.
#'
#' @examples 
#'  library(terra)
#'  elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'  ( surface.ratio <- sar(elev) )
#'    plot(surface.ratio)
#'     
#' @export
sar <- function(x, s = NULL, scale = TRUE) {  
  if (!inherits(x, "SpatRaster")) 
	stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if (length(grep("longlat", terra::crs(x))) > 0 && is.null(s))
    stop("Projection is geographic, must define cell size argument in planar units")
    if( is.null(s) ) {
      s = terra::res(x)[1] ^2 
	} else {
      if(!is.numeric(s))
	    stop("s must be numeric")
    }	 
        saf <- function(x, cs = s) { cs * cos(x) } 
      slp <- terra::terrain(x, v='slope', unit='degrees') * (pi / 180)
	s <- terra::app(slp, fun=saf)
  if(!scale) {
    return( s )
  } else {  
    return( s / terra::global(s, "max", na.rm=TRUE)[,1] )
  }
}  
