#' @title Surface Area Ratio
#' @description Calculates the Berry (2002) Surface Area Ratio based on slope
#' 
#' @param x    raster object
#' @param s    cell resolution (default is NULL, not needed if projection 
#'             is in planar units)
#' @param ...  Additional arguments passed to raster::calc
#' 
#' @return raster class object of Berry (2002) Surface Area Ratio
#'
#' @note SAR is calculated as: resolution^2 * cos( (degrees(slope) * (pi / 180)) )
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references 
#' Berry, J.K. (2002). Use surface area for realistic calculations. Geoworld 15(9):20-1.
#'
#' @examples 
#'   library(raster)
#'   data(elev)
#'   surface.ratio <- sar(elev, s=90)
#'   plot(surface.ratio)
#'     
#' @export
sar <- function(x, s = NULL, ...) {  
  if (!inherits(x, "RasterLayer")) 
    stop("MUST BE RasterLayer OBJECT")
  if (length(grep("longlat", raster::crs(x))) > 0 && is.null(s))
    stop("Projection is geographic, must define cell size argument in planar units")
    if( is.null(s) ) s = raster::res(x)[1] * raster::res(x)[2] 
      saf <- function(x, cs = s) { cs * cos(x) } 
      slp <- raster::terrain(x, out='slope', unit='degrees', flatAspect=0) * (pi / 180)  
    return( raster::calc(slp, fun=saf, ...) )
}  
