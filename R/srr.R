#' @title Surface Relief Ratio
#' @description Calculates the Pike (1971) Surface Relief Ratio
#' 
#' @param x    A terra SpatRaster object
#' @param s    Focal window size
#' @param ...  Additional arguments passed to terra::lapp
#' 
#' @return A terra SpatRaster object of Pike's (1971) Surface Relief Ratio
#'
#' @note
#' Describes rugosity in continuous raster surface within a specified window. 
#' The implementation of SRR can be shown as: (mean(x) - min(x)) / (max(x) - min(x)) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples 
#' \donttest{
#'  library(terra)
#'  elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'   r.srr <- srr(elev, s=5)
#'     plot(r.srr, main="Surface Relief Ratio") 
#'  }     
#' @export srr
srr <- function(x, s = 5, ...) {  
  if (!inherits(x, "SpatRaster")) 
	stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  m <- matrix(1, nrow=s, ncol=s)
  rmin <- terra::focal(x, w=m, fun=min)
    rmax <- terra::focal(x, w=m, fun=max)
	  rmean <- terra::focal(x, w=m, fun=mean)
  return( terra::lapp(c(rmean,rmin,rmax), 
    fun=function(x,y,z) { (x - y ) / ( z - y ) }, ... ) )
} 
