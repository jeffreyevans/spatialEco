#' @title Dissection
#' @description Calculates the Evans (1972) Martonne's modified 
#'              dissection 
#' 
#' @param x    A terra SpatRaster class object
#' @param s    Focal window size
#' @param ...  Additional arguments passed to terra::lapp
#' 
#' @return A SpatRaster class object of Martonne's modified dissection  
#'
#' @note
#' Dissection is calculated as: 
#' ( z(s) - min(z(s)) ) / ( max(z(s)) - min(z(s)) ) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples
#' \donttest{ 
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'   d <- dissection(elev, s=3)
#'     plot(d, main="dissection") 
#' }     
#'
#' @export dissection
dissection <- function(x, s=5, ...) {  
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")		
    if( length(s) == 1) s = c(s[1],s[1])
       m <- matrix(1, nrow=s, ncol=s)
    rmin <- terra::focal(x, w=m, fun=min)
    rmax <- terra::focal(x, w=m, fun=max)
    return( terra::lapp(c(x,rmin,rmax), fun = function(x,y,z) { (x - y ) / 
	        ( z - y ) }, ... )   )
}  
