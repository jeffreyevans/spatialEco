#' @title Dissection
#' @description Calculates the Evans (1972) Martonne's modified 
#'              dissection 
#' 
#' @param x    raster object
#' @param s    Focal window size
#' @param ...  Additional arguments passed to raster::calc
#' 
#' @return raster class object of Martonne's modified dissection  
#'
#' @note
#' Dissection is calculated as: 
#' ( z(s) - min(z(s)) ) / ( max(z(s)) - min(z(s)) ) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples
#' \donttest{ 
#'   library(raster)
#'   data(elev)
#'   d <- dissection(elev, s=3)
#'     plot(d, main="dissection") 
#' }     
#'
#' @export dissection
dissection <- function(x, s=5, ...) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
    if( length(s) == 1) s = c(s[1],s[1])
       m <- matrix(1, nrow=s, ncol=s)
    rmin <- raster::focal(x, w=m, fun=min)
    rmax <- raster::focal(x, w=m, fun=max)
    return( raster::calc(raster::stack(x,rmin,rmax), fun = function(x) { (x[1] - x[2] ) / 
	        ( x[3] - x[2] ) }, ... )   )
}  
