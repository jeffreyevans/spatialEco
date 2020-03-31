#' @title Invert raster
#' @description Inverts (flip) the values of a raster
#' 
#' @param x raster object
#' 
#' @return 
#' raster class object with inverted (flipped) raster values
#'
#' @note
#' Inverts raster values using the formula: (((x - max(x)) * -1) + min(x)
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples 
#' \donttest{
#'   library(raster)
#'   r <- raster(nrows=500, ncols=500, xmn=571823, xmx=616763, 
#'               ymn=4423540, ymx=4453690)
#'     r[] <- runif(ncell(r), 1, 100)
#'	 r <- focal(r, focalWeight(r, 150, "Gauss") )
#'   r.inv <- raster.invert(r)
#'
#' opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(1,2))
#'       plot(r, main="original raster")
#'       plot(r.inv, main="inverted raster") 
#' par(opar)
#' }     
#'
#' @export raster.invert
raster.invert <- function(x) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
    rmax <- raster::cellStats(x, stat = "max", na.rm = TRUE, asSample = FALSE)
	rmin <- raster::cellStats(x, stat = "min", na.rm = TRUE, asSample = FALSE)
    return( ((x - rmax) * -1) + rmin )
}  
