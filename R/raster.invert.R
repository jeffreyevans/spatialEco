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
#' library(terra)
#' r <- rast(nrows=500, ncols=500, xmin=571823, xmax=616763, 
#'             ymin=4423540, ymax=4453690)
#'  crs(r) <- "epsg:9001"
#' r[] <- runif(ncell(r), 1000, 2500)
#' r <- focal(r, focalMat(r, 150, "Gauss") )
#'
#' r.inv <- raster.invert(r)
#'
#' opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(1,2))
#'       plot(r, main="original raster")
#'       plot(r.inv, main="inverted raster") 
#' par(opar)   
#'
#' @export raster.invert
raster.invert <- function(x) {  
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
      rmax <- terra::global(x, "max", na.rm = TRUE)[,1]
	rmin <- terra::global(x, "min", na.rm = TRUE)[,1]
  return( ((x - rmax) * -1) + rmin )
}  
