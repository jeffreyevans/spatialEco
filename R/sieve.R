#' @title Sieve raster data
#' @description Removes contiguous cells < specified query area
#'
#' @param x      An integer terra SpatRaster 
#' @param a      Query area to remove 
#' @param units  The unit to use for area query options are c("m", "km", "ha")
#' 
#' @return 
#' A terra SpatRaster with < a set to NA  
#'
#' @note 
#' A sieve can be used to establish a minimal mapping unit where
#' contiguous cells < specified query area are set to NA
#'
#' @examples
#' \donttest{
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#' m <- matrix(c(100,200,1,200,300,2,300,400,3,400,
#'        500,4, 500,600,5), ncol=3, byrow=TRUE)
#'   x <- classify(elev, m)
#' 
#' # Sieve to a MMU of 60km
#' sv <- sieve(x, a = 60, units = "km")
#'   plot(c(x, sv))
#' } 
#' @export
sieve <- function(x, a, units = c("m", "km", "ha")) {
  if(missing(a))
    stop("Must define query area (a)")
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")		
  message("Sieve applied for a MMU < ", a, units)	
  rs <- terra::zonal(terra::cellSize(x, unit = units[1]), x, 
                     sum, as.raster = TRUE)
    rs <- terra::ifel(rs < a, NA, x)
  return( rs )
}  
