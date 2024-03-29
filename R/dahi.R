#' @title Diurnal Anisotropic Heat Index
#' @description 
#' Simple approximation of the anisotropic diurnal heat (Ha) distribution 
#'
#' @param x          An elevation raster of class terra SpatRaster
#' @param amax       The Alpha Max (amax) parameter in degrees defined 
#'                   as: minimum = 0, maximum = 360 with the default = 202.500  
#'
#' @details 
#' The Diurnal Anisotropic Heat Index is based on this equation.  
#' Ha = cos(amax - a) * arctan(b)
#'   Where; amax defines the aspect with the maximum total heat  
#'   surplus, a is the aspect and b is the slope angle. 
#'
#' @return terra SpatRaster class object Diurnal Anisotropic Heat Index
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references
#'  Boehner, J., and Antonic, O. (2009) Land-surface parameters specific to 
#'    topo-climatology. In: Hengl, T., & Reuter, H. (Eds.), Geomorphometry -
#'    Concepts, Software, Applications. Developments in Soil Science, 
#'    33:195-226 

#' @examples
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#' Ha <- dahi(elev)
#'   plot(Ha)
#'
#' @export dahi
dahi <- function(x, amax = 202.500) {
  if(!inherits(x, "SpatRaster"))
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
    tr <- terra::terrain(x, v=c("slope", "aspect"), unit="degrees")
  return( cos(amax - tr[[2]]) * atan(tr[[1]]) )
}
