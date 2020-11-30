#' @title Diurnal Anisotropic Heat Index
#' @description 
#' Simple approximation of the anisotropic diurnal heat (Ha) distribution 
#'
#' @param x          An elevation raster of class RasterLayer, SpatRaster or 
#'                   SpatialPixelsDataFrame 
#' @param amax       The Alpha Max (amax) parameter in degrees defined 
#'                   as: minimum = 0, maximum = 360 with the default = 202.500  
#'
#' @return RasterLayer class object Diurnal Anisotropic Heat Index
#'
#' @description
#' The Diurnal Anisotropic Heat Index is based on this equation.  
#' Ha = cos(amax - a) * arctan(b)
#'   Where; amax defines the aspect with the maximum total heat  
#'   surplus, a is the aspect and b is the slope angle. 
#' 
#' @references
#'  Boehner, J., and Antonic, O. (2009) Land-surface parameters specific to 
#'    topo-climatology. In: Hengl, T., & Reuter, H. (Eds.), Geomorphometry -
#'    Concepts, Software, Applications. Developments in Soil Science, 
#'    33:195-226 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' library(raster)
#' data(elev)
#' Ha <- dahi(elev)
#'   plot(Ha)
#'
#' @export dahi
dahi <- function(x, amax = 202.500) {
  if(class(x) == "RasterLayer") {
    tr <- raster::terrain(x, opt=c("slope", "aspect"), 
                          unit="degrees")				  
  } else if(class(x) == "SpatRaster") {
    tr <- raster::terrain(raster::raster(x), opt=c("slope", "aspect"), 
                          unit="degrees")
  } else if(class(x) == "SpatialPixelsDataFrame") {
    tr <- raster::terrain(raster::raster(x), opt=c("slope", "aspect"), 
                          unit="degrees")
  }
  return( cos(amax - tr[[2]]) * atan(tr[[1]]) )
}
