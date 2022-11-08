#' @title Solar-radiation Aspect Index
#' @description Calculates the Roberts and Cooper (1989) Solar-radiation 
#'              Aspect Index
#' 
#' @param x    A terra SpatRaster object
#' @param ...  Additional arguments passed to terra::app
#' 
#' @return 
#' A terra SpatRaster object of Roberts and Cooper (1989) Solar-radiation Aspect Index
#'
#' @description 
#' Roberts and Cooper (1989) rotates (transforms) the circular aspect to assign a 
#' value of zero to land oriented in a north-northeast direction, (typically the 
#' coolest and wettest orientation), and a value of one on the hotter, dryer 
#' south-southwesterly slopes. The result is a continuous variable between 0 - 1. 
#' The metric is defined as: trasp = ( 1 - cos((pi/180)(a-30) ) / 2 
#' where; a = aspect in degrees
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references 
#' Roberts. D.W., and Cooper, S.V. (1989). Concepts and techniques of vegetation mapping. 
#' In Land Classifications Based on Vegetation: Applications for Resource Management.
#' USDA Forest Service GTR INT-257, Ogden, UT, pp 90-96
#'
#' @examples 
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'   s <- trasp(elev)
#'     plot(s)
#'     
#' @export trasp
trasp <- function(x, ...) {  
  if (!inherits(x, "SpatRaster")) 
	stop(deparse(substitute(x)), " must be a terra SpatRaster object")
    asp <- terra::terrain(x, v='aspect', unit='degrees') 
  return( terra::app(asp, fun=function(x) { (1 - cos( (3.142/180)  *(x - 30)) ) / 2 }, ... ) )
}  
