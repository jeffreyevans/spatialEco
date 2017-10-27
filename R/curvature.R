#' @title Surface curvature
#' @description Calculates McNab's or Bolstad's curvature 
#' 
#' @param x      rasterLayer object
#' @param s      Focal window size
#' @param type   Method used (mcnab or bolstad)
#' 
#' @return raster class object of surface curvature
#'
#' @note
#' McNab's and Bolstad's variants of the surface curvature (concavity/convexity) index (McNab 1993; Bolstad & Lillesand 1992; McNab 1989). The index is based on features that confine the view from the center of a 3x3 window. In the Bolstad equation, edge correction is addressed be dividing by the radius distance to the outermost cell (36.2m).
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references
#' Bolstad, P.V., and T.M. Lillesand (1992). Improved classification of forest vegetation in northern Wisconsin through a rule-based combination of soils, terrain, and Landsat TM data. Forest Science. 38(1):5-20.
#' McNab, H.W. (1989). Terrain shape index: quantifying effect of minor landforms on tree height. Forest Science. 35(1):91-104.
#' McNab, H.W. (1993). A topographic index to quantify the effect of mesoscale landform on site productivity. Canadian Journal of Forest Research. 23:1100-1107.
#'
#' @examples 
#'   library(raster)
#'   data(elev)
#'
#'   m.crv <- curvature(elev, s=5, type="mcnab")
#'   b.crv <- curvature(elev, s=5, type="bolstad")
#'     par(mfrow=c(1,2))
#'       plot(m.crv, main="McNab curvature") 
#'       plot(b.crv, main="Bolstad curvature")
#'     
#' @export
curvature <- function(x, s = 3, type="mcnab") {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
    if( length(s) == 1) s = c(s[1],s[1])
       m <- matrix(1, nrow=s, ncol=s)
  if(type == "bolstad") {
    return( 10000 * ((x - raster::focal(x, w=m, fun=mean)) / 1000 / 36.2) )  
  } else {
    mcnab <- function(x) (((x[5] - x) + (x[5] - x)) / 4) / 36.2
    return( raster::focal(x, w=m, fun=mcnab) )
  }  
}  
