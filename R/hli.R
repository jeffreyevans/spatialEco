#' @title Heat Load Index
#' @description Calculates the McCune & Keon (2002) Heat Load Index
#' 
#' @param x      raster object
#' @param check  (TRUE/FALSE) check for projection integrity in northern latitudes 
#' 
#' @return raster class object of McCune & Keon (2002) Heat Load Index
#'
#' @note
#' Describes A southwest facing slope should have warmer temperatures than a southeast facing slope, 
#' even though the amount of solar radiation they receive is equivalent. The McCune and Keon (2002) 
#' method accounts for this by "folding" the aspect so that the highest values are southwest and the 
#' lowest values are northeast. Additionally, this method account for steepness of slope, which is 
#' not addressed in most other aspect rescaling equations. HLI values range from 0 (coolest) to 1 (hottest).
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references McCune, B., and D. Keon (2002) Equations for potential annual direct incident radiation 
#'               and heat load index. Journal of Vegetation Science. 13:603-606.
#' 
#' @examples 
#'   library(raster)
#'   data(elev)
#'   heat.load <- hli(elev)
#'     plot(heat.load, main="Heat Load Index") 
#'     
#' @export
hli <- function(x, check = TRUE) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
  
  if(check) {
    if (is.na(sp::proj4string(x))) stop("Projection must be defined")
    if (length(grep("longlat", sp::proj4string(x))) <= 0) {
	  geo.prj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      e <- as(raster::extent(x), "SpatialPolygons")
        sp::proj4string(e) <- sp::proj4string(x)	  
          e <- sp::spTransform(e, geo.prj )
	  l = sp::coordinates(e)[2]
	  if(l < 0) stop("Not currently supported for southern latitudes") 
    } else {
      e <- as(raster::extent(x), "SpatialPolygons")  
	  l = sp::coordinates(e)[2]
	  if(l < 0) stop("Not currently supported for southern latitudes") 
    }
  } else {
    e <- as(raster::extent(x), "SpatialPolygons")  
    l = sp::coordinates(e)[2]
  }  
  l = l * 0.017453293	
  cl = cos(l)
  sl = sin(l)
    tmp1 <- raster::terrain(x, opt="slope", unit="degrees") * 0.017453293              
      tmp2 <- raster::terrain(x, opt="aspect", unit="degrees") * 0.017453293   
        tmp3 <- raster::calc(tmp2, fun=function(x) { abs(3.141593 - abs(x - 3.926991)) } )       
          tmp4 <- raster::calc(tmp1, fun=cos)
        tmp5 <- raster::calc(tmp1, fun=sin)
      tmp6 <- raster::calc(tmp3, fun=cos)
    tmp7 <- raster::calc(tmp3, fun=sin)
  return( exp( -1.467 +  1.582 * cl * tmp4  - 1.5 * tmp6 * tmp5 * sl - 0.262 * 
              sl * tmp5  + 0.607 * tmp7 * tmp5) )
 } 
