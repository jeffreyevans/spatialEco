#' @title Heat Load Index
#' @description Calculates the McCune & Keon (2002) Heat Load Index
#' 
#' @param x                rasterLayer class object
#' @param check            (TRUE/FALSE) check for projection integrity and
#'                          calculate central latitude for non-geographic
#'                          projections 
#' @param force.hemisphere  If country is split at the equator, force southern 
#'                          or northern hemisphere equation c("southern", "northern")  
#' 
#' @return raster class object of McCune & Keon (2002) Heat Load Index
#'
#' @note
#' Describes A southwest facing slope should have warmer temperatures than a 
#' southeast facing slope, even though the amount of solar radiation they receive 
#' is equivalent. The McCune and Keon (2002) method accounts for this by "folding" 
#' the aspect so that the highest values are southwest and the lowest values are  
#' northeast. Additionally, this method account for steepness of slope, which is 
#' not addressed in most other aspect rescaling equations. HLI values range 
#' from 0 (coolest) to 1 (hottest). 
#' @note
#' The equations follow McCune (2007) and support northern and southern hemisphere 
#' calculations. The folded aspect for northern hemispheres use (180 - (Aspect – 225) ) 
#' and for Southern hemisphere  ( 180 - ( Aspect – 315) ). If a country is split at the 
#' equator you can use the force.hemisphere argument to choose which equation to use. 
#' Valid values for this argument are "southern" and "northern" with the default "none".     
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' McCune, B., and D. Keon (2002) Equations for potential annual direct 
#'   incident radiation and heat load index. Journal of Vegetation 
#'   Science. 13:603-606.
#' @references
#' McCune, B. (2007). Improved estimates of incident radiation and heat load 
#'   using non-parametric regression against topographic variables. Journal 
#'   of Vegetation Science 18:751-754. 
#' 
#' @examples 
#'   library(raster)
#'   data(elev)
#'   heat.load <- hli(elev)
#'     plot(heat.load, main="Heat Load Index") 
#'     
#' @export hli
hli <- function(x, check = TRUE, force.hemisphere = c("none", "southern", "northern")) {
  if (!inherits(x, "RasterLayer")) stop("x must be a RasterLayer object") 
  if(check) {
    if (is.na(sp::proj4string(x))) stop("Projection must be defined")
    if (length(grep("longlat", sp::proj4string(x))) <= 0) {
	  geo.prj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      e <- as(raster::extent(x), "SpatialPolygons")
        sp::proj4string(e) <- sp::proj4string(x)	  
          e <- sp::spTransform(e, geo.prj )
	  l = sp::coordinates(e)[2]
    } else {
      e <- as(raster::extent(x), "SpatialPolygons")  
	    l = sp::coordinates(e)[2]
    }
  } else {
    e <- as(raster::extent(x), "SpatialPolygons")  
    l = sp::coordinates(e)[2]
  } 
  if(l < 0) hemisphere = "southern" else hemisphere = "northern"  
  l = abs(l) * 0.017453293	
    cl = cos(l)
      sl = sin(l)
    tmp1 <- raster::terrain(x, opt="slope", unit="degrees") * 0.017453293
      tmp2 <- raster::terrain(x, opt="aspect", unit="degrees") * 0.017453293
    if(hemisphere == "northern" | force.hemisphere[1] == "northern"){ 
	  message("Using folded aspect equation for Northern hemisphere") 	
	    # Folded Aspect Northern Hemisphere  (180 - (Aspect – 225) )
		#   180(deg)=3.141593(rad), 225=3.92699(rad)
        tmp3 <- raster::calc(tmp2, fun=function(x) { abs(3.141593 - abs(x - 3.926991)) } ) 
	} else if(hemisphere == "southern" | force.hemisphere[1] == "southern") {
	  message("Using folded aspect equation for Southern hemisphere") 		
		# Folded Aspect Southern Hemisphere  ( 180 - ( Aspect – 315) )  
		#   180(deg)=3.141593(rad), 315=5.49779 
		tmp3 <- raster::calc(tmp2, fun=function(x) { abs(3.141593 - abs(x - 5.497791)) } ) 
	}	
          tmp4 <- raster::calc(tmp1, fun = cos)
        tmp5 <- raster::calc(tmp1, fun = sin)
      tmp6 <- raster::calc(tmp3, fun = cos)
    tmp7 <- raster::calc(tmp3, fun = sin)
	h <- exp( -1.467 +  1.582 * cl * tmp4  - 1.5 * tmp6 * tmp5 * sl - 0.262 * 
             sl * tmp5  + 0.607 * tmp7 * tmp5)
	if(cellStats(h,"max") > 1){
	  h <- ( h / cellStats(h, "max", asSample=FALSE) ) 
    }		  
  return( h )
 } 
