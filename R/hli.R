#' @title Heat Load Index
#' @description Calculates the McCune & Keon (2002) Heat Load Index
#' 
#' @param x                terra SpatRaster class object
#' @param check            (TRUE/FALSE) check for projection integrity and
#'                          calculate central latitude for non-geographic
#'                          projections 
#' @param force.hemisphere  If country is split at the equator, force southern 
#'                          or northern hemisphere equation c("southern", "northern")  
#' 
#' @details
#' Describes A southwest facing slope should have warmer temperatures than a 
#' southeast facing slope, even though the amount of solar radiation they receive 
#' is equivalent. The McCune and Keon (2002) method accounts for this by "folding" 
#' the aspect so that the highest values are southwest and the lowest values are  
#' northeast. Additionally, this method account for steepness of slope, which is 
#' not addressed in most other aspect rescaling equations. HLI values range 
#' from 0 (coolest) to 1 (hottest). 
#' 
#' The equations follow McCune (2007) and support northern and southern hemisphere 
#' calculations. The folded aspect for northern hemispheres use (180 - (Aspect – 225) ) 
#' and for Southern hemisphere  ( 180 - ( Aspect – 315) ). If a country is split at the 
#' equator you can use the force.hemisphere argument to choose which equation to use. 
#' Valid values for this argument are "southern" and "northern" with the default "none".     
#'
#' @return terra SpatRaster class object of McCune & Keon (2002) Heat Load Index
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' McCune, B., and D. Keon (2002) Equations for potential annual direct 
#'   incident radiation and heat load index. Journal of Vegetation 
#'   Science. 13:603-606.
#' 
#' McCune, B. (2007). Improved estimates of incident radiation and heat load 
#'   using non-parametric regression against topographic variables. Journal 
#'   of Vegetation Science 18:751-754. 
#' 
#' @examples 
#'   library(terra)
#'   elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'   heat.load <- hli(elev)
#'     plot(heat.load, main="Heat Load Index") 
#'    
#' @export hli
hli <- function(x, check = TRUE, force.hemisphere = c("none", "southern", "northern")) {
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster class object") 
  if(is.na(sf::st_crs(terra::crs(x))))	
    stop("Projection must be defined")	
  if(check) {
	if(!sf::st_is_longlat(x)) {
	  e <- sf::st_as_sf(terra::as.polygons(terra::ext(x)))
	    sf::st_crs(e) <- sf::st_crs(terra::crs(x))
	      l <- sf::st_bbox(sf::st_transform(e, 4326))[2]
    } else {
      l <- terra::ext(x)[3] 
    }	  
  }  
  if(l < 0) hemisphere = "southern" else hemisphere = "northern"  
    l = abs(l) * 0.017453293	
      cl = cos(l)
        sl = sin(l)
      tmp1 <- terra::terrain(x, v="slope", unit="degrees") * 0.017453293
    tmp2 <- terra::terrain(x, v="aspect", unit="degrees") * 0.017453293
	if(hemisphere == "northern" | force.hemisphere[1] == "northern"){ 
	  message("Using folded aspect equation for Northern hemisphere") 	
	    # Folded Aspect Northern Hemisphere  (180 - (Aspect – 225) )
		#   180(deg)=3.141593(rad), 225=3.92699(rad)
        tmp3 <- terra::app(tmp2, fun=function(x) { abs(3.141593 - abs(x - 3.926991)) } ) 
	} else if(hemisphere == "southern" | force.hemisphere[1] == "southern") {
	  message("Using folded aspect equation for Southern hemisphere") 		
		# Folded Aspect Southern Hemisphere  ( 180 - ( Aspect – 315) )  
		#   180(deg)=3.141593(rad), 315=5.49779 
		tmp3 <- terra::app(tmp2, fun=function(x) { abs(3.141593 - abs(x - 5.497791)) } ) 
	}	
          tmp4 <- terra::app(tmp1, fun = cos)
        tmp5 <- terra::app(tmp1, fun = sin)
      tmp6 <- terra::app(tmp3, fun = cos)
    tmp7 <- terra::app(tmp3, fun = sin)
	h <- exp( -1.467 +  1.582 * cl * tmp4  - 1.5 * tmp6 * tmp5 * sl - 0.262 * 
             sl * tmp5  + 0.607 * tmp7 * tmp5)
	if(terra::global(h, "max", na.rm=TRUE)[,1] > 1){
	  h <- ( h / terra::global(h, "max", na.rm=TRUE)[,1] ) 
    }		  
  return( h )
 } 
