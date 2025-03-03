#' @title Heat Load Index
#' @description Calculates the McCune & Keon (2002) Heat Load Index
#' 
#' @param aspect               terra SpatRaster class object of aspect
#' @param slope                terra SpatRaster class object of slope 
#' @param latitude             Vector of center latitude of raster, if NULL automatical calculated
#' @param direct               (FALSE/TRUE) default is "heatload", alternative is "radiation"
#' @param scaled               (FALSE/TRUE) default is "heatload", alternative is "radiation"
#' @param units                Default is in "degrees", alternative is "radians"
#' @param hemisphere           Define if in "southern" or "northern" hemisphere, default is northern
#' @param force.hemisphere     Force hemisphere to southern or northern
#' @param equation             Which equation to use (1,2,3), default is 1 covering all slopes <= 90
#'                             and latitudes 0-60N
#' 
#' @details
#' Describes A southwest facing slope should have warmer temperatures than a 
#' southeast facing slope, even though the amount of solar radiation they receive 
#' is equivalent. The McCune and Keon (2002) method accounts for this by "folding" 
#' the aspect so that the highest values are southwest and the lowest values are  
#' northeast. Additionally, this method accounts for steepness of slope, which is 
#' not addressed in most other aspect rescaling equations. 
#' 
#' The equations follow McCune (2007) and support northern and southern hemisphere 
#' calculations. The . If a country is split at the McCune (2007) folded aspect corrections 
#' for northern and southern latitudes are included. If you are bounding the equator you can 
#' use the force.hemisphere argument to choose which equation to use. Valid values for this 
#' argument are "southern" and "northern" with the default "none". Metic values are returned 
#' in ln(Rad, MJ  cm–2 yr–1) however, if scale is TRUE they are returned in an arithmetic scale.     
#'
#' Equations - the equation number (1, 2 or 3); default is 1 (the most general one). The three 
#' equations have slightly different uses. Eq. 1 (default in the function below) is broadest in 
#' the application, covering all slopes <= 90 degrees in steepness at latitudes 0-60N, but has the 
#' lowest precision. Eq. 2 increases the precision by excluding slopes steeper than 60, an 
#' inconsequential omission for almost all data sets. Eq. 3 uses only three parameters to 
#' produce a slightly stronger model but is applicable only to latitudes 30-60N (ie., not for the 
#' tropical and subtropical region).
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
#'   sa <- terrain(elev, v = c("slope", "aspect")) 
#'   heat.load <- hli(sa[[2]], sa[[1]])
#'     plot(heat.load, main="Heat Load Index", smooth=TRUE) 
#'    
#'   heat.load2 <- hli(sa[[2]], sa[[1]], equation = 2)
#'     plot(heat.load2, main="Heat Load Index eq 2", smooth=TRUE) 
#'
#' @export hli
hli <- function(aspect, slope, latitude = NULL, direct = FALSE, 
                scaled = FALSE, units = c("degrees", "radians"), 
				hemisphere = c("northern", "southern"),
				force.hemisphere = TRUE, 
				equation = c(1, 2, 3)){
  rad <- function(x) { x * (pi / 180) }
  if (!inherits(aspect, "SpatRaster")) 
    stop(deparse(substitute(aspect)), " must be a terra SpatRaster class object") 
  if(is.na(sf::st_crs(terra::crs(aspect))))	
    stop("Projection is not defined for aspect")	
  if (!inherits(slope, "SpatRaster")) 
    stop(deparse(substitute(slope)), " must be a terra SpatRaster class object") 
  if(is.na(sf::st_crs(terra::crs(slope))))	
    stop("Projection is not defined for slope")
  if(terra::ext(slope) != terra::ext(aspect))	
    stop("Raster extents do not match")	
  if(!equation[1] %in% c(1,2,3))
    stop("Not a valid option for equation, must be 1, 2, or 3")	  
  if(is.null(latitude)) {
    e <- sf::st_as_sf(terra::as.polygons(terra::ext(slope)))
	                  sf::st_crs(e) <- sf::st_crs(slope)
	  latitude <- as.numeric(sf::st_coordinates(sf::st_centroid(
	                         sf::st_transform(e, sf::st_crs(4326))))[,2])
  } 
  if(force.hemisphere) {
    hemisphere == hemisphere[1]
  } else {
    if(any(latitude <= 0)) { 
	  hemisphere = "southern" 
	} else if(latitude > 0) {
	  hemisphere = "northern" 
    }	  
  }
  if(hemisphere[1] == "northern"){ 
    message("Using folded slope equation for Northern hemisphere") 	
     if(!direct) {
	    A <- abs(pi - abs(rad(aspect) - pi * 5/4))    # orginal 2002 equation
        # A <- (rad(180) - (rad(aspect) - rad(255)))  # modified 2007 equation
      } else {
	    A <- pi - abs(rad(aspect) - pi)               # orginal 2002 equation
        # A <- (rad(180) - (rad(aspect) - rad(180)))  # modified 2007 equation
    }
  } else if(hemisphere[1] == "southern") {
    message("Using folded slope equation for Southern hemisphere") 		
    if(!direct) {
       A <- 180 - (aspect - 315)
     } else {
       A <- aspect - 180
    }
  }
  if (units[1] == "degrees") {
    message("Converting slope and aspect degrees to radians")
      S <- rad(slope)
  } else if(units[1] == "radians") {
    S <- slope
    message("Assuming data is in radians, excepting latitude")
  }  
  L <- rad(latitude)
    if(equation[1] == 1) {
    res <- -1.467 + 1.582 * cos(L) * cos(S) - 1.5 * cos(A) * 
		    sin(S) * sin(L) - 0.262 * sin(L) * sin(S) + 0.607 * 
			sin(A) * sin(S)
  } else if(equation[1] == 2) { 
    res <- -1.236 + 1.35 * cos(L) * cos(S) - 1.376 * cos(A) * sin(S) * sin(L) - 
	        0.331 * sin(L) * sin(S) + 0.375 * sin(A) * sin(S)

  } else if (equation[1] == 3) {
    if( latitude < 30 | latitude > 60)
      warning("outside the 30-60N latitude bounds that are suitable for equation 3")	
    res <- 0.339 + 0.808 * cos(L) * cos(S) - 0.196 * sin(L) * sin(S) - 0.482 * 
	       cos(A) * sin(S)
  }
    if(scaled) res <- exp(res)
  return (res)
}

# theta (slope) = 180; alpha (slope) = 30; latitude = 40

# -1.467 + 1.582 * cos(L) * cos(S) - 1.5 * cos(A) * sin(S) * sin(L) - 0.262 * sin(L) * sin(S) + 0.607 * sin(A) * sin(S)

# -1.236 + 1.35 * cos(L) * cos(S) - 1.376 * cos(A) * sin(S) * sin(L) - 0.331 * sin(L) * sin(S) + 0.375 * sin(A) * sin(S)

# 0.339 + 0.808 * cos(L) * cos(S) - 0.196 * sin(L) * sin(S) - 0.482 * cos(A) * sin(S)