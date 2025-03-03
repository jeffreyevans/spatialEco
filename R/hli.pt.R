#' @title Point estimate of Heat Load Index
#' @description Calculates the McCune & Keon (2002) Heat Load Index
#' 
#' @param alpha                numeric vector of aspect (degrees or radians)
#' @param theta                numeric vector of slope (degrees or radians)
#' @param latitude             Numeric vector of center latitude 
#' @param direct               (FALSE/TRUE) default is "heatload", alternative is "direct radiation"
#' @param scaled               (FALSE/TRUE) scale metric to arithmetic scale 
#' @param units                What units are slope and aspect in "degrees" or "radians"
#' @param hemisphere           Define if in "southern" or "northern" hemisphere, default is northern
#' @param force.hemisphere     Force hemisphere to southern or northern (defined in hemisphere arg_ 
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
#' in a log scale ln(Rad, MJ  cm–2 yr–1) however, if scale is TRUE they are returned in an 
#' arithmetic scale.     
#'
#' Equations - the equation number (1, 2 or 3); default is 1 (the most general one). The three 
#' equations have slightly different uses. Eq. 1 (default in the function below) is broadest in 
#' the application, covering all slopes <= 90 degrees in steepness at latitudes 0-60N, but has the 
#' lowest precision. Eq. 2 increases the precision by excluding slopes steeper than 60, an 
#' inconsequential omission for almost all data sets. Eq. 3 uses only three parameters to 
#' produce a slightly stronger model but is applicable only to latitudes 30-60N (ie., not for the 
#' tropical and subtropical region). Note that Eq is on an arithmetic scale 
#'
#' @return numeric vector of of McCune & Keon (2002) Heat Load Index
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
#'
#' # Single input
#' hli.pt(alpha=30, theta=180, latitude=40) 
#' 
#' # Results from McCune, B., and D. Keon (2002)
#' # Equation 1
#' # ln(Rad, MJ  cm–2 yr–1) -0.2551  -0.6280  0.0538  -0.6760  -1.1401  -0.2215
#' # arithmetic scale        0.7748   0.5337  1.0553   0.5086   0.3198   0.8013
#'
#' slp = c(0, 30, 30, 0, 30, 30)
#' asp = c(0, 0, 180, 0, 0, 180)
#' lat = c(40, 40, 40, 60, 60, 60)
#' hli.pt(alpha = asp, theta = slp, latitude = lat)
#' hli.pt(alpha = asp, theta = slp, latitude = lat, scaled=TRUE)

#' # Equation 2
#' # arithmetic scale  0.8172   0.5304   0.5291   0.5706   0.3354   0.3344
#' hli.pt(alpha = asp, theta = slp, latitude = lat, equation = 2, scaled=TRUE)
#'
#' # Equation 3
#' # arithmetic scale  0.9580  0.6416  0.9825  0.7430  0.4336  0.7744
#' hli.pt(alpha = asp, theta = slp, latitude = lat, equation = 3)
#'   
#' @export hli.pt
hli.pt <- function(alpha, theta, latitude, direct = FALSE, 
                   scaled = FALSE, units = c("degrees", "radians"), 
				   hemisphere = c("northern", "southern"),
				   force.hemisphere = TRUE, 
				   equation = c(1, 2, 3)){
  rad <- function(x) { x * (pi / 180) }
  if(missing(alpha)) 
    stop("alpha is missing must be defined")	
  if(!inherits(alpha, "numeric")) 
    stop("alpha must be numeric")
  if(missing(theta)) 
    stop("theta is missing must be defined")	
  if (!inherits(theta, "numeric")) 
    stop("theta must be numeric") 	
  if(missing(latitude)) 
    stop("latitude is missing must be defined")	
  if (!inherits(latitude, "numeric")) 
    stop("x must be numeric")
  if(length(alpha) != length(theta))
    stop("alpha and theta are not the same length")  
  if (any(latitude < -90) | any(latitude > 90))
    stop("latitude is out of range")
  if(force.hemisphere) {
    hemisphere == hemisphere[1]
  } else {
    if(any(latitude <= 0)) { 
	  hemisphere = "southern" 
	} else if(latitude > 0) {
	  hemisphere = "northern" 
    }	  
  }
  if(units[1] == "radians") {
    alpha = alpha * ( 180 / pi)
  }
  if(hemisphere[1] == "northern"){ 
    message("Using folded alpha equation for Northern hemisphere") 	
     if(!direct) {
	    A <- abs(pi - abs(rad(alpha) - pi * 5/4)) # orginal equation
        # A <- (rad(180) - (rad(alpha) - rad(255)))
      } else {
	    A <- pi - abs(rad(alpha) - pi)
        #A <- (rad(180) - (rad(alpha) - rad(180)))
    }
  } else if(hemisphere[1] == "southern") {
    message("Using folded alpha equation for Southern hemisphere") 		
    if(!direct) {
       A <- (rad(180) - (rad(alpha) - rad(315)))
     } else {
       A <- (rad(alpha) - rad(180)) 
    }
  }
  if (units[1] == "degrees") {
    message("Converting theta degrees to radians")
      theta <- rad(theta)
  } else if(units[1] == "radians") {
    message("Assuming data is in radians, excepting latitude")
  }  
  S <- theta
  L <- rad(latitude)
    L <- if (length (L) != length(A)) rep(L, length (A)) else L
  if(equation[1] == 1) {
    res <- -1.467 + 1.582 * cos(L) * cos(S) - 1.5 * cos(A) * 
		    sin(S) * sin(L) - 0.262 * sin(L) * sin(S) + 0.607 * 
			sin(A) * sin(S)
  } else if(equation[1] == 2) { 
    res <- -1.236 + 1.35 * cos(L) * cos(S) - 1.376 * cos(A) * sin(S) * sin(L) - 
	        0.331 * sin(L) * sin(S) + 0.375 * sin(A) * sin(S)

  } else if (equation[1] == 3) {
    if(any(latitude < 30 | latitude > 60))
      warning("outside the 30-60N latitude bounds that are suitable for this equation")	
    res <- 0.339 + 0.808 * cos(L) * cos(S) - 0.196 * sin(L) * sin(S) - 0.482 * 
	       cos(A) * sin(S)
  }
  if(scaled) {
    if(equation[1] != 3) res <- exp(res)
  }
  return (res)
}
