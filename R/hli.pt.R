#' @title Point estimate of Heat Load Index
#' @description Calculates the McCune & Keon (2002) Heat Load Index
#' 
#' @param alpha             Aspect in degrees
#' @param theta             Slope in degrees
#' @param latitude          A latitude representing the centrality of the data
#' @param direct            Boolean (FALSE/TRUE) Return direct incident radiation 
#'                          else HLI (default) 
#' @param scaled            Boolean (TRUE/FALSE) Apply arithmetic scale using EXP(h)          
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
#' @return Vector of McCune & Keon (2002) Heat Load Index
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
#' # Single point input
#' hli.pt(theta=180, alpha=30, latitude=40) 
#' 
#' # Multiple input, returns results from 
#' #   McCune, B., and D. Keon (2002)
#' # Raw -0.2551 -0.6280 0.0538 -0.6760 -1.1401 -0.2215
#' # arithmetic scale 0.7748 0.5337 1.0553 0.5086 0.3198 0.8013
#'
#' slp = c(0, 30, 30, 0, 30, 30)
#' asp =c(0, 0, 180, 0, 0, 180)
#' lat =c(40, 40, 40, 60, 60, 60)
#' hli.pt(theta = slp, alpha = asp, latitude = lat)
#'   
#' @export hli.pt
hli.pt <- function(alpha, theta, latitude, direct = FALSE, scaled = TRUE,    
  force.hemisphere = c("none", "southern", "northern")) {
  if (!inherits(alpha, "numeric")) 
    stop("x must be numeric")
  if (!inherits(alpha, "numeric")) 
    stop("x must be numeric") 	
  if (!inherits(latitude, "numeric")) 
    stop("x must be numeric") 	
  if (any(latitude < -180) | any(latitude > 180))
    stop("latitude is out of range")
  if(any(latitude < 0)) hemisphere = "southern" else hemisphere = "northern" 
  latitude = abs(latitude) * pi/180
    theta <- theta * pi/180 
      alpha <- alpha * pi/180   
    cl = cos(latitude)
  sl = sin(latitude) 
    if(hemisphere == "northern" | force.hemisphere[1] == "northern"){ 
	  message("Using folded aspect equation for Northern hemisphere") 	
	    # Folded Aspect Northern Hemisphere  (180 - (Aspect – 225) )
		#   180(deg)=3.141593(rad), 225=3.92699(rad)
		if(!direct) {
          folded.alpha <- abs(pi - abs(alpha - pi * 5/4)) 
        } else {
		  folded.alpha <- pi - abs(alpha - pi)
		}
	} else if(hemisphere == "southern" | force.hemisphere[1] == "southern") {
	  message("Using folded aspect equation for Southern hemisphere") 		
		# Folded Aspect Southern Hemisphere  ( 180 - ( Aspect – 315) )  
		#   180(deg)=3.141593(rad), 315=5.49779 
		folded.alpha <- abs(3.141593 - abs(alpha - 5.497791))
	}	
	  if(!direct) {
	    h <- -1.467 + 1.582 * cos(latitude) * cos(theta) -
                 1.5 * cos(folded.alpha) * sin(theta) * sin(latitude) -
                 0.262 * sin(latitude) * sin(theta) + 0.607 *
                 sin(folded.alpha) * sin(theta)
      } else {
        h <- -1.467 + 1.582 * cos(latitude) * cos(theta) - 1.5 * 
		     cos(folded.alpha) * sin(theta) * sin(latitude) - 0.262 * 
			 sin(latitude) * sin(theta) + 0.607 * sin(folded.alpha) * 
			 sin(theta)
		}				 	 
      if(scaled) h <- exp(h)			 
  return( ifelse(h > 1, 1, h) )
 } 
