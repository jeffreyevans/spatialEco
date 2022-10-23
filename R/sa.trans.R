#' @title Trigonometric  transformation of a slope and aspect interaction 
#' @description The Trigonometric  Stage (1978) 
#'              [slope * cos(aspect)] or [slope * sin(aspect)] 
#'
#' @param slope       slope values in degrees, radians or percent 
#' @param aspect      aspect values in degrees or radians
#' @param type        Type of transformation, options are: "cos", "sin" 
#' @param slp.units   Units of slope values, options are: "degrees", 
#'                    "radians" or "percent"
#' @param asp.units   Units of aspect values, options are: 
#'                   "degrees" or "radians"
#'
#' @return A vector of the modeled value
#'
#' @description 
#' An a priori assumption of a maximum in the NW quadrant (45 azimuth)
#' and a minimum in the SW quadrant can be replaced by an empirically
#' determined location of the optimum without repeated calculations of
#' the regression fit. In addition it is argued that expressions for
#' the effects of aspect should always be considered as terms involving
#' an interaction with slope (Stage, 1976)
#' @description
#' For slopes from 0% - 100%, The functions are linearized and
#' bounded from -1 to 1. Greater than 100% slopes are treated
#' out of the -1 to 1 range.
#' 
#' @description
#' An alternative for slopes with values approaching infinity is
#' to take the square root of slope/100 to reduce the range of
#' values.By default this model test all values greater than 100%
#' to 101% and flat areas (-1) to nodata.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Stage, A. R. 1976. An Expression of the Effects of Aspect, Slope, 
#'   and Habitat Type on Tree Growth. Forest Science 22(3):457-460.
#'  
#' @examples 
#'  sa.trans(slope = 48.146, aspect = 360.000)
#'
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#' 
#' # Example of slope*cos(aspect)
#' sa <- terra::terrain(elev, v=c("slope", "aspect"), unit="degrees")
#' scosa <- terra::lapp(c(sa[[1]], sa[[2]]), fun = sa.trans)
#'
#' @export
sa.trans <- function(slope, aspect, type = "cos", slp.units = "degrees", 
                       asp.units = "degrees") {			  
  if(slp.units == "degrees") { slope <- ( slope / 0.572957795786 ) * 0.01 }
  if(slp.units == "radians") { slope <- ( (slope * (180 / pi)) / 0.572957795786 ) * 0.01 } 
  if(slp.units == "percent") { slope <- slope * 0.01 } 
  if(asp.units == "degrees") aspect * (pi / 180)
    slope[slope > 1] <- 1.001
    slope[slope <= 0] <- NA
    trans <- as.vector(t(apply(data.frame(slope, aspect), 1, 
	           function (x){ return ( x[1] * cos(x[2]) ) } )))
    return(trans)
  }
