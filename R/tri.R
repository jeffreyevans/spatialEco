#' @title Terrain Ruggedness Index
#' @description Implementation of the Riley et al (1999) Terrain Ruggedness Index
#'
#' @param r      A terra SpatRaster class object
#' @param s      Scale of window. Must be odd number, can represent 2 
#'               dimensions (eg., s=c(3,5) would represent a 3 x 5 window)
#' @param exact  Calculate (TRUE/FALSE) the exact TRI or an algebraic 
#'               approximation. 
#' @param ...    Additional arguments passed to terra::focal or terra::app
#'
#' @details
#' The algebraic approximation is considerably faster. However, because 
#' inclusion of the center cell, the larger the scale the larger the divergence 
#' of the minimum value. Resuls are driven by local variations so, fixed thresholds
#' are not very reliable. However there are some reccomended breaks (eg., Riley et al., 1999). 
#' 
#' Riley et al., (1999) ranges for classifying Topographic Ruggedness Index:
#'   * 0-80 - level terrain surface.
#'   * 81-116 - nearly level surface.
#'   * 117-161 - slightly rugged surface.
#'   * 162-239 - intermediately rugged surface.
#'   * 240-497 - moderately rugged surface.
#'   * 498-958 - highly rugged surface.
#'   * gt 959 - extremely rugged surface. 
#' @md 
#'
#' @return A terra SpatRaster class object of the TRI
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @references 
#' Riley, S.J., S.D. DeGloria and R. Elliot (1999) A terrain 
#'   ruggedness index that quantifies topographic heterogeneity, 
#'   Intermountain Journal of Sciences 5(1-4):23-27.
#'
#' @examples 
#' \donttest{
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'   ( tri.ext <- tri(elev) )
#'   ( tri.app <- tri(elev, exact = FALSE) )
#'   plot(c(tri.ext, tri.app))
#' }
#'
#' @export
tri <- function(r, s = 3, exact = TRUE, ...) {
  if (!inherits(r, "SpatRaster")) 
	stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if(length(s) > 2) stop( "Specified window exceeds 2 dimensions")   
    if(length(s) == 1) s = rep(s,2)
  r.sqrt <- function(x) {
    x <- x[x >= 0]
    if(length(x) >= 1) { 
      return(sqrt(x)) 
    } else{
      return(NA)  
    }
  }	
  tri.calc <- function(x) {
    xc <- x[ceiling(length(x)/2)]
    x <- x[-ceiling(length(x)/2)]
    x <- x[!is.na(x)]
      if(!is.na(xc) & length(x) > 0) {
        x.dev <- vector()
        for(i in 1:length(x)) { x.dev <- append(x.dev, (xc - x[i])^2) }
  	  return( sqrt(sum(x.dev)) )
      } else {
      return( NA )  
    }
  }	  
  if(exact == TRUE) {  
    return( terra::focal(r, w=matrix(1,s[1],s[2]), fun=tri.calc, ...) )
  } else {
    sx <- terra::focal(r, w = matrix(1,s[1],s[2]), sum)
      e2 <- r * r
      st <- terra::focal(e2, w = matrix(1,s[1],s[2]), sum)
    r2 <- st + (s[1]*s[2]) * e2 - 2 * r * sx
  return( terra::app(r2, fun= r.sqrt, ...) )
  }
}
