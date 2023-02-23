#' @title Vector Ruggedness Measure (VRM)
#' @description Implementation of the Sappington et al., (2007) vector 
#'              ruggedness measure
#'
#' @param x          A terra SpatRaster class object
#' @param s          Scale of window. Must be odd number, can 
#'                   represent 2 dimensions (eg., s=c(3,5) would 
#'                   represent a 3 x 5 window)
#'
#' @return A terra SpatRaster class object of the VRI
#' 
#' @note 
#' This function measures terrain ruggedness by calculating the vector 
#' ruggedness measure
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @references 
#' Sappington, J.M., K.M. Longshore, D.B. Thomson (2007). Quantifying Landscape 
#'   Ruggedness for Animal Habitat Analysis: A case Study Using Bighorn Sheep in 
#'   the Mojave Desert. Journal of Wildlife Management. 71(5):1419-1426
#'
#' @examples 
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'    vrm3 <- vrm(elev) 
#'    vrm5 <- vrm(elev, s=5)
#'    plot(c(vrm3, vrm5))
#'
#' @export vrm
vrm <- function(x, s = 3) {
  if (!inherits(x, "SpatRaster")) 
	stop(deparse(substitute(x)), " must be a terra SpatRaster object")
	if(length(s) > 2) 
	  stop( "Specified window exceeds 2 dimensions") 
    if(any((s %% 2) == 0))
      stop( "Specified window must be odd number(s)") 
    if(length(s) == 1) s = rep(s,2)
  vrm.fun <- function(x, y, z) { 
    sqrt( (x^2) + (y^2) + (z^2) ) 
    }
  f = matrix(1,s[1],s[2])	
  scale.factor <- round(s[1] * s[2], 0)
  sa <- terra::terrain(x, v=c("slope", "aspect"), 
                       unit="radians", neighbors=8) 
  sin.slp <- terra::app(sa[["slope"]], fun=sin)                  # xyRaster 
    cos.slp <- terra::app(sa[["slope"]], fun=cos)                # zRaster 
      sin.asp <- terra::app(sa[["aspect"]], fun=sin) * sin.slp   # xRaster
        cos.asp <- terra::app(sa[["aspect"]], fun=cos) * sin.slp # yRaster  
          x.sum <- terra::focal(sin.asp, w = f, fun=sum) 
        y.sum <- terra::focal(cos.asp, w = f, fun=sum) 
      z.sum <- terra::focal(cos.slp, w = f, fun=sum) 
    r <- terra::lapp(c(x.sum, y.sum, z.sum), fun=vrm.fun)
  return( 1 - (r / scale.factor) )
}
