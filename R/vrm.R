#' @title Vector Ruggedness Measure (VRM)
#' @description Implementation of the Sappington et al., (2007) vector 
#'              ruggedness measure
#'
#' @param x          Elevation raster class object
#' @param s          Scale of window. Must be odd number, can 
#'                   represent 2 dimensions (eg., s=c(3,5) would 
#'                   represent a 3 x 5 window)
#' @param file.name  Name of output raster (optional)
#' @param ...        Additional arguments passed to writeRaster
#'
#' @return raster class object or raster written to disk
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
#'  library(raster)
#'  data(elev)
#'    vrm3 <- vrm(elev) 
#'    vrm5 <- vrm(elev, s=5)
#'    plot(stack(vrm3, vrm5))
#'
#' @export vrm
vrm <- function(x, s = 3, file.name = NULL, ...) {
    if (class(x)[1] != "RasterLayer") 
        stop("x must be a raster object")
	if(length(s) > 2) stop( "Specified window exceeds 2 dimensions")   
      if(length(s) == 1) s = rep(s,2)
  scale.factor <- round(s[1] * s[2], 0)
  sa <- raster::terrain(x, opt=c("slope", "aspect"), unit="radians", 
                        neighbors=8) 					
  sin.slp <- raster::calc(sa[["slope"]], fun=sin)                 # xyRaster 
  cos.slp <- raster::calc(sa[["slope"]], fun=cos)                 # zRaster 
  sin.asp <- raster::calc(sa[["aspect"]], fun=sin) * sin.slp      # yRaster
  cos.asp <- raster::calc(sa[["aspect"]], fun=cos) * sin.slp      # xRaster  
  f = matrix(1,s[1],s[2]) 
  x.sum <- raster::focal(sin.asp, w = f, fun=sum) 
  y.sum <- raster::focal(cos.asp, w = f, fun=sum) 
  z.sum <- raster::focal(cos.slp, w = f, fun=sum) 
  vrm.fun <- function(x, y, z) { 
    sqrt( (x^2) + (y^2) + (z^2) ) 
    }
  r <- raster::overlay(x.sum, y.sum, z.sum, fun=vrm.fun)
    if(!is.null(file.name)) {
      return( raster::writeRaster(1 - (r / scale.factor), filename = file.name, ...) )
	  } else {
	  return( 1 - (r / scale.factor) )
    }
  }
