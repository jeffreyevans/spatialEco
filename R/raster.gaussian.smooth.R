#' @title Gaussian smoothing of raster
#' @description Applies a Gaussian smoothing kernel to smooth raster. 
#' 
#' @param x         raster object
#' @param type      The statistic to use in the smoothing operator 
#'                  (suggest mean or sd)
#' @param sigma     standard deviation (sigma) of kernel (default is 2)
#' @param n         Size of the focal matrix, single value (default is 
#'                  5 for 5x5 window) 
#' @param ...       Additional arguments passed to raster::focal 
#' 
#' @return raster class object of the local distributional moment
#'
#' @note
#'  This is a simple wrapper for the focal function, returning local 
#'  statistical moments 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#'    library(raster)
#'    r <- raster(nrows=500, ncols=500, xmn=571823, xmx=616763, 
#'                ymn=4423540, ymx=4453690)
#' proj4string(r) <- crs("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")
#'    r[] <- runif(ncell(r), 1000, 2500)
#'    r <- focal(r, focalWeight(r, 150, "Gauss") )
#'  
#'  # Calculate Gaussian smoothing with sigma(s) = 1-4
#'  g1 <- raster.gaussian.smooth(r, sigma=1, nc=11)
#'  g2 <- raster.gaussian.smooth(r, sigma=2, nc=11)
#'  g3 <- raster.gaussian.smooth(r, sigma=3, nc=11)
#'  g4 <- raster.gaussian.smooth(r, sigma=4, nc=11)
#' 
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(2,2)) 
#'   plot(g1, main="Gaussian smoothing sigma = 1") 
#'   plot(g2, main="Gaussian smoothing sigma = 2")
#'   plot(g3, main="Gaussian smoothing sigma = 3")
#'   plot(g4, main="Gaussian smoothing sigma = 4")
#' par(opar)
#'
#' @export
raster.gaussian.smooth <- function(x, sigma = 2, n = 5, type = mean, ...) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
    gm <- gaussian.kernel(sigma=sigma, n=n)
	return( raster::focal(x, w = gm, fun = type, na.rm=TRUE, pad=FALSE, ...) )
}  
