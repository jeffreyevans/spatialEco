#' @title Gaussian smoothing of raster
#' @description Applies a Gaussian smoothing kernel to smooth raster. 
#' 
#' @param x         A terra SpatRaster raster object
#' @param type      The statistic to use in the smoothing operator 
#'                  (suggest mean or sd)
#' @param sigma     standard deviation (sigma) of kernel (default is 2)
#' @param n         Size of the focal matrix, single value (default is 
#'                  5 for 5x5 window) 
#' @param ...       Additional arguments passed to terra::focal 
#' 
#' @return A terra SpatRaster class object of the local distributional moment
#'
#' @note
#'  This is a simple wrapper for the focal function, returning local 
#'  statistical moments 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' library(terra)
#' r <- rast(nrows=500, ncols=500, xmin=571823, xmax=616763, 
#'             ymin=4423540, ymax=4453690)
#'  crs(r) <- "epsg:9001"
#' r[] <- runif(ncell(r), 1000, 2500)
#' r <- focal(r, focalMat(r, 150, "Gauss") )
#' 
#' # Calculate Gaussian smoothing with sigma(s) = 1-4
#' g1 <- raster.gaussian.smooth(r, sigma=1, nc=11)
#' g2 <- raster.gaussian.smooth(r, sigma=2, nc=11)
#' g3 <- raster.gaussian.smooth(r, sigma=3, nc=11)
#' g4 <- raster.gaussian.smooth(r, sigma=4, nc=11)
#'
#'   opar <- par(no.readonly=TRUE)
#'   par(mfrow=c(2,2)) 
#'     plot(g1, main="Gaussian smoothing sigma = 1") 
#'     plot(g2, main="Gaussian smoothing sigma = 2")
#'     plot(g3, main="Gaussian smoothing sigma = 3")
#'     plot(g4, main="Gaussian smoothing sigma = 4")
#'   par(opar)
#'
#' @export
raster.gaussian.smooth <- function(x, sigma = 2, n = 5, type = mean, ...) {  
    if (!inherits(x, "SpatRaster")) 
	  stop(deparse(substitute(x)), " must be a terra SpatRaster object")
    gm <- gaussian.kernel(sigma=sigma, s=n)
	return( terra::focal(x, w = gm, fun = type, ...) )
}  
