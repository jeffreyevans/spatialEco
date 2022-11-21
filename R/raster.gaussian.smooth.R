#' @title Gaussian smoothing of raster
#' @description Applies a Gaussian smoothing kernel to smooth raster. 
#' 
#' @param x         A terra SpatRaster raster object
#' @param s         Standard deviation (sigma) of kernel (default is 2)
#' @param n         Size of the focal matrix, single value (default is 
#'                  5 for 5x5 window) 
#' @param scale     (FALSE/TRUE) Scale sigma to the resolution of the raster
#' @param type      The statistic to use in the smoothing operator; 
#'                  "mean", "median", "sd", "convolution"
#' @param ...       Additional arguments passed to terra::focal 
#' 
#' @return A terra SpatRaster class object of the local distributional moment
#'
#' @note
#' This applies a Gaussian Kernel smoother. The convolution option performs
#' a Gaussian decomposition whereas the other options use the kernel
#' as weights for the given statistic. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#' 
#' # Calculate Gaussian smoothing with sigma = 2 and 7x7 window
#' g1 <- raster.gaussian.smooth(elev, s=2, n=7)
#'     plot(c(elev,g1))
#'
#' @export
raster.gaussian.smooth <- function(x, s = 2, n = 5, scale = FALSE, 
           type = c("mean", "median", "sd", "convolution"), ...) {   
  if(scale) s = s * terra::res(x)[1]
    type=type[1]	
    if (!inherits(x, "SpatRaster")) 
	  stop(deparse(substitute(x)), " must be a terra SpatRaster object")   
	if(type == "convolution") {
	  message("Running a Gaussian decomposition")
	message("Temporary bug in terra::focal failing convolution function")
      fconv <- function(p, K = NULL, sdv=s) {
          if(inherits(p, "matrix")) p <- as.vector(p)
          if(length(is.na(p)) > 0)
            p[is.na(p)] <- mean(p, na.rm=TRUE)
            n <- floor((sqrt(1 + 8 * length(p)) - 1)/2)-1
          if(is.null(K))
              K <- gaussian.kernel(sigma=sdv, s = n)
            K <- K / sum(K)
            X <- as.matrix(Matrix::bandSparse(length(p), 
                   k = seq(-(length(K)-1),0,1), 
                   diag = t(replicate(length(p), rev(K))), 
        		   symm=FALSE))
              lvc <- X %*% as.matrix(p, ncol=1)
      	lcv <- lcv[ceiling(length(lcv)/2)]
          return(as.numeric(lcv[1]))
        }	
      g <- terra::focal(x, w=n, fun=fconv, ...)     
    } else {
      message("Using ", type, " for convolution")
      gm <- gaussian.kernel(sigma=s, s=n)
      g <- terra::focal(x, w=gm, fun=eval(parse(text = type)), ...)	  
	} 
  return( g )
}  
