#' @title 2-dimensional kernel density estimate
#' @description Calculates kernel density estimate, over specified extent, and outputs a raster
#' 
#' @param x               SpatialPoints of SpatialPointsDataFrame object
#' @param bw              Bandwidth of Gaussian Kernel
#' @param n               Number of rows and columns (evaluation points)
#' @param ext             Extent of raster, coordinates as: c(xmin, xmax, ymin, ymax)
#' @param standardize     Standardize results to 0-1 (FALSE/TRUE)
#'
#' @return A list object with:
#' @return   kde        Raster class object of kernel density estimate
#' @return   bandwidth  Bandwidth of kernel
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' library(sp)
#' library(raster)
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' 
#' meuse.kde <- kde2D(meuse, bw = 1000, n = 500, standardize = TRUE)  
#' 
#' plot(meuse.kde$kde)
#'   plot(meuse, pch=20, cex=0.75, col="red", add=TRUE)				   
#' 
#' @export
kde2D <- function (x, bw = NULL, n = 120, ext = NULL, 
                   standardize = FALSE) {
  if (is.null(bw)) { 
    bw <- c(MASS::bandwidth.nrd(sp::coordinates(x)[,1]), 
		    MASS::bandwidth.nrd(sp::coordinates(x)[,2]))
  }		
  if (is.null(ext)) {
    ext <- c(range(sp::coordinates(x)[,1]), 
		     range(sp::coordinates(x)[,2]))
  } else {
    ext <- c(sp::bbox(x)[1],sp::bbox(x)[3],sp::bbox(x)[2],sp::bbox(x)[4]) 
  }  
    kden <- MASS::kde2d(sp::coordinates(x)[,1], sp::coordinates(x)[,2], h = bw, 
	                  n = n, lims = ext)
	if( standardize == TRUE ) {
	  kden$z <- (kden$z - min(kden$z)) / (max(kden$z) - min(kden$z))  
	}				  
    kde.est <- sp::SpatialPoints(expand.grid(kden$x, kden$y))
    kde.est <- raster::raster(sp::SpatialPixelsDataFrame(kde.est, 
	                          data.frame(kde = as.vector(array(kden$z, 
							  length(kden$z))))))							
       sp::proj4string(kde.est) <- sp::proj4string(x)  
  return( list( kde=kde.est, bandwidth=bw) )
}
