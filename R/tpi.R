#' @title Topographic Position Index (tpi)
#' @description Calculates topographic position using mean deviations
#' 
#' @param x              A raster class object
#' @param scale          The focal window size 
#' @param win            Window type. Options are "rectangle" and "circle" 
#' @param normalize      Apply deviation correction that normalizes to local surface roughness  
#' 
#' @return raster class object of tpi 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' De Reu, J., J. Bourgeois, M. Bats, A. Zwertvaegher, V. Gelorini, et al., (2014) Application of the topographic position index to heterogeneous landscapes. Geomorphology, 186:39-49.
#' 
#' @examples 
#'  library(raster)
#'  data(elev)
#'
#' # calculate tpi and plot 
#'   tpi9 <- tpi(elev, scale=9)     
#'     par(mfrow=c(1,2))
#'       plot(elev, main="original raster")
#'       plot(tpi9, main="tpi 9x9")
#'
#' @export
tpi <- function(x, scale = 3, win = "rectangle", normalize = FALSE) {
    if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")   
    if( win == "circle") {
      if( scale < raster::res(x)[1] * 2) 
        stop( "Scale is too small for a circular window")
          m <- raster::focalWeight(x, scale, type=c('circle'))
          m[m > 0] <- 1
        } else { 	  
          m <- matrix(1, nrow=scale, ncol=scale)
    }
      tp <- x - raster::focal(x, w=m, fun=mean)
    if(normalize == TRUE) {  
	  tp.sd <- raster::focal(x, w=m, fun=stats::sd)
	    tp <- tp / tp.sd 
    }
  return(tp)  
}
