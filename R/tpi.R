#' @title Topographic Position Index (tpi)
#' @description Calculates topographic position using mean deviations
#' 
#' @param x              A raster class object
#' @param scale          focal window size (n-cell x n-cell for rectangle or distance for circle) 
#' @param win            Window type. Options are "rectangle" and "circle" 
#' @param normalize      Apply deviation correction that normalizes to local surface roughness 
#' @param zero.correct   Apply correction for zero values in matrix weights      
#' 
#' @return raster class object of tpi 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' De Reu, J., J. Bourgeois, M. Bats, A. Zwertvaegher, V. Gelorini, et al., (2014) Application 
#'   of the topographic position index to heterogeneous landscapes. Geomorphology, 186:39-49.
#' 
#' @examples 
#'  library(raster)
#'  data(elev)
#'
#' # calculate tpi and plot 
#'   tpi7 <- tpi(elev, scale=7) 
#'   tpi025 <- tpi(elev, win = "circle", scale=0.025)
#'   tpi025.zc <- tpi(elev, win = "circle", scale=0.025, 
#'                    zero.correct = TRUE)
#'     par(mfrow=c(2,2))
#'       plot(elev, main="original raster")
#'       plot(tpi7, main="tpi 7x7")
#'       plot(tpi025, main="tpi Circular window d=0.025")
#'	   plot(tpi025, main="tpi Circular window d=0.025, zero correct")
#'
#' @export
tpi <- function(x, scale = 3, win = "rectangle", normalize = FALSE, 
                zero.correct = FALSE) {
    if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
    if( win == "circle") {
      if( scale < raster::res(x)[1] * 2) 
        stop( "Scale is too small for a circular window")
        m <- raster::focalWeight(x, scale, type="circle")
          m[m > 0] <- 1
      } else {
	     if(scale %% 2 == 0)
           stop("Scale for a rectangular window must be an odd number") 	  
         m <- matrix(1, nrow=scale, ncol=scale)
      }
	if( zero.correct ) {
	  tp <- x - raster::focal(x, w=m, fun = function(x, ...){ sum(x)/sum(m) })
    } else {	  
      tp <- x - raster::focal(x, w = m, fun = mean)
    }  
    if(normalize == TRUE) {
      if( zero.correct ) {
        tp.sd <- raster::focal(x, w=m, fun=function(x, ...)
			       { sqrt(sum(x-sum(x)/sum(m))^2/(sum(m)-1))})
      } else {	  
	    tp.sd <- raster::focal(x, w=m, fun=stats::sd)
	  }
	    tp <- tp / tp.sd 
    }
  return(tp)  
}
