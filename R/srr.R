#' @title Surface Relief Ratio
#' @description Calculates the Pike (1971) Surface Relief Ratio
#' 
#' @param x    raster object
#' @param s    Focal window size
#' @param ...  Additional arguments passed to raster::calc
#' 
#' @return raster class object of Pike's (1971) Surface Relief Ratio
#'
#' @note
#' Describes rugosity in continuous raster surface within a specified window. The implementation of SRR can be shown as: 
#' (mean(x) - min(x)) / (max(x) - min(x)) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples 
#'   library(raster)
#'   data(elev)
#'   r.srr <- srr(elev, s=5)
#'     plot(r.srr, main="Surface Relief Ratio") 
#'     
#' @export
srr <- function(x, s = 5, ...) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
    if( length(s) == 1) s = c(s[1],s[1])
       m <- matrix(1, nrow=s, ncol=s)
    rmin <- raster::focal(x, w=m, fun=min)
    rmax <- raster::focal(x, w=m, fun=max)
	rmean <- raster::focal(x, w=m, fun=mean)
    return( raster::calc(raster::stack(rmean,rmin,rmax), fun=function(x) { (x[1] - x[2] ) / ( x[3] - x[2] ) }, ... ) )
} 
