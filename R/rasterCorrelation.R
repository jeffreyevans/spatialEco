#' @title Raster correlation
#' @description Performs a simple moving window correlation between two rasters
#'
#' @param x              raster class object for x
#' @param y              raster class object for y
#' @param s              Scale of window. Can be a single value, two values for uneven window or a custom matrix. Must be odd number (eg., s=3, for 3x3 window or s=c(3,5) for 3 x 5 window)
#' @param type           Type of output, options are: "pearson", "spearman", "covariance"  
#' @param file.name      Name of output raster (optional)
#' @param ...            Additional arguments passed to writeRaster
#'
#' @return raster class object or raster written to disk
#' 
#' @note Depends: raster
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples 
#'  library(raster)                                                                    
#'  b <- brick(system.file("external/rlogo.grd", package="raster"))
#'  x <- b[[1]]
#'  y <- b[[3]]
#'  r.cor <- rasterCorrelation(x, y, s = 5, type = "spearman")
#'  plot(r.cor)
#'
#' @export
rasterCorrelation <- function(x, y, s = 3, type = "pearson", file.name = NULL, ...) {
  if (!inherits(x, "RasterLayer")) 
      stop("Input must be RasterLayer objects")
  if (!inherits(y, "RasterLayer")) 
      stop("Input must be RasterLayer objects")	  
  if(!inherits(s, "matrix")) {  
   if(length(s) > 2) stop( "Specified window exceeds 2 dimensions")   
   if(length(s) == 1) { s = rep(s,2) }
    s <- matrix(1,s[1],s[2])
  } 
  tmpXY = x * y
  xBar <- raster::focal(x, w = s, fun = mean)
  yBar <- raster::focal(y, w = s, fun = mean)
  xyBar <- raster::focal(tmpXY, w = s, fun = mean) 
  if(type == "covariance") {  
    if(!is.null(file.name)) {  
      return( raster::writeRaster( (xyBar - (xBar * yBar)), filename = file.name, ...) )
	    } else {
      return( xyBar - (xBar * yBar) )
      }	
    } else if (type == "pearson") {
      # Correlation
	  coVar <- xyBar - (xBar * yBar) 
      xStd <- raster::focal(x, w = s, fun=stats::sd)
      yStd <- raster::focal(y, w = s, fun=stats::sd)
      xyStd <-  xStd * yStd
    if(!is.null(file.name)) {  
      return( raster::writeRaster( (coVar / xyStd), filename = file.name, ...) )
	    } else {
      return( coVar / xyStd )
      }		    
    } else if (type == "spearman") {
      coVar <- xyBar - (xBar * yBar)
      xStd <- raster::focal(x, w = s, fun=stats::sd)
      yStd <- raster::focal(y, w = s, fun=stats::sd)	
    if(!is.null(file.name)) {  
      return( raster::writeRaster( (coVar / (xStd * yStd)), filename = file.name, ...) )
	    } else {
      return( coVar / (xStd * yStd) )
      }		  
    } else {
	stop( "Only pearson, spearman or covariance are supported")
    }
}
