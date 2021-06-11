#' @title Raster correlation
#' @description Performs a simple moving window correlation 
#'              between two rasters
#'
#' @param x          raster or terra class object for x
#' @param y          raster or terra class object for y
#' @param s          Scale of window. Can be a single value, two 
#'                   values for uneven window or a custom matrix. 
#'                   Must be odd number (eg., s=3, for 3x3 window or 
#'                   s=c(3,5) for 3 x 5 window)
#' @param type       Type of output, options are: "pearson", "spearman", 
#"                   "covariance"  
#' @param file.name  Name of output raster (optional)
#' @param ...        Additional arguments passed to writeRaster
#'
#' @return raster or terra class object or raster written to disk
#' 
#' @note The NA behavior is set to na.rm = TRUE to make default outputs
#'       consistent between the terra and raster packages. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples 
#' \donttest{
#'  library(terra)                                                                    
#'  s <- rast(system.file("ex/logo.tif", package="terra"))
#'    x <- s[[1]]
#'    y <- s[[3]]
#'  r.cor <- rasterCorrelation(x, y, s = 5, type = "spearman")
#'  plot(r.cor)
#' }
#' 
#' @export rasterCorrelation
rasterCorrelation <- function(x, y, s = 3, type = "pearson", file.name = NULL, ...) {
  if (!any(class(x) == c("SpatRaster", "RasterLayer"))) 
      stop("x must be RasterLayer or  SpatRaster class")
  if (!any(class(y) == c("SpatRaster", "RasterLayer"))) 
      stop("y must be RasterLayer or  SpatRaster class")
  if (class(y) != class(y)) 
    stop("x and y must be the same raster class")
  if(!inherits(s, "matrix")) {  
   if(length(s) > 2) stop( "Specified window exceeds 2 dimensions")   
   if(length(s) == 1) { s = rep(s, 2) }
    s <- matrix(1,s[1],s[2])
  }
  rclass <- ifelse(class(x) == "RasterLayer", "raster", "terra")     
  tmpXY = x * y
    if(rclass == "raster") {
      xBar <- raster::focal(x, w = s, na.rm = TRUE, fun = mean)
      yBar <- raster::focal(y, w = s, na.rm = TRUE, fun = mean)
      xyBar <- raster::focal(tmpXY, w = s, na.rm = TRUE, fun = mean) 
    } else if(rclass == "terra") {
      xBar <- terra::focal(x, w = s, fun = mean)
      yBar <- terra::focal(y, w = s, fun = mean)
      xyBar <- terra::focal(tmpXY, w = s, fun = mean)
    }
if(type == "covariance") {
  if(rclass == "raster") {  
    if(!is.null(file.name)) {  
      return( raster::writeRaster( (xyBar - (xBar * yBar)), filename = file.name, ...) )
	    } else {
      return( xyBar - (xBar * yBar) )
      }	
  } else if(rclass == "terra") {
    if(!is.null(file.name)) {  
      return( terra::writeRaster( (xyBar - (xBar * yBar)), filename = file.name, ...) )
	    } else {
      return( xyBar - (xBar * yBar) )
      }	
    }		 
} else if (type == "pearson") {
    if(rclass == "raster") {  
	  coVar <- xyBar - (xBar * yBar) 
      xStd <- raster::focal(x, w = s, na.rm = TRUE, fun=stats::sd)
      yStd <- raster::focal(y, w = s, na.rm = TRUE, fun=stats::sd)
      xyStd <-  xStd * yStd
    if(!is.null(file.name)) {  
      return( raster::writeRaster( (coVar / xyStd), filename = file.name, ...) )
	    } else {
      return( coVar / xyStd )
      }	
  } else if(rclass == "terra") {
	  coVar <- xyBar - (xBar * yBar) 
      xStd <- terra::focal(x, w = s, fun=stats::sd)
      yStd <- terra::focal(y, w = s, fun=stats::sd)
      xyStd <-  xStd * yStd
    if(!is.null(file.name)) {  
      return( terra::writeRaster( (coVar / xyStd), filename = file.name, ...) )
	    } else {
      return( coVar / xyStd )
      }	  
  }	  
} else if (type == "spearman") {
    if(rclass == "raster") { 
      coVar <- xyBar - (xBar * yBar)
      xStd <- raster::focal(x, w = s, na.rm = TRUE, fun=stats::sd)
      yStd <- raster::focal(y, w = s, na.rm = TRUE, fun=stats::sd)	
    if(!is.null(file.name)) {  
      return( raster::writeRaster( (coVar / (xStd * yStd)), filename = file.name, ...) )
	    } else {
      return( coVar / (xStd * yStd) )
      }	
    } else if(rclass == "terra") {
      coVar <- xyBar - (xBar * yBar)
      xStd <- terra::focal(x, w = s, fun=stats::sd)
      yStd <- terra::focal(y, w = s, fun=stats::sd)	
    if(!is.null(file.name)) {  
      return( terra::writeRaster( (coVar / (xStd * yStd)), filename = file.name, ...) )
	    } else {
      return( coVar / (xStd * yStd) )
      }	
    }
  } else {
	stop( "Only pearson, spearman or covariance are supported")
  }
}
