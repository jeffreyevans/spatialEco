#' @title Raster correlation
#' @description Performs a moving window correlation between 
#'              two rasters
#'
#' @param x          A terra SpatRaster class object for x
#' @param y          A terra SpatRasterclass object for y
#' @param s          Scale of window. Can be a single value, two 
#'                   values for uneven window or a custom matrix. 
#'                   Must be odd number (eg., s=3, for 3x3 window or 
#'                   s=c(3,5) for 3 x 5 window)
#' @param type       Type of output, options are: "pearson", "spearman", 
#'                   "covariance"  
#'
#' @return A terra SpatRaster class object
#' 
#' @note The NA behavior is set to na.rm = TRUE to make default outputs
#'       consistent between the terra and raster packages. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples 
#' \donttest{
#' library(terra)
#'  
#' r <- rast(system.file("ex/logo.tif", package="terra"))  
#'   x <- r[[1]]
#'   y <- r[[3]]
#'   
#'  r.cor <- rasterCorrelation(x, y, s = 5, type = "spearman")
#'    plot(r.cor)
#' }
#' 
#' @export rasterCorrelation
rasterCorrelation <- function(x, y, s = 3, type = "pearson") {
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if (!inherits(y, "SpatRaster")) 
    stop(deparse(substitute(y)), " must be a terra SpatRaster object")
  if(!inherits(s, "matrix")) {  
    if(length(s) > 2) 
	  stop( "Specified window exceeds 2 dimensions")   
    if(length(s) == 1) { s = rep(s, 2) }
      s <- matrix(1,s[1],s[2])
  }
  tmpXY = x * y
    xBar <- terra::focal(x, w = s, fun = mean)
      yBar <- terra::focal(y, w = s, fun = mean)
        xyBar <- terra::focal(tmpXY, w = s, fun = mean)
  if(type == "covariance") {
    return( xyBar - (xBar * yBar) )
  } else if (type == "pearson") {
	  coVar <- xyBar - (xBar * yBar) 
      xStd <- terra::focal(x, w = s, fun=stats::sd)
      yStd <- terra::focal(y, w = s, fun=stats::sd)
      xyStd <-  xStd * yStd
      return( coVar / xyStd )
  } else if (type == "spearman") {
      coVar <- xyBar - (xBar * yBar)
      xStd <- terra::focal(x, w = s, fun=stats::sd)
      yStd <- terra::focal(y, w = s, fun=stats::sd)	
      return( coVar / (xStd * yStd) )
  } else {
	stop( "Only pearson, spearman correlation or covariance are supported")
  }
}
