#' @title zonal.stats
#' @description Polygon zonal statistics of a raster 
#'
#' @param x  Polygon object of class SpatialPolygonsDataFrame 
#' @param y  Raster object of class raster
#' @param stat  Statistic or function
#' @param trace  Should progress counter be displayed
#' @param plot  Should subset polygons/rasters be plotted (TRUE/FALSE)
#'
#' @return 
#' Vector, length of nrow(x), of function results
#'
#' @note
#' This function iterates through a polygon object, masks the raster to each subset polygon and then coerces the subset raster to a vector object. The resulting vector is then passed to the specified statistic/function. This is much slower than zonal functions available in GIS software but has the notable advantage of being able to accept any custom function, passed to the 'stat' argument, appropriate for a vector object (see example).    
#'
#' @note Depends: sp, raster
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples                                                                       
#' # skewness function
#' skew <- function(x, na.rm=FALSE) { 
#'    if (na.rm) 
#'        x <- x[!is.na(x)]
#'   sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
#'   }   
#' 
#' # percent x >= p function
#' pct <- function(x, p=0.30) {
#'   if ( length(x[x >= p]) < 1 )  return(0) 
#'     if ( length(x[x >= p]) == length(x) ) return(1) 
#'      else return( length(x[x >= p]) / length(x) ) 
#' }
#' 
#' # create some example data
#' library(raster)
#' library(sp)   
#' p <- raster(nrow=10, ncol=10)
#'   p[] <- runif(ncell(p)) * 10
#'     p <- rasterToPolygons(p, fun=function(x){x > 9})
#'       r <- raster(nrow=100, ncol=100)
#'         r[] <- runif(ncell(r)) 
#' plot(r)
#'   plot(p, add=TRUE, lwd=4) 
#' 
#' # run zonal statistics using skew and pct functions   
#' z.skew <- zonal.stats(x=p, y=r, stat=skew, trace=TRUE, plot=TRUE) 
#' z.pct <- zonal.stats(x=p, y=r, stat=pct, trace=TRUE, plot=TRUE)
#'   ( z <- data.frame(ID=as.numeric(as.character(row.names(p@@data))), 
#'                     SKEW=z.skew, PCT=z.pct) )  
#'
#' @export
zonal.stats <- function(x, y, stat, trace = TRUE, plot = TRUE) {
    if (class(y) != "RasterLayer") 
        stop("y must be a raster object")
    if (class(x) != "SpatialPolygonsDataFrame") 
        stop("x must be a SpatialPolygonsDataFrame object")
    results <- vector()
    for (j in 1:nrow(x)) {
        if (trace == TRUE) {
            cat("processing", j, "of", nrow(x), "\n")
        }
        lsub <- x[j, ]
        cr <- raster::crop(y, raster::extent(lsub), snap = "out")
        crop.NA <- raster::setValues(cr, NA)
        fr <- raster::rasterize(lsub, cr)
        r <- raster::mask(x = cr, mask = fr)
        if (plot == TRUE) {
          plot(r, main = paste("Polygon: ", j, sep = " "))
        }
        r <- raster::values(r)
        r <- stats::na.omit(r)
        if (length(r) < 1) {
            results <- append(results, NA)
        } else {
            results <- append(results, stat(r))
        }
    }
    results
} 
