#' @title Sample Polygons
#' @description Creates an equal sample of n for each polygon in an sp Polygon class object
#'
#' @param x     sp class SpatialPolygons or SpatialPolygonsDataFrame object 
#' @param n     Number of random samples
#' @param type  Type of sample with options for: "random", "regular", "stratified", "nonaligned", "hexagonal", "clustered", "Fibonacci". See "spsample" for details.
#' @param ...   Additional arguments passed to spsample
#'
#' @return sp SpatialPointsDataFrame object 
#'
#' @note Depends: sp
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#'  library(raster)
#'  library(sp)   
#'    p <- raster(nrow=10, ncol=10)
#'    p[] <- runif(ncell(p)) * 10
#'    p <- rasterToPolygons(p, fun=function(x){x > 9})   
#'    s <- sample.poly(p, n = 5, type = "random")
#'      plot(p)
#'        plot(s, pch = 20, add = TRUE)
#'        box()
#'        title("Random sample (n=5) for each polygon")	   
#'
#' @export
sample.poly <- function(x, n = 10, type = "random", ...) {
  if(!(class(x) == "SpatialPolygonsDataFrame" | class(x) == "SpatialPolygons"))
    stop(deparse(substitute(x)), " MUST BE A sp spatialDataFrame OBJECT")
  sample.list <- sapply(x@polygons, sp::spsample, n = n, type = type, ...)
  return( do.call(rbind, sample.list) )
} 
