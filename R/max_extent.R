#' @title Maximum extent of multiple rasters
#' @description returns a extent polygon representing maximum extent of
#'              input rasters
#'
#' @param x    raster class object
#' @param ...  additional raster class objects
#'
#' @return a SpatialPolygons sp class object
#'
#' @note 
#' Creates a maximum extent polygon of all specified rasters
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(raster)
#' 
#' r1 <- raster(extent(61.87125, 76.64458, 23.90153, 37.27042))
#' r2 <- raster(extent(67.66625, 81.56847, 20.38458, 35.67347))
#' r3 <- raster(extent(72.64792,84.38125,5.91125,28.13347 ))
#' 
#' ( e <- max_extent(r1, r2, r3) )
#' plot(e)
#'   plot(as(extent(r1),"SpatialPolygons"),col="red",add=TRUE)
#'   plot(as(extent(r2),"SpatialPolygons"),col="red",add=TRUE)
#'   plot(as(extent(r3),"SpatialPolygons"),col="red",add=TRUE)
#'
#' @export max_extent
max_extent <- function(x, ...) {
  if(length(list(...))){
    dots <- list(...)
  } else {
    dots <- list()
  }
  dots[[length(dots)+1]] <- x
  e <- list() 
    for(i in 1:length(dots)){ e[[i]] <- raster::extent(dots[[i]]) }
  if(length(e) < 2){
    p <- as(e[[1]], "SpatialPolygons")
  } else{
    p <- as(do.call(raster::merge, e), "SpatialPolygons")	
  }
  return(p)
}
