#' @title Raster Distance
#' @description Calculates the Euclidean distance between a set of points and
#'              the cells in a raster. This is a drop-in replacement for the  
#'              raster distanceFromPoints function using the RANN algorithm for 
#'              calculating distance, resulting in a large improvement in   
#'              processing speed.   
#'
#' @param  x          rasterLayer, sp SpatialPoints or sf POINTS object 
#' @param  y          sp SpatialPoints or sf POINTS object 
#' @param  reference  A raster to use as a reference if x is points object 
#' @param  scale      (FALSE/TRUE) Perform a row standardization on results 
#'
#' @return a distance raster of class rasterLayer 
#'
#' @note  
#' This replicates the raster distanceFromPoints function but uses the Arya & Mount
#' Approximate Near Neighbor (ANN) C++ library for calculating distances. Where this
#' results in a notable increase in performance it is not memory safe, needing to read
#' in the entire raster and does not use the GeographicLib (Karney, 2013) spheroid 
#' distance method for geographic data.  
#' 
#' @references 
#' Arya S., Mount D. M., Netanyahu N. S., Silverman R. and Wu A. Y (1998), An 
#'   optimal algorithm for approximate nearest neighbor searching, Journal of 
#'   the ACM, 45, 891-923.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' library(raster)
#' r <- raster(ncol=100,nrow=100)
#'   r[] <- sample(c(0,1), ncell(r), replace = TRUE)
#'  
#' majority <- function(x){
#'  m <- table(x)
#'  names(m)[which.max(m)][1]
#' }
#' r <- focal(r, matrix(1,11,11, byrow=TRUE), majority) 
#'  
#'  pts <- rasterToPoints(r, spatial=TRUE)
#'    cls <- pts[pts$layer == "1",] 
#'  d <- rasterDistance(pts, cls, reference = r, scale=TRUE)
#'    dev.new(height=8,width=11)
#'      plot(d)
#'        points(cls,pch=19,cex=0.5)
#'
#' @seealso \code{\link[raster]{distanceFromPoints}, \link[raster]{distance}}
#'
#' @export rasterDistance 
rasterDistance <- function(x, y, reference = NULL, scale = FALSE){
  if(!is.null(reference)) {
    if(class(reference)[1] != "RasterLayer")
      stop("the reference raster is not a RasterLayer object")
  }
  if(is.null(reference)){
    if(class(x)[1] == "rasterLayer") {
      reference = x
    } else {
      reference = raster::raster(raster::extent(x))
    }
  }
  if(class(y)[1] == "sf") y <- as(y, "Spatial") 
    if(class(x)[1] == "RasterLayer") {
      p <- raster::rasterToPoints(x, spatial = TRUE)
    } else {
      p <- x
    }
  idx <- RANN::nn2(sp::coordinates(y), sp::coordinates(p),  k = 1) 
    if(scale) {    
      idx <- idx$nn.dists[,1] / max(idx$nn.dists[,1]) 
    } else {
      idx <- idx$nn.dists[,1]
    }
      p@data <- data.frame(dist=idx)
    r <- raster::rasterize(p, reference, field = "dist")
  return(r)
}
