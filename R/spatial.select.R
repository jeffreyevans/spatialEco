#' @title Spatial Select
#'
#' @description Performs a spatial select (feature subset) between two feature classes
#' 
#' @param x         A SpatialPolygonsDataFrame object that defines the spatial query
#' @param y         A sp feature class that will be subset by the query of x
#' @param distance  A proximity distance of features to select (within distance)
#' @param within    Select polygons completely within (TRUE) or partial within (FALSE) 
#'
#' @return An sp object representing a subset of y based on the spatial query of x
#'
#' @note Performs a spatial select of features based on an overlay of a polygon (x),   
#'       which can represent multiple features, and a polygon, point or line feature 
#'       classes (y). User can specify a partial or complete intersection, using within 
#'       argument, or within a distance, using distance argument, in relation to the  
#'       query polygon. This function is similar to ArcGIS spatial select. Please note
#'       that for point to point neighbor selections use the knn function. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'                                                                    
#' @examples 
#' library(raster)
#' library(sp)   
#' 
#' data(meuse)
#'   coordinates(meuse) <- ~x+y
#' 
#' spolys <- hexagons(meuse, res=100)
#' p <- raster(extent(spolys), res=800)
#'   p[] <- runif(ncell(p)) * 10
#'     p <- rasterToPolygons(p, fun=function(x){x > 6})
#' 	
#' #### On polygons	
#' # Intersecting but not contained within
#' sub.int <- spatial.select(p, spolys, within = FALSE)
#' 
#' # Contained within
#' sub.contained <- spatial.select(p, spolys, within = TRUE)
#' 
#' # Within 100m distance
#' sub.dist <- spatial.select(p, spolys, distance=100)
#' 
#' par(mfrow=c(2,2))
#'   plot(spolys, main="all data")
#'     plot(p, add=TRUE) 
#'   plot(sub.int, main="Intersecting but not contained within")
#'     plot(p, add=TRUE) 
#'   plot(sub.contained, main="Contained within")
#'     plot(p, add=TRUE) 	
#'   plot(sub.dist, main="Within 100m distance")
#'     plot(p, add=TRUE) 
#' 
#' #### On points
#' # Intersecting but not contained within
#' sub.int <- spatial.select(p, meuse, within = FALSE)
#' 
#' # Contained within (with points, same as within = FALSE)
#' sub.contained <- spatial.select(p, meuse, within = TRUE)
#' 
#' # Within 100m distance
#' sub.dist <- spatial.select(p, meuse, distance=300)
#' 
#' par(mfrow=c(2,2))
#'   plot(meuse, main="all data", pch=20)
#'     plot(p, add=TRUE) 
#'   plot(sub.int, main="Intersecting but not contained within", pch=20)
#'     plot(p, add=TRUE) 
#'   plot(sub.contained, main="Contained within", pch=20)
#'     plot(p, add=TRUE) 	
#'   plot(sub.dist, main="Within 300m distance", pch=20)
#'     plot(p, add=TRUE) 
#'
#' @seealso \code{\link[rgeos]{gIntersects}} for details on behavior of within = FALSE
#' @seealso \code{\link[rgeos]{gContains}} for details on behavior of within = TRUE
#' @seealso \code{\link[rgeos]{gWithinDistance}} for details on distance intersection
#'
#' @export spatial.select	
spatial.select <- function(x, y, distance = NULL, within = FALSE) {
  if(!any(class(x) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) 
    stop("x must be a sp polygon object")
  if(!length(grep("Spatial", class(y)) >= 1))
    stop("y must be a sp polygon, point or line object")  
  if(!is.null(distance)) {
    idx <- rgeos::gWithinDistance(x, y, dist=distance, 
                             byid=TRUE, densifyFrac=1)
  } else {
    if(!within) {
	  idx <- rgeos::gIntersects(x, y, byid=TRUE)    
    } else {
	  idx <- rgeos::gContains(x, y, byid=TRUE) 
    }
  }
  return(y[unique(which(idx == TRUE, arr.ind = TRUE)[,1]),])
}
