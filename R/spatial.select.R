#' @title Spatial Select
#'
#' @description Performs a spatial select (feature subset) between a polygon(s) and other feature class
#' 
#' @param x           An sp or sf polygon(s) object that defines the spatial query
#' @param y           A sp or sf feature class that will be subset by the query of x
#' @param distance    A proximity distance of features to select (within distance)
#' @param predicate   Spatial predicate for intersection
#' @param neighbors   If predicate = "contingency" type of neighbors options are c("queen", "rook")
#'
#' @return An sp object representing a subset of y based on the spatial query of x or, 
#'         if predicate = contingency a sparse matrix representing neighbor indexes
#'
#' @note Performs a spatial select of features based on an overlay of a polygon (x),   
#'       which can represent multiple features, and a polygon, point or line feature 
#'       classes (y). User can specify a partial or complete intersection, using within 
#'       argument, or within a distance, using distance argument, predicated on the  
#'       query polygon. This function is similar to ArcGIS/Pro spatial select. Please note
#'       that for point to point neighbor selections use the knn function. 
#'
#' @note Valid spatial predicates include: intersect, touches, covers, contains, proximity and 
#'       contingency. See [DE-9IM topology model](https://en.wikipedia.org/wiki/DE-9IM) for 
#'       detailed information on data predicates. 
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
#' sub.int <- spatial.select(p, spolys, predicate = "intersect")
#' sub.contains <- spatial.select(p, spolys, predicate = "contains")
#' sub.cov <- spatial.select(p, spolys, predicate = "covers")
#' sub.touches <- spatial.select(p, spolys, predicate = "touches")
#' sub.prox <- spatial.select(p, spolys, distance=100, predicate = "proximity")
#'
#' par(mfrow=c(2,3))
#'   plot(spolys, main="all data")
#'     plot(p, add=TRUE)  
#'   plot(sub.int, main="intersects")
#'     plot(p, add=TRUE) 
#'   plot(sub.contains, main="contains")
#'     plot(p, add=TRUE) 
#'   plot(sub.cov, main="covers")
#'     plot(p, add=TRUE) 	 
#'   plot(sub.touches, main="touches")
#'     plot(p, add=TRUE) 	
#'   plot(sub.prox, main="Proximity 100m distance")
#'     plot(p, add=TRUE) 
#' 
#' #### On points 
#' #### note; touches is not relevant for points and intersect/contains/covers 
#' ####       yield the same results  
#' sub.int <- spatial.select(p, meuse, predicate = "intersect")
#' sub.contains <- spatial.select(p, meuse, predicate = "contains")
#' sub.prox <- spatial.select(p, meuse, distance=200, predicate = "proximity")
#'
#' par(mfrow=c(2,2))
#'   plot(meuse, main="all data", pch=20)
#'     plot(p, add=TRUE)  
#'   plot(sub.int, main="intersects", pch=20)
#'     plot(p, add=TRUE) 
#'   plot(sub.contains, main="contains", pch=20)
#'     plot(p, add=TRUE) 
#'   plot(sub.prox, main="Proximity 200m distance", pch=20)
#'     plot(p, add=TRUE)
#'
#' #### For rook or queen polygon contingency 	
#' spolys <- as(sf::st_make_grid(sf::st_sfc(sf::st_point(c(0,0)), 
#'              sf::st_point(c(3,3))), n = c(3,3)), "Spatial")
#' 
#' spatial.select(spolys, predicate = "contingency")
#' spatial.select(spolys, predicate = "contingency", neighbors = "rook") 
#'
#' @seealso \code{\link[rgeos]{gIntersects}} for details on intersect predicate
#' @seealso \code{\link[rgeos]{gContains}} for details on contain predicate
#' @seealso \code{\link[rgeos]{gCovers}} for details on covers predicate
#' @seealso \code{\link[rgeos]{gTouches}} for details on touches predicate 
#' @seealso \code{\link[rgeos]{gWithinDistance}} for details on proximity predicate
#' @seealso \url{https://en.wikipedia.org/wiki/DE-9IM} for details on DE-9IM topology model
#'
#' @export spatial.select	
spatial.select <- function(x, y = NULL, distance = NULL, predicate = c("intersect",   
                           "contains", "covers", "touches", "proximity",
						   "contingency"), neighbors = c("queen", "rook")) {
  predicate = predicate[1]
  if(!is.null(y)) {
  if(any(methods::is(x, "Spatial"), methods::is(y, "Spatial"))) {
    type="dsp"
    if(!any(class(x) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) 
      stop("x must be a sp polygon object")
    if(!methods::is(y, "Spatial"))
      stop("y must be a sp polygon, point or line object")
  } else if(methods::is(x, "sf") | methods::is(y, "sf")) {
    type="dsf"
    if(attributes(x$geometry)$class[1] != "sfc_POLYGON")
	  stop("x must be a sf sfc_POLYGON object")
    if(!methods::is(y, "sf"))
      stop("y must be a sf polygon, point or line object")   
  } else {
    stop("x and y need to be sp or sf spatial objects")
  }	
  } else {
    if(!any(methods::is(x, "Spatial")))
	  stop("x must be a spatial object")
	if(!any(class(x) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) 
      stop("x must be a sp polygon object")  
	if(predicate != "contingency")
      stop("The only predicate that supports self realization is contingency")	
    if(!any(class(x) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) 
      stop("x must be a sp polygon object")	  
  }
  if(predicate == "intersect") {
    if(type == "dsp") {
 	  idx <- rgeos::gIntersects(x, y, byid=TRUE)
	    idx <- unique(which(idx == TRUE, arr.ind = TRUE)[,1])
	} else if(type == "dsf") { 
      idx <- sort(unique(unlist(sf::st_intersects(x,y))))	   
	}
  } else if(predicate == "contains") {
    if(type == "dsp") {
	  idx <- rgeos::gContains(x, y, byid=TRUE)
	    idx <- unique(which(idx == TRUE, arr.ind = TRUE)[,1])
    } else if(type == "dsf") { 
      idx <- sort(unique(unlist(sf::st_contains(x,y))))		 
	}
  } else if(predicate == "covers") {
    if(type == "dsp") {  
	  idx <- rgeos::gCovers(x, y, byid=TRUE)
	    idx <- unique(which(idx == TRUE, arr.ind = TRUE)[,1])
    } else if(type == "dsf") {
      idx <- sort(unique(unlist(sf::st_covers(x,y))))		
	}
  } else if(predicate == "touches") {
    if(type == "dsp") {   
	  idx <- rgeos::gTouches(x, y, byid=TRUE)
	    idx <- unique(which(idx == TRUE, arr.ind = TRUE)[,1])
    } else if(type == "dsf") {
      idx <- sort(unique(unlist(sf::st_touches(x,y))))	
	} 
  } else if(predicate == "contingency") {
    if(neighbors[1] == "rook"){ adj = "F***1****"} else {adj = "F***T****"}
      cont <- function(a, b = a, adj) sf::st_relate(a, b, pattern = adj)
	    y <- cont(as(x, "sf"), adj = adj) 
          return(y)
  } else if(predicate == "proximity") {
    if(is.null(distance)) 
	  stop("For proximity predicate distance must be defined")
    if(type == "dsp") {   	  
      idx <- rgeos::gWithinDistance(x, y, dist=distance, 
                                    byid=TRUE, densifyFrac=1)
	    idx <- unique(which(idx == TRUE, arr.ind = TRUE)[,1])
    } else if(type == "dsf") {
      idx <- sort(unique(unlist(sf::st_is_within_distance(x,y, dist = distance))))	 
	}
  } else {
    stop("Not a valid spatial relationship predicate")
  }
    if(length(idx) < 1)
      stop(paste0("No results match ", predicate,  " predicate"))	
  return(y[idx,])
}
