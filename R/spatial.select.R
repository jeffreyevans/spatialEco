#' @title Spatial Select
#'
#' @description 
#' Performs a spatial select (feature subset) between a polygon(s) and other 
#' feature class
#' 
#' @param x           An sp or sf polygon(s) object that defines the spatial query
#' @param y           A sp or sf feature class that will be subset by the query of x
#' @param distance    A proximity distance of features to select (within distance)
#' @param predicate   Spatial predicate for intersection
#' @param neighbors   If predicate = "contingency" type of neighbors options are 
#'                    c("queen", "rook")
#'
#' @return 
#' An sf object representing a subset of y based on the spatial query of x or, 
#' if predicate = contingency a sparse matrix representing neighbor indexes
#'
#' @note
#' Performs a spatial select of features based on an overlay of a polygon (x),   
#' which can represent multiple features, and a polygon, point or line feature 
#' classes (y). User can specify a partial or complete intersection, using within 
#' argument, or within a distance, using distance argument, predicated on the  
#' query polygon. This function is similar to ArcGIS/Pro spatial select. Please note
#' that for point to point neighbor selections use the knn function. 
#' Valid spatial predicates include: intersect, touches, covers, contains, proximity 
#' and contingency. See DE-9IM topology model for detailed information on data predicates. 
#' @details
#' * intersection  Create a spatial intersection between two features
#' * intersect     Boolean evaluation of features intersecting   
#' * contains      Boolean evaluation of x containing y
#' * covers        Boolean evaluation of x covering y
#' * touches       Boolean evaluation of x touching y
#' * proximity     Evaluation of distance-based proximity of x to y (x and y can be the same)
#' * contingency   Evaluation of polygon contingency (eg., 1st, 2nd order)
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'                                                                    
#' @examples 
#' library(sf)
#'  if(require(sp, quietly = TRUE)) {
#'    data(meuse, package = "sp")
#'    meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                      agr = "constant")
#' 
#' spolys <- hexagons(meuse, res=100)
#'   spolys$ID <- 1:nrow(spolys)
#'     p <- st_as_sf(st_sample(spolys, 500))
#' 	  p$PTID <- 1:nrow(p) 
#' 	  sf::st_geometry(p) <- "geometry"
#' 
#'   plot(st_geometry(spolys), main="all data")
#'     plot(st_geometry(p), pch=20, add=TRUE)
#' 	
#' sub.int <- spatial.select(p, spolys, predicate = "intersect")
#'   plot(st_geometry(sub.int), main="intersects")
#'     plot(st_geometry(p), pch=20, add=TRUE)	
#' 
#' sub.prox <- spatial.select(p, spolys, distance=100, predicate = "proximity")	
#'   plot(st_geometry(sub.int), main="intersects")
#'     plot(st_geometry(p), pch=20, add=TRUE)
#' 
#' # For rook or queen polygon contingency 	
#' plot( spolys <- sf::st_make_grid(sf::st_sfc(sf::st_point(c(0,0)), 
#'                  sf::st_point(c(3,3))), n = c(3,3)) )
#'   
#' spatial.select(x=spolys, predicate = "contingency")
#' spatial.select(spolys, predicate = "contingency", neighbors = "rook") 
#' 
#' }
#' @seealso \code{\link[sf]{st_intersection}} for details on intersection predicate
#' @seealso \code{\link[sf]{st_intersects}} for details on intersect predicate
#' @seealso \code{\link[sf]{st_contains}} for details on contain predicate
#' @seealso \code{\link[sf]{st_covers}} for details on covers predicate
#' @seealso \code{\link[sf]{st_touches}} for details on touches predicate 
#' @seealso \code{\link[sf]{st_is_within_distance}} for details on proximity predicate
#' @seealso \url{https://en.wikipedia.org/wiki/DE-9IM} for details on DE-9IM topology model
#'
#' @export spatial.select	
spatial.select <- function(x, y = NULL, distance = NULL, 
                           predicate = c("intersection", "intersect",   
                           "contains", "covers", "touches", "proximity",
						   "contingency"), neighbors = c("queen", "rook")) {
  predicate = predicate[1]
  gtypes = c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT", 
             "LINESTRING", "MULTILINESTRING")
  if(!is.null(y)) {
    if(any(methods::is(y, "Spatial"))){
      y <- sf::st_as_sf(y)
	  message("coercing y to sf")
	}  
    if(!inherits(y, c("sf", "sfc")))
      stop(deparse(substitute(y)), " must be an sf object or coercible")	  
    if(!any(unique(as.character(st_geometry_type(y))) != gtypes))
      stop(deparse(substitute(y)), " must be one of ", 
	       paste(gtypes, collopse=""))		     
  } else {
    if(predicate != "contingency")
      stop("The only predicate that supports self realization is contingency")
  }
  if(any(methods::is(x, "Spatial"))){
    x <- sf::st_as_sf(x)
	message("coercing y to sf")
  }	
  if(!inherits(x, c("sf", "sfc")))
    stop(deparse(substitute(x)), " must be an sf object or coercible")
  if(!any(unique(as.character(st_geometry_type(x))) != gtypes))
    stop(deparse(substitute(c)), " must be one of ", 
	     paste(gtypes[1:2], collopse=""))
		 
  if(predicate == "intersection") {
    y <- suppressWarnings(sf::st_intersection(x,y))
	  return(y)
  } else if(predicate == "intersect") {
    idx <- suppressWarnings(sort(unique(unlist(sf::st_intersects(x,y)))))	   
  } else if(predicate == "contains") {
    idx <- suppressWarnings(sort(unique(unlist(sf::st_contains(x,y)))))		 
  } else if(predicate == "covers") {
    idx <- suppressWarnings(sort(unique(unlist(sf::st_covers(x,y)))))		
  } else if(predicate == "touches") {
    idx <- suppressWarnings(sort(unique(unlist(sf::st_touches(x,y)))))	
  } else if(predicate == "contingency") {
    if(neighbors[1] == "rook"){ adj = "F***1****"} else {adj = "F***T****"}
          cont <- function(a, b = a, adj) sf::st_relate(a, b, pattern = adj)
  	    y <- cont(x, adj = adj) 
      return(y)
  } else if(predicate == "proximity") {
    if(is.null(distance)) 
      stop("For proximity predicate distance must be defined")
        idx <- sort(unique(unlist(sf::st_is_within_distance(x, y, dist = distance))))	 
  } else {
    stop("Not a valid spatial relationship predicate")
  }
  if(length(idx) < 1)
    stop(paste0("No results match ", predicate,  " predicate"))	
  return(y[idx,])
}
