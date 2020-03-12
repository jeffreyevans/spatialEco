#' @title Point and Polygon Intersect
#' @description Intersects point and polygon feature classes and adds polygon attributes to points
#'
#' @param x         sp SpatialPointsDataFrame or SpatialPoints or sf point object
#' @param y         sp SpatialPolygonsDataFrame or sf polygon object
#' @param sp        (TRUE/FALSE) Return an sp class object, else returns sf class object
#' @param duplicate (TRUE/FALSE) Return duplicated features with more than one polygon intersection
#' @param ...       Additional arguments passed to sf::st_join  
#'
#' @return A SpatialPointsDataFrame or sf 
#'
#' @note 
#' If duplicate argument is TRUE and more than one polygon intersection occurs, points will be duplicated (new row added) and all attributes joined.  
#' However, if duplicate is FALSE, with duplicate intersections, a new column for each unique intersecting polygon will 
#' be returned and the points will not be duplicated. For example, if a point intersect three polygons, three new columns will be added
#' representing the polygons ID.    
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#'  #### Simple one-to-one feature overlay. 
#'  require(sp)
#'    data(meuse)
#'    coordinates(meuse) = ~x+y
#'    meuse@data$test.na <- NA
#'  
#'    sr1=Polygons(list(Polygon(cbind(c(180114, 180553, 181127, 181477, 181294,  
#'      181007, 180409, 180162, 180114), c(332349, 332057, 332342, 333250, 333558,  
#'      333676, 332618, 332413, 332349)))),'10')
#'    sr2=Polygons(list(Polygon(cbind(c(180042, 180545, 180553, 180314, 179955, 179142,  
#'      179437, 179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683, 
#'      331133, 331623, 332152, 332357, 332373)))),'20')
#'    sr3=Polygons(list(Polygon(cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 
#'      179875, 179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
#'      c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004, 
#'      329783, 329665, 329720, 329933, 330478, 331062, 331086)))),'30')
#'    sr4=Polygons(list(Polygon(cbind(c(180304, 180403,179632,179420,180304),
#'      c(332791, 333204, 333635, 333058, 332791)))),'40')
#'    sr=SpatialPolygons(list(sr1,sr2,sr3,sr4))
#'    polys=SpatialPolygonsDataFrame(sr, data.frame(row.names=c('10','20','30','40'), 
#'                                   PIDS=1:4, y=runif(4)))
#'    polys@data$pid <- polys@data$PIDS + 100 
#'  
#'  plot(polys)
#'    plot(meuse, pch=19, add=TRUE)  
#'    
#'  # Point in polygon overlay  
#'  pts.poly <-  point.in.poly(meuse, polys)
#'    head(pts.poly@data)
#'  
#'  # Count points in each polygon
#'  tapply(pts.poly$cadmium, pts.poly$pid, FUN=length)    
#'    
#'  #### Complex many-to-one feature overlay. 
#'  require(sf)
#'  p <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#'  polys <- sf::st_sf(sf::st_sfc(p, p + c(.8, .2), p + c(.2, .8)))
#'  pts <- sf::st_sf(sf::st_sample(polys, size=100))
#'  
#'  # Duplicates points for each new polygon, no attributes so returns IDs for features    
#'  pts.poly.dup <-  point.in.poly(pts, polys)
#'    head(pts.poly.dup@data)
#'    
#' \dontrun{
#'  # **** Should throw error due to lack of attributes ****  
#'    pts.poly <- point.in.poly(pts, polys, duplicate = FALSE) 
#' }
#'  
#'  # Coerce to sp class objects
#'  x <- as(pts, "Spatial")
#'    x <- SpatialPointsDataFrame(x, data.frame(IDS=1:nrow(x), pty=runif(nrow(x))))
#'  y <- as(polys, "Spatial")
#'    y <- SpatialPolygonsDataFrame(y, data.frame(IDS=1:nrow(y), py=runif(nrow(y))))
#'  
#'  # Returns point attributes with column for each unique polygon   
#'  pts.poly <- point.in.poly(x, y, duplicate = FALSE)
#'    head(pts.poly@data)  
#'    
#'  # Duplicates points for each new polygon, joins all attributes  
#'  pts.poly.dup <-  point.in.poly(x, y)
#'    head(pts.poly.dup@data)  
#'  
#'  # Count points in each polygon
#'  tapply(pts.poly.dup$IDS.x, pts.poly.dup$IDS.y, FUN=length)      
#'
#' @import sf
#' @export
point.in.poly <- function(x, y, sp = TRUE, duplicate = TRUE, ...) {
  if(!any(class(x)[1] == c("SpatialPoints", "SpatialPointsDataFrame", "sf"))) {
    stop("x is not a suitable point feature object class") }
  if(!any(class(y)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame", "sf"))) {
    stop("y is not a suitable polygon feature object class") }
  if(any(class(x) == "sfc")) { x <- sf::st_sf(x) }  
	if(duplicate == FALSE) {
	  if(!any(class(x)[1] == c("SpatialPoints", "SpatialPointsDataFrame"))) {
	    x <- methods::as(x, "Spatial")
		  if(dim(x@data)[2] == 0) stop("There are no attributes associated with points")
	  }	
      if(!any(class(y)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
	    y <- methods::as(y, "Spatial")
		  if(dim(y@data)[2] == 0) stop("There are no attributes associated with polygons")
	  }
      o <- sp::over(x, y, returnList = TRUE)
      m <- max(unlist(lapply(o, nrow)))
        ids <- row.names(y)
          xy <- data.frame(t(sapply(1:length(o), 
            function(i) c(ids[i], c(o[[i]][,1], rep(NA, m))[1:m])
          )))
        colnames(xy) <- c("p",paste0("pid", 1:m))
      x@data <- data.frame(x@data, xy)
	    if( sp == FALSE ) sf::st_as_sf(x) 
	  return( x )
    } else {
  if(any( class(x) == c("SpatialPoints", "SpatialPointsDataFrame"))) {
    x <- sf::st_as_sf(x)
  }
  if(any(class(y) == "sfc")) { x <- sf::st_sf(y) }
  if(any( class(y) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
    y <- sf::st_as_sf(y)	
  } 
  if(dim(x)[2] == 1) x$pt.ids <- 1:nrow(x)	
    if(dim(y)[2] == 1) y$poly.ids <- 1:nrow(y)	  
      o <- sf::st_join(x, y, ...)
   if( sp ) o <- methods::as(o, "Spatial")
  # if( sp ) o <- sf::as_Spatial(o)
  return( o ) 
  } 
}
