#' @title Point and Polygon Intersect
#' @description Intersects point and polygon feature classes and adds polygon attributes to points
#'
#' @param x         sp SpatialPointsDataFrame or SpatialPoints or sf point object
#' @param y         sp SpatialPolygonsDataFrame or sf polygon object
#' @param sp        (TRUE/FALSE) Return an sp class object, else returns sf class object
#' @param ...       Additional arguments passed to sf::st_join  
#'
#' @return A SpatialPointsDataFrame or sf 
#'
#' @note Depends: sp
#' @note If poly.id is NULL a column "pids" is created based on the rownames of the polygons
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#'  # sp example - simple one-to-one feature overlay. Returns number of points from 
#'  #   original data
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
#'  # sf example - complex many-to-one feature overlay. Recycles points from multiple polygon 
#'  #   intersections and can return more points than original data 
#'  require(sf)
#'  p <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#'  polys <- st_sf(sf::st_sfc(p, p + c(.8, .2), p + c(.2, .8)))
#'  pts <- sf::st_sf(sf::st_sample(polys, size=100))
#'  
#'  plot(polys)
#'    plot(pts)
#'  
#'  polys$PIDS <- 1:nrow(polys)
#'  polys$y <- runif(nrow(polys))
#'  pts$ATT <- 1:nrow(pts)
#'  
#'  # Point in polygon overlay
#'  pts.poly <-  point.in.poly(pts, polys)
#'    head(pts.poly@data)
#'  
#'  # Count points in each polygon
#'  tapply(pts.polys$ATT, pts.polys$PIDS, FUN=length)      
#'
#' @import sf 
#' @export
point.in.poly <- function(x, y, sp = TRUE, ...) {
  if(!any(class(x)[1] == c("SpatialPoints", "SpatialPointsDataFrame", "sf"))) {
      stop("x is not a suitable point feature object class") }
  if(!any(class(y)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame", "sf"))) {
      stop("y is not a suitable polygon feature object class") }
  if(any(class(x) == "sfc")) { x <- sf::st_sf(x) }
  if(any( class(x) == c("SpatialPoints", "SpatialPointsDataFrame"))) {
    x <- st_as_sf(x) 
  }
  if(any(class(y) == "sfc")) { x <- sf::st_sf(y) }
  if(any( class(y) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
    y <- st_as_sf(y)  
  }  
    o <- sf::st_join(x, y, ...)
  if( sp ) o <- as(o, "Spatial")
 return( o )  
}
