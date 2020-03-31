#' @title Erase points
#' @description Removes points intersecting a polygon feature class
#'    
#' @param y       A SpatialPoints or SpatialPointsDataFrame      
#' @param x       A SpatialPolygons or SpatialPolygonsDataFrame
#' @param inside  (TRUE/FALSE) Remove points inside polygon, else outside polygon
#'
#' @return A SpatialPoints or SpatialPointsDataFrame  
#' 
#' @note Provides the same functionality as the ESRI ArcGIS Erase Point tool
#'
#' @author Jeffrey S. Evans    <jeffrey_evans<at>tnc.org>
#'
#' @examples 
#' require(sp)
#'   data(meuse)
#'   coordinates(meuse) = ~x+y
#' poly <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(
#'             Polygon(cbind(c(180042, 180545, 180553, 180314, 179955,
#'             179142, 179437, 179524, 179979, 180042), c(332373, 332026,
#' 			   331426, 330889, 330683, 331133, 331623, 332152, 332357,
#'             332373)))),'1'))), data.frame(row.names=c('1'), PIDS=1))
#'
#' meuse.erase <- erase.point(meuse, poly)
#' 
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2))
#'   plot(poly,)
#'     points(meuse, pch=20)
#'   plot(poly)
#'     points(meuse.erase, pch=20)
#' par(opar)
#'
#' @export erase.point
erase.point <- function(y, x, inside = TRUE) {
    #if(class(y) == "sf") {y <- as(y, "Spatial")}
	#if(class(x) == "sf") {x <- as(x, "Spatial")}
    if (!inherits(y, "SpatialPointsDataFrame") | !inherits(y, "SpatialPoints")) 
      stop("y must be a SpatialPoints or SpatialPointsDataFrame")
	if (!inherits(x, "SpatialPolygonsDataFrame") | !inherits(x, "SpatialPolygons")) 
      stop("x must be a SpatialPolygons or SpatialPolygonsDataFrame")	
  if(inside) {
    return( y[-which(rgeos::gIntersects(y, x, byid = TRUE)),] )
  } else {
    return( y[which(rgeos::gIntersects(y, x, byid = TRUE)),] )  
  }
}
