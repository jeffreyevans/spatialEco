#' @title Erase points
#' @description Removes points intersecting a polygon feature class
#'    
#' @param y       A SpatialPoints or SpatialPointsDataFrame      
#' @param x       A SpatialPolygons or SpatialPolygonsDataFrame
#' @param inside  (TRUE/FALSE) Remove points inside polygon, else outside polygon
#'
#' @return A SpatialPoints or SpatialPointsDataFrame  
#' 
#' @note 
#' Used to erase points that intersect polygon(s). If inside=FALSE then
#' the function results in an intersection operation where points that
#' intersect the polygon are retained. This function effectively duplicates
#' the ESRI ArcGIS Erase Point tool.
#'
#' @author Jeffrey S. Evans    <jeffrey_evans<at>tnc.org>
#'
#' @examples 
#' library(sp)
#' library(raster)
#' library(rgeos)
#'   data(meuse)
#'   coordinates(meuse) = ~x+y
#' 
#' # Create systematic sample and polygons
#' s <- spsample(x=as(extent(meuse), "SpatialPolygons"), n=1000,
#'               type="regular")
#' b <- rgeos::gBuffer(s[sample(1:length(s),5),],
#'                     byid = FALSE, width = 300)
#' 
#' # Erase points based on polygons
#' s.erase <- erase.point(s, b)
#'  
#'  opar <- par(no.readonly=TRUE)
#'  par(mfrow=c(2,2))
#'    plot(s, pch=20, main="original data")
#'    plot(b, main="erased data")
#'      points(s.erase, pch=20)
#'    plot(b, main="erased data using inside=FALSE")
#'      points(erase.point(s, b, inside=FALSE), pch=20)
#'  par(opar)
#' 
#' @export erase.point
erase.point <- function(y, x, inside = TRUE) {
  if(class(y) == "sf") { y <- as(y, "Spatial") }
  if(class(x) == "sf") { x <- as(x, "Spatial") }
  if (!any(class(y) == c("SpatialPointsDataFrame","SpatialPoints")))
    stop("y must be a SpatialPoints or SpatialPointsDataFrame")
  if (!any(class(x) == c("SpatialPolygonsDataFrame","SpatialPolygons")))
    stop("x must be a SpatialPolygons or SpatialPolygonsDataFrame")
  idx <- rgeos::gIntersects(y, x, byid = TRUE) 
    idx <- which(apply(idx, MARGIN=2, FUN=function(x) any(x==TRUE)))
  if(inside) { 
    y <- y[-idx,]
  } else { 
    y <- y[idx,]
  }
  return(y)
}
