#' @title Point and Polygon Intersect
#' @description Intersects point and polygon feature classes and adds polygon attributes to points
#'
#' @param x         sp SpatialPointsDataFrame or SpatialPoints object
#' @param y         sp SpatialPolygonsDataFrame object
#' @param poly.id   Name of column contaning unique polygon ID's  
#'
#' @return A SpatialPointsDataFrame with intersected polygon ID's
#'
#' @note Depends: sp
#' @note If poly.id is NULL a column "pids" is created based on the rownames of the polygons
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' require(sp)
#' data(meuse)
#' coordinates(meuse) = ~x+y
#' meuse@data$test.na <- NA
#' 
#' sr1=Polygons(list(Polygon(cbind(c(180114, 180553, 181127, 181477, 181294,  
#'   181007, 180409, 180162, 180114), c(332349, 332057, 332342, 333250, 333558,  
#'   333676, 332618, 332413, 332349)))),'10')
#' sr2=Polygons(list(Polygon(cbind(c(180042, 180545, 180553, 180314, 179955, 179142,  
#'   179437, 179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683, 
#'   331133, 331623, 332152, 332357, 332373)))),'20')
#' sr3=Polygons(list(Polygon(cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 
#'   179875, 179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
#'   c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004, 
#'   329783, 329665, 329720, 329933, 330478, 331062, 331086)))),'30')
#' sr4=Polygons(list(Polygon(cbind(c(180304, 180403,179632,179420,180304),
#'   c(332791, 333204, 333635, 333058, 332791)))),'40')
#' sr=SpatialPolygons(list(sr1,sr2,sr3,sr4))
#' srdf=SpatialPolygonsDataFrame(sr, data.frame(row.names=c('10','20','30','40'), 
#'                               PIDS=1:4, y=runif(4)))
#'    srdf@data$pid <- srdf@data$PIDS + 100 
#' 
#' # Intersect points with polygons and add polygon IDS to pts@@data. 
#'   pts.poly <- point.in.poly(meuse, srdf, poly.id = "pid") 
#'     head(pts.poly)
#' 
#' # Point counts for each polygon
#' tapply(pts.poly$lead, pts.poly$pid, FUN=length)  
#'
#' @export
point.in.poly <- function(x, y, poly.id = NULL) {
    # if(class(x) == "sf") { x <- as(x, "Spatial") }
	# if(class(y) == "sf") { y <- as(x, "Spatial") }
    if (!inherits(y, "SpatialPolygonsDataFrame")) 
        stop("y must be a SpatialPolygonsDataFrame object")
    if ((inherits(x, "SpatialPointsDataFrame") | inherits(x, "SpatialPoints")) == FALSE) 
        stop("x must be a SpatialPointsDataFrame object")
	if(!is.null(poly.id)) {
      if( length(unique(y@data[,poly.id])) != nrow(y) ) stop("poly.id not unique")
    } 	
    if(is.null(poly.id)) { 
	  y@data$pids <- rownames(y@data)
      poly.id = "pids"
    }	  
	z <- x[!is.na(sp::over(x, sp::geometry(y))),]
	  z@data <- data.frame(sp::over(x, y))
	    row.ids <- rownames(z@data)
	    z@data <- data.frame(z@data[,which(names(y) %in% poly.id)])
          names(z) <- poly.id
            rownames(z@data) <- row.ids		  
	      z@data <- data.frame(z@data, merge(z, x@data, by="row.names", all.x = FALSE))
		rm.idx <- which(names(z) %in% c("x", "y", "Row.names", "optional", paste0(poly.id, ".1")))
	  if(sum(rm.idx) > 0) z@data <- z@data[,-rm.idx]
    z@proj4string <- x@proj4string
  return( z )
} 
