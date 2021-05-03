#' @title Extract vertices for polygons or lines
#' @description Extracts [x,y] vertices from an sp line or polygon object
#'
#' @param x               An sp class SpatialPolygonsDataFrame, SpatialPolygons,
#'                        SpatialLinesDataFrame or SpatialLines object      
#' @param as.sp           (FALSE/TRUE) Output as sp SpatialPointsDataFrame     
#' @param rm.duplicates   (FALSE/TRUE) remove duplicate (x,y) coordinates  
#' @param join            (FALSE/TRUE) Joint attributes from original object 
#'
#' @return 
#' A SpatialPointsDataFrame or data.frame with id, x, y and merged attributes
#'
#' @note
#' This function returns the vertices of a line or polygon object, as opposed
#' to the polygon centroids or line start/stop coordinates available in 
#' the @@coords slot. This requires accessing the coordinates located in the 
#' x@@polygons@@Polygons or x@@lines@@Lines slots 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(sf)
#' 
#' # For polygons
#' r <- raster(xmn=-11.69, xmx=2988.31, ymn=-749.97, ymx=1650.03,
#'             resolution=c(100,100))
#'   r[] <- runif(ncell(r))
#'     names(r) <- "random_process"
#'   
#' polys <- as(r, "SpatialPolygonsDataFrame")
#'   polys <- polys[sample(1:nrow(polys),10),]
#' 
#' extract.vertices(polys, join=TRUE, rm.duplicates=TRUE)
#' 
#' v <- extract.vertices(polys, as.sp=TRUE, join=TRUE)
#'   head(v@data)
#'   
#'   plot(polys)
#'     points(v, pch=20, cex=2, col="red")
#' 
#' # For lines
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'   nc <- sf::st_cast(sf::st_cast(nc, "POLYGON"), "LINESTRING")
#'     nc <- as(nc, "Spatial")
#'  
#' extract.vertices(nc)
#' extract.vertices(nc, join=TRUE, rm.duplicates=TRUE)
#' 
#' v <- extract.vertices(nc, as.sp=TRUE, join=TRUE)
#'   head(v@data)
#' 
#'   plot(nc)
#'     points(v, pch=20, cex=2, col="red")
#' 
#' @export extract.vertices 
extract.vertices <- function(x, as.sp = FALSE, rm.duplicates = FALSE,
                             join = FALSE) {
  if(!any(class(x)[1] == c("SpatialPolygonsDataFrame", "SpatialPolygons",
                        "SpatialLinesDataFrame", "SpatialLines")))
    stop("x must be an sp Polygons or Lines feature class")
  if(any(class(x)[1] == c("SpatialPolygonsDataFrame", "SpatialPolygons")) ){ 
    xy <- lapply(methods::slot(x, "polygons"), function(x) lapply(methods::slot(x,"Polygons"), 
                 function(y) methods::slot(y, "coords"))) 
  } else if(any(class(x)[1] == c("SpatialLinesDataFrame", "SpatialLines")) ){
    xy <- lapply(methods::slot(x, "lines"), function(x) lapply(methods::slot(x,"Lines"), 
                 function(y) methods::slot(y, "coords"))) 
  } 	 
  for(i in 1:length(xy)){
    xy[[i]] <- data.frame(ID=row.names(x)[i], xy[[c(i,1)]])
  }
  xy <- do.call(rbind, xy)
    names(xy) <- c("ID","X","Y")
	if(rm.duplicates) xy <- xy[!duplicated(xy[,2:3]),]
      if(as.sp) sp::coordinates(xy) <- ~X+Y  	
      if(join) { 	  
	    xy <- merge(xy, x@data, by.x="ID", 
	    			by.y="row.names", all.x=TRUE) 
      }
  return( xy )	  
}
