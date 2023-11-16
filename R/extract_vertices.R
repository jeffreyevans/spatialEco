#' @title Extract vertices for polygons or lines
#' @description Extracts [x,y] vertices from an sf line or polygon object
#'
#' @param x       An sf line or polygon class object
#' @param join    (TRUE/FALSE) Joint attributes from original object 
#'
#' @details 
#' This function returns the vertices of a line or polygon object, as opposed
#' to the polygon centroids or line start/stop coordinates 
#'
#' @return 
#' An sf POINT object of extrated line or polygon vertices
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#'
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'   nc <- suppressWarnings(sf::st_cast(nc, "POLYGON"))
#'     nc <- nc[c(10,50),]
#'   
#' ( v <- extract.vertices(nc) )
#'   plot(st_geometry(nc))
#'     plot(st_geometry(v), pch=20, cex=2, col="red", add=TRUE)
#'
#' @export extract.vertices 
extract.vertices <- function(x, join = TRUE) {
  if(!inherits(x, c("sf", "sfc")))		
    stop(deparse(substitute(x)), " must be an sf object")
 if(!unique(as.character(sf::st_geometry_type(x))) %in% c("POLYGON", 
   "MULTIPOLYGON", "MULTILINESTRING", "LINESTRING"))
    stop(deparse(substitute(x)), " must be an sf POINT object")	
  if(join == TRUE) {
    xy <- ncp <- suppressWarnings(sf::st_cast(x, "POINT"))
	# xy <- as.data.frame(st_coordinates(x))[,c(1,2,4)]
    #   names(xy)[3] <- "LID"
    # xy <- split(xy, xy$L2)
    # xy <- lapply(1:nrow(x), function(i) {
    #   data.frame(xy[[i]],
    #     do.call("rbind", replicate(nrow(xy[[i]]), 
    #             sf::st_drop_geometry(x[i,]), 
    #   	   	  simplify = FALSE)))
    # })
    # xy <- do.call("rbind", xy)  
  } else {	
    xy <- as.data.frame(sf::st_coordinates(x))[,c(1,2,4)]
      names(xy)[3] <- "LID"  
    xy <- sf::st_as_sf(xy, coords = c("X", "Y"), 
                       crs = sf::st_crs(x), 
                       agr = "constant")
  } 
  return( xy )
}
