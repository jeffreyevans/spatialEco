#' @title Polygon perimeter
#' @description Calculates the perimeter length(s) for a polygon object
#'
#' @param x   sp class SpatialPolygonsDataFrame object 
#'
#' @return A vector of polygon perimeters
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' library(sp)
#' p1 <- Polygons(list(Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))), "1")
#' p2 <- Polygons(list(Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))), "2")
#' p3 <- Polygons(list(Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))),"3")
#' polys <- SpatialPolygons(list(p1,p2,p3), 1:3)
#' 
#' polyPerimeter(polys)
#'
#' @export
polyPerimeter <- function(x) {
      p <- vector()
          for(i in 1:length(x)) {
           px <- as(x[i,], "SpatialLines")
           p <- append(p, sp::LineLength(as.matrix(sp::coordinates(px)[[1]][[1]])))
          }
        return( p )
      }  
	  