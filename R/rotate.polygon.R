#' @title Rotate polygon
#' @description rotates polygon by specified angle
#'
#' @param p       A polygon object of sf or sp class
#' @param angle   Rotation angle in degrees
#' @param sp      (FALSE | TRUE) Output sp class object
#' @param anchor  Location to rotate polygon on options are "center", 
#'               "lower.left" and "upper.right" 
#'
#' @details 
#' The anchor is the location that the rotation is anchored to. The center
#' is the centroid where the lower.left and upper.right are based on the 
#' min or max of the coordinates respectively. 
#'
#' @return an sp or sf polygon object with rotated polygon
#' 
#' @examples
#' library(sf)
#'  
#' data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), 
#'                     crs = 28992, agr = "constant")
#'  
#'  e <- st_convex_hull(st_union(meuse))
#'    e30 <- rotate.polygon(e, angle=30)
#'  
#'  plot(e, main="rotated 30 degrees")
#'    plot(e30, add=TRUE)
#'  
#' @export rotate.polygon
rotate.polygon <- function(p, angle = 45, sp = FALSE,  
    anchor = c("center", "lower.left", "upper.right")) {
  if(class(p)[1] != "sf") p <- sf::st_as_sf(p)
    p.coords <- sf::st_coordinates(p)[,1:2]
  if(anchor[1] == "center") {
    p.center <- suppressWarnings(sf::st_coordinates(sf::st_centroid(p)))
  } else if(anchor[1] == "lower.left") {
    p.center <- c(min(sf::st_coordinates(p)[,1]),
                  min(sf::st_coordinates(p)[,2]))
  } else if(anchor[1] == "upper.right") { 
    p.center <- c(max(sf::st_coordinates(p)[,1]),
                  max(sf::st_coordinates(p)[,2])) 
  }  
  rotate.coords <- function(xy, a, center) {
    co <- cos(-a * pi / 180)
      si <- sin(-a * pi / 180)
        adj <- matrix(rep(center, nrow(xy)), ncol=2, byrow=TRUE)
      xy <- xy-adj
    cbind(co * xy[,1] - si * xy[,2],si * xy[,1] + co * xy[,2]) + adj
  }
      p.rotate <- rotate.coords(p.coords, a = angle, center = c(p.center[1], 
	                            p.center[2]))
	sf::st_geometry(p) <- sf::st_sfc(sf::st_polygon(list(p.rotate)))
    if(sp) p <- sf::as_Spatial(p)
  return( p )	
}
