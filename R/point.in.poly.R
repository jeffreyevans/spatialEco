#' @title Point and Polygon Intersect
#' @description Intersects point and polygon feature classes and adds polygon 
#'              attributes to points
#'
#' @param ...  arguments passed to sf::st_intersection
#' @return NA   
#'
#' @examples
#'  \dontrun{
#'   sf::st_intersection()
#' }
#'
#' @export 
point.in.poly <- function(...) {
  .Deprecated("point.in.poly", package="spatialEco", 
    msg="Function is deprecated because sf::st_intersection
	intersections points an polygons and returns associated
		 attributes  ")
}
