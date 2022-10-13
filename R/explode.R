#' @title Explodes multipart features
#' @description Explodes multipart features into single part  
#'
#' @param ... Parameters to be passed to st_cast
#'
#' @note 
#' Multipart geometries are a data structure where a single attribute 
#' shares multiple features (polygons, points, lines). This function 
#' dissaggregates the data into a one-to-one match.    
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \donttest{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- suppressWarnings(st_cast(nc, "POLYGON"))
#' }
#'
#' @export
explode <- function(...) {
  .Deprecated("explode", package="spatialEco", 
    msg="Function is deprecated because sf provides the ability to explode 
	    multipart geometries using the sf::st_cast function ")
 message("An example for polygons is: st_cast(x, POLYGON) ")	
}
