#' @title Polygon perimeter
#' @description Calculates the perimeter length(s) for a polygon object
#'
#' @param x   sf POLYGON class object 
#'
#' @return A vector of polygon perimeters in projection units
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' if(require(sf, quietly = TRUE)) {
#'   polys <- st_read(system.file("shape/nc.shp", package="sf"))
#'     polys <- suppressWarnings(st_cast(polys[c(10,100),], "POLYGON"))
#'  
#'  polyPerimeter(polys)
#' }
#'
#' @export
polyPerimeter <- function(x) {
  if (!inherits(x, "sf")) 
    stop(deparse(substitute(x)), " must be an sf POLYGON object")
  if(!unique(as.character(sf::st_geometry_type(x))) %in% c("POLYGON", "MULTIPOLYGON"))
    stop(deparse(substitute(x)), " must be an sf POLYGON object")		
  if(unique(as.character(sf::st_geometry_type(x))) %in% "MULTIPOLYGON")
    stop("Function does not support multi-part MLTIPOLYGON objects")		
  p <- suppressWarnings(sf::st_length(sf::st_cast(x, "LINESTRING")))
  return( units::drop_units(p) )
}  
