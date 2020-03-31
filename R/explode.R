#' @title Explodes multipart features
#' @description Explodes multipart features into single part  
#'
#' @param x     sp or sf multipart (MULTIPOLYGON, MULTIPOINT, MULTILINE) 
#'              object
#' @param sp    (FALSE/TRUE) output as sp class object, else is sf class
#'
#' @return A single part sp or sf object (polygons or points) 
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
#' library(sp)
#' 
#' dim( p.sf <- st_read(system.file("shapes/sids.shp", package = "spData")[1]) )
#' dim( p.sf <- explode(p.sf) )
#' }
#'
#' @import sf
#' @export
explode <- function(x, sp = FALSE) {
  sp.types = paste0("Spatial", c("Points","Lines", "Polygons"), "DataFrame")
  if(!any(class(x)[1] == c(sp.types, "sf"))) {
    stop("x is not a suitable feature object class") 
  }  
  if(any(class(x)[1] == c("sfc", "sfg"))) { 
    x <- sf::st_sf(x) 
  }  
  if(any(class(x)[1] == sp.types)) { 
    x <- as(x, "sf") 
  } 
  if(length(unique(as.character(st_geometry_type(x)))) > 1) 
    stop("x is a GEOMETRYCOLLECTION but needs to represent a single geometry type") 	
  g <- unique(as.character(sf::st_geometry_type(x)))	
  if(!any(unique(g) == c("MULTIPOLYGON", "MULTIPOINT", "MULTILINE")))
    stop("x does not appear to have multipart geometry") 
  message(paste0("Converting multipart ", g, " to single part ", substring(g, 6, last = 1000000L)))	
	  x <- suppressWarnings( sf::st_cast(x, substring(g, 6, last = 1000000L)) )
    if(sp) x <- as(x, "Spatial")
  return(x)	
}
