#' @title Hexagons
#' @description Create hexagon polygons
#'                                                                                                                                                                              
#' @param x     sf class object indicating extent
#' @param res   Area of resulting hexagons
#' 
#' @details 
#' Based on extent of x, creates a hexagon mesh with size of hexagons defined by res argumnet 
#' 
#' @return sf POLYGONS object
#'                                                                 
#' @examples 
#' library(sf)
#' if(require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#'
#' hex <- hexagons(meuse, res=300)   
#'   plot(st_geometry(hex))
#'     plot(st_geometry(meuse),pch=20,add=TRUE)
#' 
#' # subset hexagons to intersection with points
#' idx <- which(apply(st_intersects(hex, meuse, sparse=FALSE), 1, any))
#' hex.sub <- hex[idx,] 
#'   plot(st_geometry(hex.sub))
#'     plot(st_geometry(meuse),pch=20,add=TRUE)
#'
#' } else { 
#'   cat("Please install sp package to run example", "\n")
#' }
#'
#' @export      
hexagons <- function(x, res = 100) {
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf object")
  if(sf::st_is_longlat(x))
    stop("Data appears to be in Latitude/Longitude 
	  and needs to be projected")
  e <- sf::st_as_sfc(sf::st_bbox(x))
    res <- sf::st_as_sf(sf::st_make_grid(e, cellsize = res, 
                        square = FALSE))
        sf::st_geometry(res) <- "geometry"
      sf::st_crs(res) <- sf::st_crs(x) 						
  return(res)
} 
