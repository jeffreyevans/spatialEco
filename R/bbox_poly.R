#' @title Bounding box polygon
#'
#' @description Creates a polygon from a vector or raster extent
#' 
#' @param x    An sf or terra object or vector of bounding coordinates 
#'
#' @details 
#' If not a spatial object, expected order of input for x is: xmin, ymin, 
#' xmax, ymax. Where; xmin, ymin and the coordinates of top left corner of the 
#' bounding box and xmax, ymax represent the bottom right corner. The maximum 
#' value of xmax is width of the extent while maximum value of ymax is the height 
#' of the extent.
#' 
#' @return A single feature sf class polygon object
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' if(require(sp, quietly = TRUE)) {
#' library(terra)
#' library(sf)
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' 
#' # raster (terra)
#' r <- rast(ext(meuse))
#'   r[] <- runif(ncell(r))
#'  crs(r) <- "epsg:28992"
#' e <- bbox_poly(r)
#' 
#' plot(r)
#'   plot(st_geometry(e), border="red", add=TRUE)
#' 
#' # extent vector
#' e <- bbox_poly(c(178605, 329714, 181390, 333611)) 
#'   plot(e)
#' 
#' # vector bounding box
#' e <- bbox_poly(meuse)
#' 
#' plot(st_geometry(meuse), pch=20)
#'   plot(st_geometry(e), add=TRUE)
#' 
#' } else { 
#'   cat("Please install sp package to run this example", "\n")
#' }
#'
#' @export bbox_poly
bbox_poly <- function(x) {
  if(inherits(x, "SpatRaster")) {
    e <- as.vector(terra::ext(x))[c(1,3,2,4)] 
      e <- sf::st_as_sfc(sf::st_bbox(c(e[1], e[2], e[3], e[4])))
	    p <- terra::crs(x)
		if(! p == "") 
	      sf::st_crs(e) <- sf::st_crs(p) 
  } else if(inherits(x, c("sf", "sfc"))) {
     e <- sf::st_as_sfc(sf::st_bbox(x))
       sf::st_crs(e) <- sf::st_crs(x)   
  } else if(inherits(x, "numeric")) {
    names(x) <- c("xmin", "ymin", "xmax", "ymax")
    e <- sf::st_as_sfc(sf::st_bbox(c(x[1], x[2], x[3], x[4])))
  }
  return(sf::st_as_sf(e))
}
