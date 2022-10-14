#' @title Bounding box polygon
#' @description Creates a polygon from a vector or raster extent
#' 
#' @param x    An sf or terra object or vector of bounding coordinates 
#' @param crs  The CRS assigned to the object, mostly relevant 
#'             for coordinates    
#'
#' @return A single feature sf class polygon object
#'
#' @note
#' If not a spatial object, expected order of input for x is: xmin, ymin, 
#' xmax, ymax. Where; xmin, ymin and the coordinates of top left corner of the 
#' bounding box and xmax, ymax represent the bottom right corner. The maximum 
#' value of xmax is width of the extent while maximum value of ymax is the height 
#' of the extent.
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' p = c("sf", "sp", "terra")
#'   if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
#'     m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
#'     message("Can't run examples, please install ", paste(p[m], collapse = " "))
#'   } else {
#'   invisible(lapply(p, require, character.only=TRUE))
#' 
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' 
#' # vector bounding box
#' e <- bbox_poly(meuse)
#' 
#' plot(st_geometry(meuse), pch=20)
#'   plot(st_geometry(e), add=TRUE)
#' 
#' # raster (terra)
#'   r <- rast(ext(meuse))
#'     r[] <- runif(ncell(r))
#'   e <- bbox_poly(r)
#'   
#'   plot(r)
#'     plot(st_geometry(e), border="red", add=TRUE)
#' 
#' # extent vector
#' e <- bbox_poly(c(178605, 329714, 181390, 333611), 
#'                crs=28992) 
#' plot(e)
#' 
#' }
#'
#' @export bbox_poly
bbox_poly <- function(x, crs=NULL) {
  if(inherits(x, "SpatRaster")) {
   e <- as.vector(terra::ext(x))[c(1,3,2,4)] 
     e <- sf::st_as_sfc(sf::st_bbox(c(e[1], e[2], e[3], e[4])))

      if(!is.null(crs)) {
        sf::st_crs(e) <- sf::st_crs(crs)
          message("assigning defined CRS, not feature CRS")		
      } else {
        sf::st_crs(e) <- sf::st_crs(terra::crs(x))    
      }	 
  } else if(inherits(x, c("sf", "sfc"))) {
    e <- sf::st_as_sfc(sf::st_bbox(x))
      if(!is.null(crs)) {
        sf::st_crs(e) <- sf::st_crs(crs)
          message("assigning defined CRS, not feature CRS")		
      } else {
        sf::st_crs(e) <- sf::st_crs(x)   
      }	
  
  } else if(inherits(x, "numeric")) {
    names(x) <- c("xmin", "ymin", "xmax", "ymax")
    e <- sf::st_as_sfc(sf::st_bbox(c(x[1], x[2], x[3], x[4])))
      if(!is.null(crs)){ 
        sf::st_crs(e) <- sf::st_crs(crs)
          message("assigning defined CRS")
      } else {
        message("no CRS defined")
      }	  
  }
  return(sf::st_as_sf(e))
}
