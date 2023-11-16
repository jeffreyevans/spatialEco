#' @title Background sample
#'
#' @description Creates a point sample that can be used as 
#'              a NULL for SDM's and other modeling approaches. 
#'
#' @param x      A sf class polygon defining sample region    
#' @param p      Size of sample
#' @param known  An sf POINT class of known locations with same CSR as x
#' @param d      Threshold distance for known proximity 
#' @param type   Type of sample c("systematic", "random", "hexagon", "nonaligned")
#'
#' @details 
#' This function creates a background point sample based on an extent 
#' or polygon sampling region. The known argument can be used with d 
#' to remove sample points based on distance-based proximity to existing  
#' locations (eg., known species locations). The size (p) of the resulting 
#' sample will be dependent on the known locations and the influence of 
#' the distance threshold (d). As such, if the know and d arguments are
#' provided the exact value provided in p will not be returned. 
#'
#' @return A sf POINT feature class or data.frame with x,y coordinates
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' library(sf)
#' 
#' # define study area
#' sa <- suppressWarnings(st_cast(st_read(
#'         system.file("shape/nc.shp", 
#'         package="sf")), "POLYGON"))
#'   sa <- sa[10,]
#' 
#' # create "known" locations  
#' locs <- st_sample(sa, 50)
#'   st_crs(locs) <- st_crs(sa)
#' 
#' # systematic sample using extent polygon
#' e <- st_as_sf(st_as_sfc(st_bbox(sa)))
#'   st_crs(e) <- st_crs(sa)
#' s <- background(e, p=1000, known=locs, d=1000)
#'   plot(st_geometry(s), pch=20)
#'     plot(st_geometry(locs), pch=20, col="red", add=TRUE)
#' 
#' # systematic sample using irregular polygon
#' s <- background(sa, p=1000, known=locs, d=1000)
#'   plot(st_geometry(sa)) 
#'     plot(st_geometry(s), pch=20, add=TRUE)
#'       plot(st_geometry(locs), pch=20, col="red", add=TRUE)
#' 
#' # random sample using irregular polygon
#' s <- background(sa, p=500, known=locs, 
#'                 d=1000, type="random")
#'   plot(st_geometry(sa)) 
#'     plot(st_geometry(s), pch=20, add=TRUE)
#'       plot(st_geometry(locs), pch=20, col="red", add=TRUE)
#' 
#' @export
background <- function(x, p=1000, known=NULL, d=NULL, 
                type=c("regular", "random", "hexagon", "nonaligned")) {
    if(is.null(d)) 
      stop("distance (d) must be defined")  
    if(missing(x))
      stop("extent or x argument must be defined")	 
	if(inherits(x, c("sf", "sfc"))) {   
	  if(as.character(unique(sf::st_geometry_type(x))) != "POLYGON")
        stop(deparse(substitute(x)), " x must be an sf POLYGON object")	
    }
  if(length(find.package("lwgeom", quiet = TRUE)) == 0)
      stop("please install lwgeom package before running this function")
  if(!is.null(known)){
    if(!inherits(known, c("sf", "sfc")))  
      stop(deparse(substitute(known)), " must be an sf POINT object")
    if(as.character(unique(sf::st_geometry_type(known))) != "POINT")
      stop(deparse(substitute(known)), " must be an sf POINT object")
	if(sf::st_crs(x) != sf::st_crs(known))
      stop("CSR of known and x do not match")	  
  }
  s <- sf::st_as_sf(sf::st_sample(x=x, size=p, type=type[1]))
    if(!is.null(known)) {
      idx <- unique(unlist(sf::st_is_within_distance(known, s,  
                     dist = d,sparse = TRUE)))
        s <- s[-idx,] 
    }
    if(!is.na(sf::st_crs(x))) {
      sf::st_crs(s) <- sf::st_crs(x) 
    }	  
  return(s)
}
