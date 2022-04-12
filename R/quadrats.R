#' @title Quadrats
#' @description Creates quadrat polygons for sampling or analysis  
#'
#' @param x    A sp or sf polygon object defining extent      
#' @param s    Radius defining single or range of sizes of quadrats    
#' @param n    Number of quadrats    
#' @param r    A rotation factor for random rotation, default is NULL
#' @param sp   (FALSE | TRUE) Output sp class object
#'               
#' @return an sf or sp polygon object with rotated polygon(s)
#'
#' @note 
#' The radius (s) parameter can be a single value or a range of values, 
#' representing a randomization range of resulting quadrat sizes. The
#' rotation (r) parameter can also be used to defined a fixed rotation or
#' random range of quadrat rotations. You can specify each of these parameters
#' using an explicit vector that will be sampled eg., seq(100,300,0.5)         
#' 
#' @examples
#' library(sf)
#' library(terra) 
#' 
#' # read meuse data and create convex hull 
#'  data(meuse, package = "sp")
#'    meuse <- st_as_sf(meuse, coords = c("x", "y"), 
#'                      crs = 28992, agr = "constant") 
#'      e <- st_convex_hull(st_union(meuse))
#' 
#'  # Fixed size 250 and no rotation 
#'  s <- quadrats(e, s = 250, n = 50)
#'    plot(st_geometry(s))
#'  
#'  # Variable sizes 100-300 and rotation of 0-45 degrees
#'  s <- quadrats(e, s = c(100,300), n = 50, r = c(0,45))
#'    plot(st_geometry(s))
#'  
#'  # Variable sizes 100-300 and no rotation 
#'  s <- quadrats(e, s = c(100,300), n = 50)
#'   plot(st_geometry(s))
#'  
#' @export quadrats
quadrats <- function(x, s = 250, n = 100, r = NULL, sp = FALSE) { 
  	rrange = range(r)
	if(min(rrange) < 0 | max(rrange) > 360)
	  stop("rotation parameter is out of range")
    quadrat <- list()
    for(i in 1:n) {
	  if(length(s) == 1) {
	    ss = s
	  } else if(length(s) == 2) {
        ss = sample(s[1]:s[2],1)
	  } else if(length(s) > 2) {
        ss = sample(s,1)	    
	  }
	  if(!is.null(r)){
	    if(length(r) == 1) {
	      rr = r
	    } else if(length(r) == 2) {
          rr = sample(r[1]:r[2],1)
	    } else if(length(rr) > 2) {
          rr = sample(rr,1)	    
	    }
	  }       
       p <- sf::st_buffer(sf::st_sample(x, size=1, type="random"), ss)
	     p <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(p)))
		   p$angle <- rr
		     p$dist <- ss
	  if(!is.null(r)) {	
        quadrat[[i]] <- rotate.polygon(p, angle = rr)
      } else {
	    quadrat[[i]] <- p
	  }
    }
        quadrat <- do.call("rbind", quadrat)
	  sf::st_crs(quadrat) <- sf::st_crs(x)
	if(sp) p <- sf::as_Spatial(p)
  return(quadrat)   
}
