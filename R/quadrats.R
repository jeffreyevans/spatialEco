#' @title Quadrats
#' @description Creates quadrat polygons for sampling or analysis  
#'
#' @param x  A sp or sf polygon object defining extent      
#' @param s  Radius defining single or range of sizes of quadrats    
#' @param n  Number of quadrats    
#' @param r  A rotation factor for random rotation, default is NULL
#'               
#' @return an sp or sf polygon object with rotated polygon
#'
#' @note 
#' The radius (s) parameter can be a single value or a range of values, 
#' representing a randomization range of resulting quadrat sizes. The
#' rotation (r) parameter can also be used to defined a fixed rotation or
#' random range of quadrat rotations. You can specify each of these parameters
#' using an explicit vector that will be sampled eg., seq(100,300,0.5)         
#' 
#' @examples
#' library(sp)
#' library(raster)
#' library(rgeos)
#' data(meuse)
#'   coordinates(meuse) <- ~x+y
#'   e <- gConvexHull(meuse)
#' 
#' # Fixed size 250 and no rotation 
#' s <- quadrats(e, s = 250, n = 50)
#'   spplot(s, "ID")
#' 
#' # Variable sizes 100-300 and rotation of 0-45 degrees
#' s <- quadrats(e, s = c(100,300), n = 50, r = c(0,45))
#'   spplot(s, "ID")
#' 
#' # Variable sizes 100-300 and no rotation 
#' s <- quadrats(e, s = c(100,300), n = 50)
#'   spplot(s, "ID")
#' 
#' @export quadrats
quadrats <- function(x, s = 250, n = 100, r = NULL) { 
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
	    rrange = range(r)
	    if(min(rrange) < 0 | max(rrange) > 360)
	      stop("rotation parameter is out of range")
	    if(length(r) == 1) {
	      rr = r
	    } else if(length(r) == 2) {
          rr = sample(s[1]:s[2],1)
	    } else if(length(rr) > 2) {
          rr = sample(rr,1)	    
	    }
	  }  
      p <- as(raster::extent(rgeos::gBuffer(sp::spsample(x, 1, "random"), 
	          width = ss)), "SpatialPolygons")
        p <- sp::SpatialPolygonsDataFrame(p, data.frame(ID = i))
	  if(!is.null(r)) {	
        quadrat[[i]] <- rotate.polygon(p, angle = rr, sp=TRUE)
      } else {
	    quadrat[[i]] <- p
	  }
    }
      quadrat <- do.call("rbind", quadrat)
    sp::proj4string(quadrat) <- sp::proj4string(x) 
  return(quadrat)   
}
