#' @title Proximity Index 
#' @description Calculates proximity index for a set of polygons  
#'
#' @param   x           A polygon class sp or sf object
#' @param   y           Optional column in data containing classes
#' @param   min.dist    Minimum threshold distance
#' @param   max.dist    Maximum neighbor distance
#' @param   background  Optional value in y column indicating background value 
#'
#' @return 
#' A vector equal to nrow(x) of proximity index values, if a background value is 
#' specified NA values will be returned in the position(s) of the specified class
#'   
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Gustafson, E.J., & G.R. Parker (1994) Using an Index of Habitat Patch Proximity 
#'   for Landscape Design. Landscape and Urban Planning 29:117-130
#'
#' @examples
#' \donttest{
#'  library(sf)
#'  if(require(sp, quietly = TRUE)) {
#'    data(meuse, package = "sp")
#'    meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                      agr = "constant")
#'    meuse <- st_buffer(meuse, dist = meuse$elev * 5)
#'      meuse$LU <- sample(c("forest","nonforest"), nrow(meuse), 
#'                        replace=TRUE) 
#' 
#'  # All polygon proximity index 1000 radius	
#'  ( pidx <- proximity.index(meuse, min.dist = 1) )
#'    pidx[pidx > 1000] <- 1000
#'  
#'  # Class-level proximity index 1000 radius
#'  ( pidx.class <- proximity.index(meuse, y = "LU", min.dist = 1) )
#'    
#'  # plot index for all polygons
#'  meuse$pidx <- pidx
#'    plot(meuse["pidx"])
#'  
#'  # plot index for class-level polygons 
#'  meuse$cpidx <- pidx.class
#'    plot(meuse["cpidx"])
#'  
#'  # plot index for just forest class
#'  forest <- meuse[meuse$LU == "forest",]
#'   plot(forest["cpidx"])
#'
#' } else { 
#'   cat("Please install sp package to run example", "\n")
#' }
#' }   
#' @export proximity.index
proximity.index <- function(x, y = NULL, min.dist = 0, max.dist = 1000, 
                            background = NULL) {
  if(!inherits(x, "sf"))
    stop(deparse(substitute(x)), " must be an sf object")	  
  if(!unique(as.character(sf::st_geometry_type(x))) == "POLYGON")
      stop(deparse(substitute(x)), " must be POLYGON geometry (not multi-part) ")
  if(!is.null(y)) {  
    if(!any(y %in% names(x))) 
	  stop("Column (y) is not in polygon data")
	classes <- unique(sf::st_drop_geometry(x[,y])[,1])  
    if(!is.null(background)) {
	  if(!any(background %in% classes))
        stop("Background class not in data")
      bg.idx <- which( sf::st_drop_geometry(x[,y])[,1] == background ) 
        x <- x[sf::st_drop_geometry(x[,y])[,1] != background,]
          classes <- classes[-grep(background, classes)]		
	  }	  
    class.pidx <- vector()
      rn <- vector()	
    for(j in classes) { 
	  xx <- x[which(sf::st_drop_geometry(x[,y])[,1] == j),]
	  rn <- append(rn, rownames(xx))
      dmat <- units::drop_units(sf::st_distance(xx, sparse=FALSE))
	    colnames(dmat) <- rownames(xx)
		rownames(dmat) <- rownames(xx)
	    #diag(dmat) <- NA
        dmat[dmat <= min.dist | dmat > max.dist] <- NA
      a <- sf::st_area(xx)
        names(a) <- rownames(xx)	  
	  pidx <- vector()
        for(i in 1:nrow(dmat)) {
	      idx <- colnames(dmat)[which(!is.na(as.numeric(dmat[,i])))]
		  pidx[i] <- sum(a[which(names(a) %in% idx)] * dmat[,i][idx]^-2)
        }
      class.pidx <- append(class.pidx, pidx)
 	}
	pidx <- class.pidx[order(match(rownames(x),rn))]
  } else {
	dmat <- units::drop_units(sf::st_distance(x, sparse=FALSE))
	  colnames(dmat) <- rownames(x)
	  rownames(dmat) <- rownames(x)	
    dmat[dmat <= min.dist | dmat > max.dist] <- NA
	a <- sf::st_area(x)
	  names(a) <- rownames(x)
	pidx <- vector()
      for(i in 1:nrow(dmat)) {
	    idx <- colnames(dmat)[which(!is.na(as.numeric(dmat[,i])))]
		pidx[i] <- sum(a[which(names(a) %in% idx)] * dmat[,i][idx]^-2)
      }
	}
    if(!is.null(background)) {
	  z <- numeric(length(pidx) + length(bg.idx))
        z[bg.idx] <- NA
        z[-bg.idx] <- pidx
	  pidx <- z
	}
  return ( pidx )
}
