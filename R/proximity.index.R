#' @title Proximity Index 
#' @description Calculates proximity index for a set of polygons  
#'
#' @param   x           A polygon class sp or sf object
#' @param   y           Optional column in data containing classes
#' @param   min.dist    Minimum threshold distance
#' @param   max.dist    Maximum neighbor distance
#' @param   background  Optional value in y column indicating background value 
#'
#' @return A vector equal to nrow(x) of proximity index values, if a background value is specified
#'         NA values will be returned in the position(s) of the specified class
#'   
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Gustafson, E.J., & G.R. Parker (1994) Using an Index of Habitat Patch Proximity 
#'               for Landscape Design. Landscape and Urban Planning 29:117-130
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' 
#' # Create test polygons
#' data(meuse)
#'   coordinates(meuse) = ~x+y
#'   meuse_poly <- gBuffer(meuse, width = meuse$elev * 5, byid = TRUE)
#'     meuse_poly$LU <- sample(c("forest","nonforest"), nrow(meuse_poly), replace=TRUE) 
#' 
#' # All polygon proximity index 1000 radius	
#' ( pidx <-proximity.index(meuse_poly, min.dist = 1) )
#'   pidx[pidx > 100] <- 100
#' 
#' # Class-level proximity index 1000 radius
#' ( pidx.class <- proximity.index(meuse_poly, y = "LU", min.dist = 1) )
#'   pidx.class[pidx.class > 100] <- 100  
#'   
#' # plot index for all polygons
#' meuse_poly$pidx <- pidx
#'   spplot(meuse_poly, "pidx")
#' 
#' # plot index for class-level polygons 
#' meuse_poly$cpidx <- pidx.class
#'   spplot(meuse_poly, "cpidx")
#' 
#' # plot index for just forest class
#' forest <- meuse_poly[meuse_poly$LU == "forest",]
#'   spplot(forest, "cpidx")
#'   
#' @export
proximity.index <- function(x, y = NULL, min.dist = 0, max.dist = 1000, 
                            background = NULL) {
  if(any(class(x) == "sf")) { x <- as(x, "Spatial") }
    if(!any(class(x) == "SpatialPolygonsDataFrame"))
      stop("x must be an sp SpatialPolygonsDataFrame object")  	  
  if(!is.null(y)) {  
    if(!any(y %in% names(x))) 
	  stop("Column (y) is not in polygon data")
	classes <- unique(x@data[,y])  
    if(!is.null(background)) {
	  if(!any(background %in% unique(x@data[,y])))
        stop("Background class not in data")
		bg.idx <- which( x@data[,y] == background ) 
        x <- x[x@data[,y] != background,]
        classes <- classes[-grep(background, classes)]		
	  }	  
    class.pidx <- vector()
      rn <- vector()	
    for(j in classes) {   
	  xx <- x[x@data[,y] == j,]
	  rn <- append(rn,rownames(xx@data))
      dmat <- rgeos::gDistance(xx, byid=TRUE)
        dmat[dmat <= min.dist | dmat > max.dist] <- NA
      a <- rgeos::gArea(xx, byid=TRUE)	
	  pidx <- vector()
        for(i in 1:nrow(dmat)) {
	      idx <- colnames(dmat)[which(!is.na(as.numeric(dmat[,i])))]
		  pidx[i] <- sum(a[which(names(a) %in% idx)] * dmat[,i][idx]^-2)
        }
      class.pidx <- append(class.pidx, pidx)
 	}
	pidx <- class.pidx[order(match(rownames(x@data),rn))]
  } else {
	dmat <- rgeos::gDistance(x, byid=TRUE)
      dmat[dmat <= min.dist | dmat > max.dist] <- NA
	a <- rgeos::gArea(x, byid=TRUE)
	pidx <- vector()
      for(i in 1:nrow(dmat)) {
	    idx <- colnames(dmat)[which(!is.na(as.numeric(dmat[,i])))]
		#pidx[i] <- sum(a[which(names(a) %in% idx)] / dmat[,i][idx])
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
