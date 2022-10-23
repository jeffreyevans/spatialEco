#' @title Remove or return polygon holes
#' @description Removes or returns all holes (null geometry) in sf polygon 
#' class objects 
#'
#' @param x           sf POLYGON or MULTIPOLYGON object
#' @param only.holes  Delete holes (FALSE) or returns only holes (FALSE)
#'
#' @return sf POLYGON object
#'
#' @note 
#' A hole is considered a polygon within a polygon (island) representing null 
#' geometry. If you want to return only holes, no longer NULL, use keep = TRUE. 
#' To delete holes use default only.holes = FALSE. Single part features will be 
#' returned regardless of input.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples
#' library(sf)
#' 
#' p <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(
#'   cbind(c(2,4,4,1,2),c(2,3,5,4,2)),
#'   cbind(c(2.33, 2.05, 3.25, 3.25, 2.33), 
#'       c(3.00, 3.56, 3.95, 3.46, 3.00)))),
#'   sf::st_polygon(list(	
#'     cbind(c(5,4,2,5),c(2,3,2,2)))),
#'   sf::st_polygon(list(  
#'     cbind(c(4,4,5,10,4),c(5,3,2,5,5)),
#'     cbind(c(5,6,6,5,5),c(4,4,3,3,4)) 
#' ))))
#'   p$ID <- 1:3
#' 
#' rh <- remove.holes(p)
#' kh <- remove.holes(p, only.holes=TRUE)
#' 
#' opar <- par(no.readonly=TRUE)
#'    par(mfrow=c(2,2))
#'      plot(st_geometry(p), main="Original with holes")
#'      plot(st_geometry(rh), main="holes removed only.holes=FALSE")
#' 	 plot(st_geometry(kh), main="return holes only.holes=TRUE")
#' par(opar)
#' 
#' @export remove.holes
remove.holes <- function(x, only.holes = FALSE) {
  if(!inherits(x, c("sf", "sfc")))		
    stop(deparse(substitute(x)), " must be an sf or sfc object")
 if(!unique(as.character(sf::st_geometry_type(x))) %in% c("POLYGON", "MULTIPOLYGON"))
    stop(deparse(substitute(x)), " must be aPOLYGON object")		
  if(!only.holes) {
   l <- lapply(sf::st_geometry(x), function(i) i[1])
      l <- lapply(l, function(x) {
	         if(length(x) > 0)
             x[[1]]		   
	       })
      l[sapply(l, is.null)] <- NULL
    l <- lapply(l, function(x) sf::st_polygon(list(x)) ) 
	h <- sf::st_as_sf(sf::st_sfc(l))
	  if(all(class(x) == "sf" | ncol(x) > 1)){
	    h <- cbind(h, sf::st_drop_geometry(x))   
	  }
  } else if(only.holes) {
    l <- lapply(sf::st_geometry(x), function(i) i[-1])
      l <- lapply(l, function(x) {
	         if(length(x) > 0)
             x[[1]]		   
	       })
        l[sapply(l, is.null)] <- NULL
	  l <- lapply(l, function(x) sf::st_polygon(list(x)) )
	h <- sf::st_as_sf(sf::st_sfc(l))
	  h$ID <- 1:nrow(h)
  }
    sf::st_geometry(h) <- "geometry"
  return( h )
}
