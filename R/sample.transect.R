#' @title Sample transect
#' @description Creates random transects from points and generates 
#'              sample points along each transect
#' 
#' @param x           A sf point object
#' @param min.dist    Minimum length of transect(s)
#' @param max.dist    Maximum length of transect(s)
#' @param distance    A vector of distances, same length as x, used
#'                    to define transect distances (length)
#' @param azimuth     A vector of azimuths, same length as x, used
#'                    to define transect direction
#' @param id          A unique identification column in x
#' @param ...         Additional arguments passed to st_sample 
#'
#' @note 
#' Function create lines and samples using random or defined direction 
#' and length transects and then creates a point sample along each transect. 
#' The characteristic of the sample points are defined by arguments passed 
#' to the sf::st_sample function. The distance and azimuth arguments allow
#' for specifying the exact length and direction for each points transect.   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples 
#' 
#' library(sf)
#' if(require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' }
#' meuse <- meuse[sample(1:nrow(meuse),10),]
#' 
#' transects <- sampleTransect(meuse, min.dist=200, max.dist=500, 
#'                             type="regular", size=20)
#'    plot(st_geometry(transects$transects))
#'      plot(st_geometry(meuse), pch=19, cex=2, add=TRUE)
#'        plot(st_geometry(transects$samples), 
#' 	        col="red", pch=19, add=TRUE)
#' 
#' @export sampleTransect
sampleTransect <- function(x, min.dist, max.dist, distance = NULL,
                           azimuth = NULL, id = NULL, ...) {
  if (!inherits(x, "sf")) 
    stop(deparse(substitute(x)), " must be an sf POLYGON object")
  if(!is.null(distance)) {
    if(length(distance) != nrow(x))
	  stop("length of distance does not match x")
  }  
  if(!is.null(azimuth)) {
    if(length(azimuth) != nrow(x))
	  stop("length of azimuth does not match x")
  }  	
  if(!is.null(id)) {
    if(!names(x) %in% id) 
	  stop(id, " not present in ", deparse(substitute(x)))
    ids <- sf::st_drop_geometry(x[,id])[,1]	   
  }	else {
    ids <- 1:nrow(x)
  }
  dots <- as.list(match.call(expand.dots = TRUE)[-1])
    if (is.null(dots[["size"]]) & "size" %in% names(dots) == FALSE) 
    dots[["size"]] <- 10 
  if (is.null(dots[["type"]]) & "type" %in% names(dots) == FALSE) 
    dots[["type"]] <-  "random" 
  message(paste("Creating", dots$size, dots$type, "samples for each transect"))
  tlines <- list()   
  tpoints <- list()
    for(i in 1:nrow(x) ) {
	  p <- x[i,]
      if(!is.null(azimuth)) {      
	    az = azimuth[i]   
	  } else {
        az = stats::runif(1, 0, 360)
      }	 
      if(!is.null(azimuth)) {      
	    d = distance[i]  
	  } else {
        d = stats::runif(1, min.dist, max.dist)
      }	 	  
      samp.pt <- spatialEco::bearing.distance(
	                 sf::st_coordinates(p)[,1], 
	                 sf::st_coordinates(p)[,2], 
	                 distance = d, azimuth = az)
        l <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(
               rbind(sf::st_coordinates(p)[,1:2], samp.pt))),
			     crs=sf::st_crs(x))
		  sf::st_geometry(l) <- "geometry"
		    l$ID <- ids[i]
	  dots[["x"]] <- l		
	  pts <- suppressWarnings(sf::st_as_sf(sf::st_cast(
	           do.call(sf::st_sample, dots),"POINT")))
		  sf::st_geometry(pts) <- "geometry"
		    pts$ID <- ids[i]			   
	  tpoints[[i]] <- pts 
	  tlines[[i]] <- l
    }	
  return( list( transects = do.call("rbind", tlines), 
          samples = do.call("rbind", tpoints) ) )	
}    
