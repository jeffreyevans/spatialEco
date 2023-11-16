#' @title Maximum extent of multiple rasters
#' @description returns a extent polygon representing maximum extent of
#'              input rasters
#'
#' @param x    terra SpatRaster class object
#' @param ...  additional SpatRaster class objects in same projection
#'
#' @details 
#' Creates a maximum extent polygon of all specified rasters
#'
#' @return An sf POLYGON class object representing maximum extents
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(terra)
#' 
#' r1 <- rast(ext(61.87125, 76.64458, 23.90153, 37.27042))
#' r2 <- rast(ext(67.66625, 81.56847, 20.38458, 35.67347))
#' r3 <- rast(ext(72.64792,84.38125,5.91125,28.13347 ))
#' 
#' ( e <- max_extent(r1, r2, r3) )
#' plot(e, border=NA)
#'   plot(ext(r1), border="red", add=TRUE)
#'   plot(ext(r2), border="green", add=TRUE)
#'   plot(ext(r3), border="blue", add=TRUE)
#'   plot(e, border="black", add=TRUE)
#'			    
#'  sf::st_bbox(e) # full extent
#'
#' @export max_extent
max_extent <- function(x, ...) {
  if(length(list(...))){
    dots <- list(...)
	dots[[length(dots)+1]] <- x
  } else {
    dots <- list()
	dots[[1]] <- x
  }
  for(i in dots) { 
    if(!inherits(i, "SpatRaster"))		
      stop("One of the rasters is not a terra SpatRaster object")	
  } 
  e <- lapply(dots, function(i) {
    as.vector(terra::ext(i))[c(1,3,2,4)] 
  })
  if(length(dots) > 1) {  
    e <- do.call("rbind", lapply(e, function(b) {
      sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(b[1], 
                   b[2], b[3], b[4]))))
    }))
  } else {
    e <- e[[1]]
    e <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(e[1], 
                      e[2], e[3], e[4]))))
  }
  return(
    sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(e)))
  )
}
