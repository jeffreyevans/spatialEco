#' @title Topographic distance
#' @description Calculates topographic corrected distance for a 
#'              LINESTRING object
#'
#' @param x     sf LINESTRING object
#' @param r     terra or raster class elevation raster
#' @param echo  (FALSE/TRUE) print progress to screen
#'
#' @return 
#' Vector of corrected topographic distances same length as nrow(x)
#'
#' @note
#' This function corrects straight-line (euclidean) distances for 
#' topographic-slope effect. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#'  library(sf)
#'  library(raster)
#'  library(terra)
#'  
#'  # create example data
#'  elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'	  r <- crop(r, ext(6482554, 6536750, 6615538, 6667271))
#'      names(elev) <- "elev"
#' 
#'  lns <- lapply(1:5, function(i) {
#'    p <- st_combine(st_as_sf(spatSample(elev, size=2, as.points=TRUE)))
#'    st_as_sf(st_cast(p, "LINESTRING")) }) 
#'  lns <- do.call(rbind, lns) 
#'   
#'   plot(elev)
#'     plot(st_geometry(lns), add=TRUE)
#'       
#'  # Calculate topographical distance  
#'  ( tdist <- topo.distance(lns, elev) )
#'  ( lgt <- as.numeric(st_length(lns)) ) 
#'  
#'  # Increase in corrected distance
#'  tdist - lgt
#'  
#'  # Percent increase in corrected distance
#'  ((tdist - lgt) / lgt) * 100
#'
#' @export topo.distance
topo.distance <- function(x, r, echo = FALSE) {
  att <- attr(class(x), "package")
  if( if(length(att) > 0) { att == "sp" } else { FALSE } )
    x <- sf::st_as_sf(x)
  if(sf::st_geometry_type(x, by_geometry = FALSE) != "LINESTRING")
    stop("x must be a LINESTRING object")
  if(!inherits(r, c("SpatRaster", "RasterLayer")))	
    stop("r must be a terra or raster object")	
  if (!inherits(r, "RasterLayer"))
    r <- terra::rast(r)
  step.dist <- function(x) {
    d <- vector()
      for(i in 1:(nrow(x)-1)){
        d <- append(d, as.numeric(sf::st_distance(x[i,], x[i+1,])))
      }
    return( d <- append(d, NA) )	
  }
  line.dist <- vector()
    for(i in 1:nrow(x)) {
	  if(echo) cat("Calculating corrected distance for:", i, "of", nrow(x), "\n")
	  pts <- sf::st_cast(sf::st_line_sample(x[i,], 
	           density=1/terra::res(r)[1]), "POINT")		   
	    pts <- sf::st_as_sf(pts)
          pts$elev <- terra::extract(r, terra::vect(pts))[,2]
		  d <- c(rep(as.numeric(sf::st_distance(pts[1,],pts[2,])), 
		         nrow(pts)-1), NA)
		  z <- pts$elev
          n <- length(z) - 1
      rise <- abs( z[2:(n+1)] - z[1:n] )
	  d <- sum( d[!is.na(d)] + ( d[!is.na(d)] * 
	             (rise / d[!is.na(d)]) ), na.rm = TRUE)
      sl.length <- as.numeric(sf::st_length(x[i,]))
      if(sl.length > d) { 
	    line.dist[i] <- sl.length 
	  } else { 
	    line.dist[i] <- d 
	  }
    }
  return(line.dist)
}
