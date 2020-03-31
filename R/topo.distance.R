#' @title Topographic distance
#' @description Calculates topographic corrected distance for a 
#'              SpatialLinesDataFrame object
#'
#' @param x     sp SpatialLinesDataFrame object
#' @param r     raster class elevation raster
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
#' \dontrun{       
#'  library(sp)
#'  library(raster)
#'  library(GeNetIt)
#'  
#'  # create example data
#'  data(elev)
#'    r <- projectRaster(elev, res=c(1000,1000), 
#'                       crs="+proj=aea +lat_1=29.5 +lat_2=42.5")
#'    e <- extent(616893.6,714697.3,5001027,5080542)
#'      elev <- crop(r,e) 
#'        names(elev) <- "elev"
#'  pts <- sampleRandom(elev, 10, sp=TRUE)
#'    pts$ID <- LETTERS[seq( from = 1, to = nrow(pts) )]
#'    
#'  graph <- GeNetIt::knn.graph(pts, row.names=pts@data[,"ID"])
#'    proj4string(graph) <- proj4string(elev)
#'    head(graph@data)
#'   
#'  plot(elev)
#'    plot(graph, cex=0.5, add=TRUE)
#'    plot(pts,pch=19,col="red",add=TRUE)
#'  
#'  # Calculate topographical distance  
#'  ( tdist <- topo.distance(graph, elev) )
#'  
#'  # Increase in corrected distance
#'  tdist - graph$length
#'  
#'  # Percent increase in corrected distance
#'  ((tdist - graph$length) / graph$length) * 100
#' }
#'
#' @export topo.distance
topo.distance <- function(x, r, echo = FALSE) {
  if (class(x) != "SpatialLinesDataFrame") 
    stop("x must be a SpatialLinesDataFrame object")
  if (class(r) != "RasterLayer") 
    stop("r must be a raster object")
  step.dist <- function(x) {
    d <- vector()
      for(i in 1:(nrow(x)-1)){
        d <- append(d, rgeos::gDistance(x[i,], x[i+1,]))
      }
    return( d <- append(d, NA) )	
  }
  line.dist <- vector()
    for(i in 1:nrow(x)) {
	  if(echo) cat("Calculating corrected distance for:", i, "of",  nrow(x), "\n")
      graph.pts <- spatialEco::sample.line(x[i,], d = raster::res(r)[1] )     
        graph.pts$elev <- raster::extract(r, graph.pts)
          z <- graph.pts[graph.pts$LID == i,]
          d <- step.dist(z)
          z <- z@data$elev
      n <- length(z) - 1
      rise <- abs( z[2:(n+1)] - z[1:n] )
	  d <- sum( d[!is.na(d)] + ( d[!is.na(d)] * 
	             (rise / d[!is.na(d)]) ), na.rm = TRUE)
      sl.length <- sp::SpatialLinesLengths(x[i,])
      if(sl.length > d) { 
	    line.dist[i] <- sl.length 
	  } else { 
	    line.dist[i] <- d 
	  }				 
    }
  return(line.dist)
}
