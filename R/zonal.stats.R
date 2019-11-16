#' @title zonal.stats
#' @description Polygon zonal statistics of a raster 
#'
#' @param x      Polygon object of class SpatialPolygonsDataFrame 
#' @param y      rasterLayer object of class raster
#' @param stats  Statistic or function
#'
#' @return 
#' data.frame, nrow(x) and ncol of function results
#'
#' @note
#' This function calculates the zonal statistics between a polygon vector object and a raster. 
#'   This provides the advantage of being able to accept any custom function, passed to the 'stats' 
#    argument. Please note that any custom function needs to have a 'na.rm' argument.     
#'
#' @note Depends: sp, raster, velox
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples                                                                       
#' # skewness function
#' skew <- function(x, na.rm = FALSE) { 
#'    if (na.rm) 
#'        x <- x[!is.na(x)]
#'   sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
#'   }   
#' 
#' # percent x >= p function
#' pct <- function(x, p=0.30, na.rm = FALSE) {
#'   if ( length(x[x >= p]) < 1 )  return(0) 
#'     if ( length(x[x >= p]) == length(x) ) return(1) 
#'      else return( length(x[x >= p]) / length(x) ) 
#' }
#' 
#' # create some example data
#' library(raster)
#' library(sp)   
#' p <- raster(nrow=10, ncol=10)
#'   p[] <- runif(ncell(p)) * 10
#'     p <- rasterToPolygons(p, fun=function(x){x > 9})
#'       r <- raster(nrow=100, ncol=100)
#'         r[] <- runif(ncell(r)) 
#' plot(r)
#'   plot(p, add=TRUE, lwd=4) 
#' 
#' # run zonal statistics using skew and pct functions   
#' z.skew <- zonal.stats(x = p, y = r, stats = "skew") 
#' z.pct <- zonal.stats(x=p, y=r, stats = "pct")
#'   ( z <- data.frame(ID = as.numeric(as.character(row.names(p@@data))), 
#'                     SKEW=z.skew, PCT=z.pct) )  
#'
#' @import velox
#' @export
zonal.stats <- function(x, y, stats = c("min", "mean", "max")) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
    if (class(y) != "RasterLayer") 
        stop("y must be a raster object")
    if (class(x) != "SpatialPolygonsDataFrame") 
        stop("x must be a SpatialPolygonsDataFrame object")
  rvx <- velox::velox(y) 
    ldf <- rvx$extract(sp = x)
	  names(ldf) <- row.names(x)
	  
    stats.fun <- function(x, m = stats) {
	  slist <- list()
        for(i in 1:length(m)) {
	      slist[[i]] <- apply(x, MARGIN=2, m[i], na.rm=TRUE)
	    }	
	  return( as.data.frame(t(unlist(slist))) )
    }
        results <- lapply(ldf, FUN=stats.fun)
	  results <- do.call("rbind", results)
	rn <- vector()
	  for(n in stats) { rn <- append(rn, paste(n, names(y), sep="."))}
	    names(results) <- rn 
  return( results )
} 
