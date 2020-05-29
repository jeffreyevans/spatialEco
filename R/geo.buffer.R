#' @title Buffer geographic data 
#' @description Buffers data in geographic (Latitude/Longitude) projection
#' 
#' @param x    A sf or sp vector class object
#' @param r    Buffer radius in meters
#' @param sf   (FALSE/TRUE) Output sf class object else sp
#' @param ...  Additional arguments passed to gBuffer
#'
#' @return an sp or sf polygon class object representing buffer for each feature
#'
#' @note 
#' Projects (Latitude/Longitude) data in decimal-degree geographic projection 
#' using an on-the-fly azimuthal equidistant projection in meters centered on
#  each feature.  
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(sp)
#' library(raster)
#' 
#' s <- spsample(as(extent(61.87125, 76.64458, 23.90153, 37.27042), 
#'               "SpatialPolygons"), n=100, type="random")
#'   proj4string(s) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'  
#'   
#' b <- geo.buffer(x=s, r=1000, quadsegs=100)
#'   plot(b[1,])
#'     points(s[1,], pch=20,cex=2)
#' 	
#' @seealso \code{\link[rgeos]{gBuffer}} for gBuffer ... arguments
#'
#' @export geo.buffer 
geo.buffer <- function(x, r, sf = FALSE, ...) {
  if(missing(x))
    stop("must supply x argument")
  if(!any(grep(paste(c("SpatialPoints", "SpatialPolygons", "sf"), collapse="|"), class(x))))	
    stop("x must be sp or sf class object")
  if(class(x)[1] == "sf") { x <- as(x, "Spatial") }  
  if(sp::is.projected(x))
    stop("Data appears to be projected and not Latitude/Longitude")
  results <- list()  
    for(i in 1:length(x)){
	  l <- x[i,]
      p <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                   l@coords[[2]], l@coords[[1]])
      b <- rgeos::gBuffer(sp::spTransform(l, sp::CRS(p)), width = r, 
	                      byid = TRUE, ...)
	    b <- sp::spChFIDs(b, as.character(i))
    results[[i]] <- sp::spTransform(b, x@proj4string)
	}
	  b <- do.call(rbind, results)
    if(sf) { b <- as(b, "sf") } 
  return( b )	
}
