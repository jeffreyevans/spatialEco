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
#' if(require(sf, quietly = TRUE)) {
#'   e <- c(61.87125, 23.90153, 76.64458, 37.27042)
#'     names(e) <- c("xmin", "ymin", "xmax", "ymax")
#'     s <- st_as_sf(st_sample(st_as_sfc(st_bbox(e)), size=100, 
#'                    type = "regular"))
#'   	st_crs(s) <- st_crs(4326)
#'         s$id <- 1:nrow(s)
#'   
#'   b <- geo.buffer(x=s, r=1000)
#'     plot(st_geometry(b[1,]))
#'        plot(st_geometry(s[1,]), pch=20,cex=2, add=TRUE)
#' }	 
#' 	
#' @seealso \code{\link[sf]{st_buffer}} for st_buffer ... arguments
#'
#' @export geo.buffer 
geo.buffer <- function(x, r, ...) {
  prj <- sf::st_crs("+proj=aeqd  +R=6371000 +lat_0=51 +lon_0=7")
  if(missing(x) | missing(r))
    stop("must supply x and r arguments")
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf POINT object")	
  if(unique(as.character(st_geometry_type(x))) != "POINT")
    stop(deparse(substitute(x)), " must be an sf POINT object")			
  if(is.na(st_crs(s)))
    stop(deparse(substitute(x)), " must have a defined projection")	  
  if(!sf::st_is_longlat(x))
    stop("Data appears to be projected and not Latitude/Longitude")
  b <- sf::st_buffer(sf::st_transform(x, prj), dist=r, ...) 
    b <- sf::st_transform(b, sf::st_crs(4326))
  return( b )	
}
