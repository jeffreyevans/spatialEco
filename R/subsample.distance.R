#' @title Distance-based subsampling
#'
#' @description Draws a minimum, and optional maximum constrained, distance sub-sampling 
#' 
#' @param x             A POLYGON or POINT sf object 
#' @param size          Subsample size 
#' @param d             Minimum sampling distance in meters
#' @param d.max         Maximum sampling distance in meters
#' @param replacement   (FALSE/TRUE) Subsample with replacement   
#'
#' @return A subsampled POLYGON or POINT sf object  
#'
#' @note This function provides a distance constrained subsample of existing point 
#'       or polygon data. Please note that units are in meters regardless of input
#'       CRS projection units (including lat/long).   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples      
#' \donttest{
#' if(require(sp, quietly = TRUE)) {
#' library(sf)
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' 
#' # Subsample with a 500m minimum sample spread 
#' sub.meuse <- subsample.distance(meuse, size = 10, d = 500)  
#'   plot(st_geometry(meuse), pch=19, main="min dist = 500")
#'     plot(st_geometry(sub.meuse), pch=19, col="red", add=TRUE) 
#'  
#' # Check distances	
#' dm <- st_distance(sub.meuse)
#'   diag(dm) <- NA
#' cat("\n", "Min distance for subsample", min(dm, na.rm=TRUE), "\n")  
#' cat("Max distance for subsample", max(dm, na.rm=TRUE), "\n")  
#' 
#' } else { 
#'   cat("Please install sp package to run example", "\n")
#' }
#' }
#' @export subsample.distance
subsample.distance <- function(x, size, d, d.max = NULL, 
                               replacement = FALSE) {						   
  gtypes = c("POLYGON", "POINT", "MULTIPOLYGON", "MULTIPOINT")			 
  if(missing(x)) 
    stop("Must define a spatial object")
  if(missing(d)) 
    stop("Must define minimum separation distance")
  if(!is.null(d.max)) {
    if(d.max <= d) 
	  stop("Maximum distance must be larger than minimum")
  } 
  if(!inherits(x, c("sf", "sfc")))
    stop(deparse(substitute(x)), " must be an sf object or coercible")	  
  if(any(unique(as.character(sf::st_geometry_type(x))) == gtypes[3:4]))
    stop("Function does not support multi-part geometry")  
  if(!any(unique(as.character(sf::st_geometry_type(x))) != gtypes[1:2]))
    stop(deparse(substitute(x)), " must be one of ", 
	     paste(gtypes, collopse=""))		     
  if( size >= nrow(x)) 
    stop("subsample size must be smaller than population")
  rs <- sample(1:nrow(x),1) 
    s <- x[rs,]
      if(replacement == FALSE) { x <- x[-rs,] }
        deval = TRUE	  
        for(i in 2:size) {	
          nsamp=0
          while(deval == TRUE) { 		  
            rs <- sample(1:nrow(x),1)
	          pts.dist <- units::drop_units(units::set_units(sf::st_distance(s, x[rs,]), "m"))
            if(is.null(d.max)) {
              deval <- any(pts.dist < d, na.rm = TRUE)
		    } else {
		      deval <- any(pts.dist < d, na.rm = TRUE) | any(pts.dist > d.max, na.rm = TRUE)
	        } 
            nsamp = nsamp + 1			
              if(nsamp == nrow(x)) break			
	      }  
        if(nsamp == nrow(x)) {
          message(paste0("Warning: sampling cannot converge at n=", size, " returning n=", nrow(s)))
            return(s)  
        }
	 deval = TRUE
	   s <- rbind(s, x[rs,])
	     if(replacement == FALSE) { x <- x[-rs,] } 
	} 	
  return(s)  
}
