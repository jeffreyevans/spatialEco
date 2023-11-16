#' @title Remove duplicate geometries
#' @description Removes duplicate geometries in a single-part feature class
#'
#' @param x           An sf POINT, POLYGON or LINESTRING object
#' @param threshold   A distance threshold indicating fuzzy duplication,
#'                    default i 0.00001
#'
#' @details
#' This function removes duplicate geometries based on order and not "non null" 
#' attribution or other factors, the first feature gets to stay. If one needs to 
#' know which points were removed sf::st_difference can be used between original 
#' data and results of the function.  
#'
#' @return sf object, of same feature class as x, with duplicate geometries removed
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples
#' library(sf)
#' 
#' # data with 10 duplicate obs
#' s <- data.frame(x = runif(100), y = runif(100))
#'   s <- data.frame(rbind(s, s[sample(1:nrow(s), 10),]) ) 
#'     s <- st_as_sf(s, coords = c("x", "y"))
#'       s$ID <- 1:nrow(s)
#' 
#' nrow(s) 
#' nrow( srmd <- remove_duplicates(s) )
#'
#' @export
remove_duplicates <- function(x, threshold=0.00001) {
  if(missing(x))
    stop("must supply x argument")
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf object")	
  if(!unique(as.character(sf::st_geometry_type(x))) %in% 
     c("POINT","POLYGON", "LINESTRING"))
    stop("Multipart geometry is not supported, must be 
	     POINT, POLYGON, or LINESTRING")	
  if(unique(as.character(sf::st_geometry_type(x))) == "POLYGON")
    warning("For POLYGON geometry, zero distance may indicate that a polygon
	  is entirely contained within another polygon (island effect)")		 
  d <- sf::st_is_within_distance(x, dist=threshold)
    dups <- unlist(mapply(function(x,y) x[x < y], d, seq_along(d)))
	  if(length(dups) > 0) {
	    message(length(dups), " duplicate observations removed")
          x <- x[-dups, ]
		return(x)
	  } else {
        message("No duplicate observations found")
      }		  
}
