#' @title Distance-based subsampling
#'
#' @description Draws a minimum, and optional maximum constrained, distance sub-sampling 
#' 
#' @param x             A spatial polygons or points sp object
#' @param size          Subsample size 
#' @param d             Minimum sampling distance
#' @param d.max         Maximum sampling distance
#' @param replacement   (FALSE/TRUE) Subsample with replacement   
#' @param latlong       (FALSE/TRUE) Is the data in a geographic projection
#' @param echo          (FALSE/TRUE) Print min and max sample distances
#'
#' @return A subsampled spatial polygons or points sp object  
#'
#' @note This function provides a distance constrained subsample of existing point 
#'       or polygon data  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples      
#' \dontrun{
#' library(sp)
#' data(meuse)
#'   coordinates(meuse) <- ~ x+y
#' 
#' # Subsample with a 500m minimum sample spread 
#' sub.meuse <- subsample.distance(meuse, size = 10, d = 500, echo = TRUE)  
#'   plot(meuse, pch=19, main="min dist = 500")
#'     points(sub.meuse, pch=19, col="red") 
#' 
#' # Check distances	
#' dm <- spDists(sub.meuse)
#'   diag(dm) <- NA
#' cat("\n", "Min distance for subsample", min(dm, na.rm=TRUE), "\n")  
#' cat("Max distance for subsample", max(dm, na.rm=TRUE), "\n") 
#'
#'   # Subsample with a 500m minimum and 3500m maximum sample spread   
#'   sub.meuse <- subsample.distance(meuse, size = 10, d = 500, d.max = 3500)  
#'     plot(meuse,pch=19, main="min dist = 500, max dist = 3500")
#'       points(sub.meuse, pch=19, col="red") 
#'
#'   # Check distances		
#'   dm <- spDists(sub.meuse)
#'     diag(dm) <- NA
#'   cat("Min distance for subsample", min(dm, na.rm=TRUE), "\n")  
#'   cat("Max distance for subsample", max(dm, na.rm=TRUE), "\n")    
#' }
#'
#' @export subsample.distance
subsample.distance <- function(x, size, d, d.max = NULL, replacement = FALSE,
                               latlong = FALSE, echo = FALSE) {
  if(missing(x)) 
    stop("Must define a spatial object")
  if(missing(d)) 
    stop("Must define minimum separation distance")
  if(!is.null(d.max)) {
    if(d.max <= d) 
	  stop("Maximum distance must be larger than minimum")
  }
  if(!any(class(x) == c("SpatialPointsDataFrame", "SpatialPolygonsDataFrame")) )
    stop("x must be sp class polygons or points")
  if(latlong == TRUE) {  
    message("geographic projection distances must be in kilometers")
  }
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
	      pts.dist = sp::spDists(s, x[rs,], longlat = latlong)
            if(is.null(d.max)) {
              deval <- any(pts.dist < d, na.rm = TRUE)
		} else {
		  deval <- any(pts.dist < d, na.rm = TRUE) | any(pts.dist > d.max, na.rm = TRUE)
	    } 
        nsamp = nsamp + 1			
		  if(echo) cat("Sample iteration=", nsamp, "\n")  
            if(nsamp == nrow(x)) break			
	      }  
		if(echo) {
		  cat("\n","Min distance for", i, "=", min(pts.dist, na.rm=TRUE), "\n")
              cat(" Max distance for", i, "=", max(pts.dist, na.rm=TRUE), "\n")
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
