#' @description Minimum, and optional maximum, distance constrained sub-sampling
#'
#' @param x             A spatial polygons or points sp object
#' @param n             Number of random samples drawn from x
#' @param d             Minimum sample distance
#' @param d.max         Maximum sample distance (not used unless specified 
#' @param replacement  (FALSE/TRUE) sample with or without replacement
#' @param latlong      (FALSE/TRUE) is the data in a geographic projection (latitude/longitude)
#' @param trace        (FALSE/TRUE) Print min and max sample distances
#' 
#' @return A sp spatial object, of the same class as x containing the random sub-samples 
#'
#' @note This function provides a distance constrained subsample of existing point or polygon data  
#'  
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#'   library(sp)
#'   data(meuse)
#'     coordinates(meuse) <- ~ x+y
#'   
#'   # Subsample with a 500m minimum sample spread 
#'   sub.meuse <- subsample.distance(meuse, n = 10, d = 500, trace = TRUE)  
#'     plot(meuse,pch=19, main="min dist = 500")
#'       points(sub.meuse, pch=19, col="red") 
#'   
#'   # Check distances	
#'   dm <- spDists(sub.meuse)
#'     diag(dm) <- NA
#'   cat("\n", "Min distance for subsample", min(dm, na.rm=TRUE), "\n")  
#'   cat("Max distance for subsample", max(dm, na.rm=TRUE), "\n")  
#'     
#' \dontrun{
#'   # Subsample with a 500m minimum and 3500m maximum sample spread   
#'   sub.meuse <- subsample.distance(meuse, n = 10, d = 500, d.max = 3500)  
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
subsample.distance <- function(x, n, d, d.max = NULL, replacement = FALSE,
                            latlong = FALSE, trace = FALSE) {
  if(missing(x)) stop("Must define a spatial object")
    if(missing(d)) stop("Must define minimum separation distance")
  if(!any(class(x) == c("SpatialPointsDataFrame", "SpatialPolygonsDataFrame")) )
    stop("x must be sp class polygons or points")
  if(latlong == TRUE)  
    message("Note; geographic projection (lat-long) distances must be in kilometers")  
  rs <- sample(1:nrow(x),1) 
    s <- x[rs,]
      if(replacement == FALSE) { x <- x[-rs,] }
        deval = TRUE	  
        for(i in 2:n) {	
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
		  if(trace) cat("Sample iteration=", nsamp, "\n")  
            if(nsamp == nrow(x)) break			
	      }  
		if(trace) {
		  cat("\n","Min distance for", i, "=", min(pts.dist, na.rm=TRUE), "\n")
                  cat(" Max distance for", i, "=", max(pts.dist, na.rm=TRUE), "\n")
		}
        if(nsamp == nrow(x)) {
          message(paste0("Warning: sampling cannot converge at n=", n, " returning n=", nrow(s)))
            return(s)  
        }
	 deval = TRUE
	   s <- rbind(s, x[rs,])
	     if(replacement == FALSE) { x <- x[-rs,] } 
	} 	
  return(s)  
}
