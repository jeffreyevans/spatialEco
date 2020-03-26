#' @title Raster multidimensional scaling (MDS) 
#' @description Multidimensional scaling of raster values within an 
#'              N x N focal window
#'                                                                       
#' @param r              Raster layer  
#' @param s              Window size (may be a vector of 1 or 2) of 
#'                       n x n dimension. 
#' @param window.median  (TRUE/FALSE) Return the median of the MDS 
#'                        matrix values. 
#' @param ...            Additional arguments passed to raster::focal    
#'
#' @return A raster class object or raster written to disk 
#'
#' @description
#' An MDS focal function. If only one value provided for s, then a square matrix 
#' (window) will be used. If window.median = FALSE then the center value of the 
#' matrix is returned and not the median of the matrix   
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references
#' Quinn, G.P., & M.J. Keough (2002) Experimental design and data analysis 
#'   for biologists. Cambridge University Press. Ch. 18. Multidimensional 
#'   scaling and cluster analysis.
#' 
#' @examples 
#' \donttest{ 
#'  library(raster)
#'  r <- raster(system.file("external/rlogo.grd", package="raster")) 
#'    r <- r / cellStats(r, "max")
#'  
#'  diss <- raster.mds(r)
#'  diss.med <- raster.mds(r, window.median = TRUE)
#'    par(mfrow=c(2,2))
#'    plot(r)
#'      title("R logo band-1")
#'    plot( focal(r, w = matrix(1, nrow=5, ncol=5), fun = var) )
#'      title("Variance")		 
#'      plot(diss)
#'        title("MDS")
#'      plot(diss.med)
#'        title("Median MDS")
#' }
#'
#' @export raster.mds  
raster.mds <- function(r, s = 5, window.median = FALSE, ...) {
  if (!inherits(r, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")  
    if( length(s) == 1) s = c(s[1],s[1])
  cmd.diss <- function(x, n = s, k = 1, med = window.median) {
    na.idx <- which(is.na(x))
    x <- x[!is.na(x)]
    if(length(x) < 2) {
      cmd <- NA	
	} else {
      if(stats::var(x) == 0) { 
        cmd <- 0
      } else {		
     cmd <- stats::cmdscale(stats::dist(x), k=1)
	 if(length(na.idx) > 0) cmd <- spatialEco::insert.values(cmd, NA, na.idx)
	   if(n[1] != n[2]) {
	     n=sort(n)
	     cmd <- matrix(cmd, nrow = n[2], ncol = n[1], byrow = TRUE)
		 post <- c(round(ncol(cmd)/2), round(nrow(cmd)/2)+1) 
	   } else {
	     cmd <- matrix(cmd, nrow = n, ncol = n, byrow = TRUE)
		 post <- c(round(ncol(cmd)/2)+1, round(nrow(cmd)/2)+1) 
	   }  
	     if( med == TRUE ) {
           cmd <- stats::median(cmd, na.rm = TRUE)
         } else {		
	       cmd <- cmd[,post[1]][post[2]]
	     }
	   }
     }	
    return(cmd)
  }  
  diss <- raster::focal(r, w = matrix(1, nrow=s[1], ncol=s[2]), 
	                    fun = cmd.diss, ...)
} 
