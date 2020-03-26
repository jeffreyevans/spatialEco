#' @title Raster Entropy
#' @description Calculates entropy on integer raster (i.e., 8 bit 0-255)  
#'                                                                       
#' @param x            Object of class raster (requires integer raster)  
#' @param d            Size of matrix (window)
#' @param categorical  Is the data categorical or continuous (FALSE/TRUE)
#' @param global       Should the model use a global or local n to calculate 
#'                     entropy (FALSE/TRUE)
#' @param filename     Raster file written to disk
#' @param ...          Optional arguments passed to writeRaster or dataType              
#'  
#' @return raster class object or specified format raster written to disk                
#'
#' @description
#' Entropy calculated as: H = -sum(Pi*ln(Pi)) where; Pi, Proportion of one value 
#' to total values Pi=n(p)/m and m, Number of unique values. Expected range: 
#' 0 to log(m) H=0 if window contains the same value in all cells.
#' H increases with the number of different values in the window.
#'
#' Maximum entropy is reached when all values are different, same as log(m)
#'   max.ent <- function(x) { log( length( unique(x) ) ) }
#'

#' @references
#' Fuchs M., R. Hoffmann, F. Schwonke (2008) Change Detection with GRASS 
#'   GIS - Comparison of images taken by different sensor. 
#'
#' @examples 
#' require(raster)
#'   r <- raster(ncols=100, nrows=100)
#'     r[] <- round(runif(ncell(r), 1,8), digits=0)
#'
#' rEnt <- raster.entropy(r, d=5, categorical = TRUE, global = TRUE)
#'   opar <- par  
#'     par(mfcol=c(2,1))
#'       plot(r)
#'         plot(rEnt)
#'   par(opar)
#'
#' @export  
raster.entropy <- function(x, d = 5, categorical = FALSE, global = FALSE,  
                           filename = FALSE, ...) {
    if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")
	#if( categorical == FALSE & global == TRUE ) { 
	#  warning("global cannot be true with non-categorical data", call. = FALSE)
	  #global = FALSE
	# }
	if(length(d) == 1) { 
	  d <- matrix(1, nrow=d, ncol=d, byrow=TRUE)
	} else if(length(d) == 2) { 
	  d <- matrix(1, nrow=d[1], ncol=d[2], byrow=TRUE)
	} else {
	  stop("Window matrix cannot have more than two dimensions")
	}
    if(global == TRUE) {
      k = sum(rep(raster::cellStats(x, stat="max"), nrow(d)*ncol(d))) 
    }	
	if(categorical == FALSE) {
      entropy <- function(x, n = NULL) {  
        x <- x[!is.na(x)]
        if (length(unique(x)) <= 1) { return(0) }
        return(-sum(prop.table(x) * log(prop.table(x))))
      }
	  
    } else {
	  entropy <- function(x, n = NULL) {  
        x <- x[!is.na(x)]
        if (length(unique(x)) <= 1) { return(0) }		  
        if(!is.null(n)) { nv = n } else { nv <- sum(table(x)) }
        return(-sum((table(x) / nv) * (log(table(x) / nv))))
      }	
	}	
    if(global == TRUE) {
      if (filename != FALSE) {          	  
        raster::focal(x, w = d, fun = function(x) { entropy(x, n = k) }, 
                      filename = filename, ...)
          message(paste("Raster written to", filename, sep = ": "))				
      	} else {
		return( raster::focal(x, w = d, fun = function(x) { entropy(x, n = k) }) )  
	  }  
	} else {  
	  if (filename != FALSE) {	
        raster::focal(x, w = d, fun = function(x) { entropy(x) }, filename = filename, ...)
          message(paste("Raster written to", filename, sep = ": "))	
	  } else {  
		return(raster::focal(x, w = d, fun = function(x) { entropy(x) }))  
      }
  }	
} 
