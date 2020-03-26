#' @title Statistical transformation for rasters
#' @description Transforms raster to a specified statistical transformation 
#' 
#' @param x       raster class object
#' @param trans   Transformation method: "norm", "rstd", "std", "stretch", 
#'                 "nl", "slog", "sr" (please see notes)
#' @param smin    Minimum value for stretch 
#' @param smax    Maximum value for stretch
#' 
#' @return raster class object of transformation
#'
#' @description
#' Transformation option details:
#' * norm - (Normalization_ (0-1): if min(x) < 0 ( x - min(x) ) / ( max(x) - min(x) )
#' * rstd - (Row standardize) (0-1): if min(x) >= 0 x / max(x) This normalizes data 
#' *        with negative distributions
#' * std - (Standardize) (x - mean(x)) / sdv(x)
#' * stretch - (Stretch) ((x - min(x)) * max.stretch / (max(x) - min(x)) + min.stretch) 
#'              This will stretch values to the specified minimum and maximum values 
#'              (eg., 0-255 for 8-bit)
#' * nl - (Natural logarithms) if min(x) > 0 log(x)
#' * slog - (Signed log 10) (for skewed data): if min(x) >= 0 ifelse(abs(x) <= 1, 0, 
#'           sign(x)*log10(abs(x))) 
#' * sr - (Square-root) if min(x) >= 0 sqrt(x) 
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples 
#' \donttest{
#'   library(raster)
#'   r <- raster(nrows=100, ncols=100, xmn=571823, xmx=616763, 
#'               ymn=4423540, ymx=4453690)
#'     r[] <- runif(ncell(r), 1000, 2500)
#'
#'  # Postive values so, can apply any transformation    
#'	for( i in c("norm", "rstd", "std", "stretch", "nl", "slog", "sr")) {
#'	  print( raster.transformation(r, trans = i) ) 
#'    }
#'
#'  # Negative values so, can't transform using "nl", "slog" or "sr"
#'	r[] <- runif(ncell(r), -1, 1)
#'    for( i in c("norm", "rstd", "std", "stretch", "nl", "slog", "sr")) {
#'	  try( print( raster.transformation(r, trans = i) ) ) 
#'    }
#' }
#'
#' @export raster.transformation
raster.transformation <- function(x, trans = "norm", smin=0, smax=255) {
  slog <- function(x) { ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))}
    rmin <- raster::cellStats(x, stat = "min", na.rm = TRUE)
    rmax <- raster::cellStats(x, stat = "max", na.rm = TRUE)
    rmean <- raster::cellStats(x, stat = "mean", na.rm = TRUE)
    rsd <- raster::cellStats(x, stat = "sd", na.rm = TRUE)
	
  if( trans == "slog" && rmin < 0) {
    stop(" Minimum value < 0, cannot log transform")
  }
  if( trans == "nl" && rmin < 0) {
    stop(" Minimum value < 0, cannot log transform")
  }
  if( trans == "sr" && rmin < 0) {
    stop(" Minimum value < 0, cannot log transform")
  }
  if( trans == "norm" && rmin < 0) {
    print(" Min value < 0, running row standardization instead")
    return( x / rmax )
  }
  
  if( trans == "norm") {
    message("applying normalization transformation", "\n")
    return( ( x - rmin ) / ( rmax - rmin ) )
   } else if ( trans == "rstd") {
     message("applying row-standardization transformation", "\n")
     return( x / rmax )
      } else if ( trans == "std") {
	    message("applying standardization transformation", "\n")
	    return( (x - rmean) / rsd )
	     } else if ( trans == "stretch") {
		   message("applying stretch transformation", "\n")
		   return( (x - rmin) * smax / (rmax - rmin) + smin )
		    } else if ( trans == "nl") {
			  message("applying log transformation", "\n")
			  return(  raster::calc(x, fun=log) )
			  } else if ( trans == "slog") {
			    message("applying singned-log10 transformation", "\n")
			    return(raster::calc(x, fun=slog) )
			    } else if ( trans == "sr") {
				  message("applying sqare-root transformation", "\n")
			      return(  raster::calc(x, fun=sqrt) )		  
		          } else {
                    stop("Not a valid transformation") 
    }		
}
