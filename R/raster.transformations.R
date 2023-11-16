#' @title Statistical transformation for rasters
#' @description Transforms raster to a specified statistical transformation 
#' 
#' @param x       A terra SpatRaster class object
#' @param trans   Transformation method: "norm", "rstd", "std", "stretch", 
#'                 "nl", "slog", "sr" (please see notes)
#' @param smin    Minimum value for stretch 
#' @param smax    Maximum value for stretch
#' 
#' @details
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
#' @return A terra SpatRaster class object of specified transformation
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples 
#' \donttest{
#' library(terra)
#' r <- rast(nrows=500, ncols=500, xmin=571823, xmax=616763, 
#'             ymin=4423540, ymax=4453690)
#'   crs(r) <- "epsg:9001"
#' r[] <- runif(ncell(r), 1000, 2500)
#'
#'  # Positive values so, can apply any transformation    
#'  for( i in c("norm", "rstd", "std", "stretch", "nl", "slog", "sr")) {
#'    print( raster.transformation(r, trans = i) ) 
#'  }
#'
#'  # Negative values so, can't transform using "nl", "slog" or "sr"
#'  r[] <- runif(ncell(r), -1, 1)
#'    for( i in c("norm", "rstd", "std", "stretch", "nl", "slog", "sr")) {
#'	  try( print( raster.transformation(r, trans = i) ) ) 
#'    }
#' }
#'
#' @export raster.transformation
raster.transformation <- function(x, trans = "norm", smin=0, smax=255) {
  slog <- function(x) { ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))}
   if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")    
	  rmin <- terra::global(x, "min", na.rm = TRUE)[,1]
	  rmax <- terra::global(x, "max", na.rm = TRUE)[,1]
  	  rmean <- terra::global(x, "mean", na.rm = TRUE)[,1]
  	  rsd <- terra::global(x, "sd", na.rm = TRUE)[,1]	
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
    message(" Min value < 0, running row standardization instead")
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
			  return(  terra::app(x, fun=log) )
			  } else if ( trans == "slog") {
			    message("applying signed-log10 transformation", "\n")
			    return(terra::app(x, fun=slog) )
			    } else if ( trans == "sr") {
				  message("applying square-root transformation", "\n")
			      return( terra::app(x, fun=sqrt) )		  
		          } else {
                    stop("Not a valid transformation") 
    }		
}
