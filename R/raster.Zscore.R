#' @title Modified z-score for a raster
#' @description Calculates the modified z-score for all cells 
#'              in a raster
#' 
#' @param x                A raster class object
#' @param p.value          Return p-value rather than z-score 
#'                         raster (FALSE/TRUE)
#' @param file.name        Name of raster written to disk
#' @param ...              Additional arguments passed to writeRaster
#'
#' @return raster class object or raster written to disk
#'
#' @note 
#' Since this functions needs to operate on all of the raster values, 
#' it is not memory safe 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \donttest{
#' library(raster)
#' r <- raster(nrows=824, ncols=767, xmn=2451905, xmx=3218905, 
#'             ymn=-2744771, ymx=-1920771, resolution = 5000)
#'   r[] <- runif(ncell(r), 0, 1)
#'
#' # Modified z-score
#' z <- raster.Zscore(r)
#'
#' # P-value
#' p <- raster.Zscore(r, p.value = TRUE)
#' }	
#'
#' @export raster.Zscore
raster.Zscore <- function(x, p.value = FALSE, file.name = NULL, ...) {
  r.vals <- raster::getValues(x)
  na.idx <- which(!is.na(r.vals))
  if( p.value == TRUE ) {
    x[na.idx] <- stats::pnorm( outliers( stats::na.omit(raster::getValues(x)) ) )
  } else {
    x[na.idx] <- outliers( stats::na.omit(raster::getValues(x)) )  
  }
    if(!is.null(file.name)) { 
      raster::writeRaster(x, ...)
    } else {
      return( x )  
    }
} 
