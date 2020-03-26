#' @title Landscape metrics for points and polygons
#' @description Calculates a variety of landscape metrics, on 
#'              binary rasters, for polygons or points with a 
#'              buffer distance 
#'   
#' @param ... Parameters to be passed to the modern version 
#'            of the function
#'    
#' @examples
#' \dontrun{
#' library(landscapemetrics)
#' library(raster)
#' 
#' data(landscape)
#' points <- matrix(c(10, 5, 25, 15, 5, 25), 
#'                  ncol = 2, byrow = TRUE)
#' 
#' sample_lsm(landscape, y = points, size = 10, 
#'            level = "landscape", type = "diversity metric", 
#'            classes_max = 3,
#'            verbose = FALSE)
#' }	   
#' @export
land.metrics <- function(...) {
  .Deprecated("land.metrics", package="spatialEco", 
    msg="Function is deprecated, for methodological consistency, 
	      please use functions in the landscapemetrics package (on CRAN) ")
 message("Please see land.metrics help for example of replicating function 
           using sample_lsm in landscapemetrics package")	
}
