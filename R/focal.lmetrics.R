#' @title Focal landscape metrics
#' @description Calculates a variety of landscape metrics on  
#'              integer rasters using focal approach
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
#' 
#' s <- matrix(1, nrow = 3, ncol = 3)
#' ( result <- do.call(stack, window_lsm(landscape, window = s, 
#'                   what = c("lsm_l_pr", "lsm_l_joinent"))) )
#'   plot(result)
#' }
#' @export
focal.lmetrics <- function(...) {
  .Deprecated("focal.lmetrics", package="spatialEco", 
    msg="Function is deprecated, for methodological consistency, 
	      please use functions in the landscapemetrics package (on CRAN) ")
 message("Please see focal.lmetrics help for example of replicating function 
           using window_lsm in landscapemetrics package")	
}
