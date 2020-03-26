#' @title Dutilleul moving window bivariate raster 
#'        correlation 
#' @description A bivarate raster corrlation using Dutilleul's 
#'              modified t-test
#'   
#' @param ... Parameters to be passed to the modern version 
#'            of the function
#'    
#' @export
mwCorr <- function(...) {
     .Deprecated("mwCorr", package="spatialEco", 
	 msg="Function is deprecated, please use raster.modified.ttest")
}
