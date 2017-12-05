#' @title Dutilleul moving window bivariate raster correlation 
#' @description A bivarate raster corrlation using Dutilleul's modified t-test
#'       
#' @export
mwCorr <- function(...) {
     .Deprecated("mwCorr", package="spatialEco", 
	 msg="Function is deprecated, please use raster.modified.ttest")
  mwCorr(...)
}
