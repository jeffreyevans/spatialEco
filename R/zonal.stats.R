#' @title zonal.stats
#' @description Polygon zonal statistics of a raster 
#'
#' @param ...  arguments passed to terra::extract
#' @return NA   
#'
#' @examples
#'  \dontrun{
#'   terra::extract()
#' }
#'
#' @export 
zonal.stats <- function(...) {
  .Deprecated("zonal.stat", package="spatialEco", 
    msg="Function is deprecated because terra::extract or 
	exactextractr::exact_extract can accept custom functions 
		 for specialized statistics  ")
}
