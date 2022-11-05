#' @title polygon raster extract
#' @description Fast method for extracting raster values to polygons 
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
polygon_extract <- function(...) {
  .Deprecated("polygon_extract", package="spatialEco", 
    msg="Function is deprecated because terra::extract or 
	exactextractr::exact_extract benchmarks about the same ")
}
