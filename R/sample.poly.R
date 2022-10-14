#' @title Sample Polygons
#' @description Creates an equal sample of n for each polygon in an 
#'              sp Polygon class object
#'
#' @param ...  arguments passed to sf::st_sample 
#' @return NA   
#'
#' @examples
#'  \dontrun{
#'   sf::sf_sample()
#' }
#'
#' @export
sample.poly <- function(...) {
  .Deprecated("sample.poly", package="spatialEco", 
    msg="Function is deprecated because sf provides the ability to 
	     sample polygons using sf::st_sample function ")
}
