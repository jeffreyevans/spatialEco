#' @title 2-dimensional kernel density estimate
#' @description Calculates 2-dimensional kernel density estimate over specified extent
#'   
#' @param ... Parameters to be passed to the modern version of the function
#'   
#' @export
kde.2D <- function(...) {
     .Deprecated("mwCorr", package="spatialEco", 
	 msg="kde2D is deprecated, please use sp.kde which provides a weighted option")
  kde.2D(...)
}
