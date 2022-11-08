#' @title Class comparison between two nominal rasters
#' @description Compares two categorical rasters using Cohen's Kappa (d) 
#'              or paired t-test statistic(s)
#'
#' @param ...  arguments passed to raster.change
#' @return NA   
#'
#' @examples
#'  \dontrun{
#'   raster.change()
#' }
#'
#' @export 
class.comparison <- function(...) {
  .Deprecated("class.comparison", package="spatialEco", 
    msg="Function is deprecated because it is now the same as
	the raster.change function, using stats = t.test or kappa  ")
}
