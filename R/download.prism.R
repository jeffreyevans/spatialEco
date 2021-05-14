#' @title Download PRISM 
#' @description Batch download of monthly gridded PRISM climate data 
#'
#' @param ... Nonexistent parameters 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @export
download.prism <- function(...) {
  .Deprecated("download.prism", package="spatialEco", 
    msg="Function is deprecated, for methodological consistency, 
	      please use functions in the prism package (on CRAN) ")
}
