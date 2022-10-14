#' @title Sample Lines
#' @description Creates a sample for each line in a 
#'              sf LINESTRING class object
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
sample.line <- function(...) {
  .Deprecated("sample.line", package="spatialEco", 
    msg="Function is deprecated because sf provides the ability to 
	     sample lines using sf::st_sample function ")
}
