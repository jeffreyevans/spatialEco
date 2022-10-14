#' @title sp na.omit
#' @description Removes row or column NA's in sp object
#'
#' @param ...  arguments passed to stats::na.omit   
#' @return NA                                                                
#'
#' @export 
sp.na.omit <- function(...) {
  .Deprecated("sp.na.omit", package="spatialEco", 
    msg="Function is deprecated because stats::na.omit operates on 
	  sf class objects ")
}
