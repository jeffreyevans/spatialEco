#' @title is.empty
#' @description evaluates empty elements in a vector
#'  
#' @param x           A vector to evaluate elements
#' @param all.na      (FALSE / TRUE) Return a TRUE if all elements are NA
#' @param na.empty    (TRUE / FALSE) Return TRUE if element is NA
#' @param trim        (TRUE / FALSE) Trim empty strings
#'
#' @return A Boolean indicating empty elements in a vector, if all.na = FALSE
#' a TRUE/FALSE value will be returned for each element in the vector 
#' 
#' @description This function evaluates if an element in a vector is empty
#' the na.empty argument allows for evaluating NA values (TRUE if NA) and
#' all.na returns a TRUE if all elements are NA. The trim argument trims
#' a character string to account for the fact that c(" ") is not empty but,
#' a vector with c("") is empty. Using trim = TRUE will force both to return TRUE   
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' is.empty( c("") )
#' is.empty( c(" ") )
#' is.empty( c(" "), trim=FALSE )
#' 
#' is.empty( c("",NA,1) )
#' is.empty( c("",NA,1), na.empty=FALSE)
#' 
#' is.empty( c(NA,NA,NA) )
#' is.empty( c(NA,NA,NA), all.na=TRUE )
#' is.empty( c(NA,2,NA), all.na=TRUE )
#' 
#' any( is.empty( c("",2,3) ) )
#' any( is.empty( c(1,2,3) ) )
#'
#' @export is.empty
is.empty <- function (x, all.na = FALSE, 
                      na.empty = TRUE, 
					  trim = TRUE) {
  if(trim) x <- gsub("^\\s+|\\s+$", "", x)
  if (!is.null(x)) {
    if(length(x) == 0) { 
      z <- TRUE
	} else {
    if (is.character(x)) {
      z <- nchar(x) == 0
	  }
    }
  }
  if(na.empty) z[is.na(z)] <- TRUE 
    if(all.na){ 
	  if(all(is.na(x))){ 
	    z <- TRUE
	  } else {
	    z <- FALSE
	  }
	}  	  
  return(unname(z))  
}