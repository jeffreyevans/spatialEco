#' @title is.whole
#' @description Boolean for evaluating whole numbers
#'  
#' @param a  A numeric vector to evaluate, only first element will be evaluated
#' @param tol numeric >= 0, differences smaller than tolerance are not reported
#'
#' @return A Boolean indicating if number is whole or float
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' is.whole( 1 )
#' is.whole( 1.5 )
#' is.whole( 0.5 )
#'
#' @export is.whole
is.whole <- function(a, tol = 1e-7) { 
   is.eq <- function(x,y) { 
	 r <- all.equal(x,y, tolerance=tol)
	 is.logical(r) && r 
   }
   (is.numeric(a) && is.eq(a, floor(a))) ||
   (is.complex(a) && {ri <- c(Re(a),Im(a)); is.eq(ri, floor(ri))})
}  
