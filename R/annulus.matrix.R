#' @title Annulus matrix
#' @description Creates a square matrix representing annulus position values of 1 
#'              and defined null 
#' 
#' @param scale        Number of rings (defines dimensions of matrix)
#' @param inner.scale  Number of inner rings to set to null.value 
#' @param outer.scale  Number of outer rings to set to null.value
#' @param null.value   Value to set inner and outer scale(s) to
#'
#' @return A matrix object with defined null.value and 1, representing retained rings  
#'
#' @note 
#' This function will return a matrix of 1 and defined null.value based on a specification
#' of the scale, inner scale and outer scale. The scale defines how many rings will be
#' represented in the matrix based on (2 * scale - 1). So, a scale of 3 will result in a
#' 5x5 matrix. The inner.scale and outer.scale arguments represent the > and < rings that
#' will be set to the defined null.value (see examples). The resulting matrix can be used
#' as the specified window in a focal function.      
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' annulus.matrix(5)                   # 5 concentric rings
#' annulus.matrix(5, 3)                # 5 concentric rings with the 3 inner set to 0
#' annulus.matrix(5, 3, null.value=NA) # 5 concentric rings with the 3 inner set to NA
#' annulus.matrix(5, 3, 5)             # 5 rings with 3 inner and 5 outer set to 0
#' annulus.matrix(9, 3, 7)             # 9 rings with 3 inner and 7 outer set to 0
#' 
#' @export annulus.matrix 
annulus.matrix <- function(scale = 3, inner.scale = 0, outer.scale = 0,  
                           null.value = 0) {
  if(scale <= inner.scale) stop("inner scale must be < than scale")
    if(outer.scale > scale) stop("outer scale must be <= than scale")	
  if(scale %% 2 == 0)
     stop("Outer scale must be an odd number") 
  if(inner.scale > 0) {	 
    if(inner.scale %% 2 == 0)
       stop("Inner scale must be an odd number")
  }	
  if(outer.scale > 0) {	 
    if(outer.scale %% 2 == 0)
       stop("Inner scale must be an odd number")
  }	  
  if(inner.scale > 0 & outer.scale > 0) {
    if(!(outer.scale >= inner.scale + 2))
	  stop("outer.scale must be at least two values larger than inner.scale")
  } 	 
    n <- 2 * scale - 1
      m <- diag(n)
      x <- pmax(abs(row(m) - scale), abs(col(m) - scale))
	  if(inner.scale > 0) x[x < inner.scale] <- null.value
      if(outer.scale > 0) x[x <= scale & x >= (outer.scale-1)] <- null.value
	x[x >= 1] <- 1
  return( x )  
}
