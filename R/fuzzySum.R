#' @title Fuzzy Sum
#' @description Calculates the fuzzy sum of a vector
#' 
#' @param x       Vector of values to apply fuzzy sum 
#' 
#' @return Value of fuzzy sum
#'
#' @note The fuzzy sum is an increasing linear combination of values. This can be used to sum probabilities or results of multiple density functions. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples 
#' p = c(0.8,0.76,0.87)
#'   fuzzySum(p)
#'   sum(p)
#' 
#' p = c(0.3,0.2,0.1)
#'   fuzzySum(p)
#'   sum(p)  
#'
#' @export  
fuzzySum <- function(x) { return( 1 - prod( (1 - x) ) ) }
