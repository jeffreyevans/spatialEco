#' @title Nth values
#' @description Returns the Nth highest or lowest values in a vector
#' 
#' @param x           Numeric vector
#' @param N           Number of (Nth) values returned
#' @param smallest    (FALSE/TRUE) Return the highest, else smallest values
#'
#' @details 
#' This function returns n lowest or highest elements in a vector
#'
#' @return Numeric vector of Nth values 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' nth.values(1:20, N=3, smallest = TRUE)                 
#' nth.values(1:20, N=3)
#' 
#' @export nth.values
nth.values <- function(x, N=2, smallest = FALSE) {
  if(!is.numeric(x))
    stop("x must be a numeric vector")
  if(N >= length(x))
    stop("N cannot be larger than length-1 of the vector")
  if(smallest) {
    x[order(x)[1:N]]
  } else {
    sort(x[order(x, decreasing = T)[1:N]])
  }
}
