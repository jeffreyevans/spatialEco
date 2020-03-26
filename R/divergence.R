#' @title divergence
#' @description Kullback-Leibler Divergence (Cross-entropy) 
#'       
#' @param x     a vector of integer values, defining observed
#' @param y     a vector of integer values, defining estimates
#' @param type  Type of divergence statistic c("Kullback-Leibler", 
#'              "cross-entropy")
#' 
#' @return single value vector with divergence statistic
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples
#' x <- round(runif(10,1,4),0)
#' y <- round(runif(10,1,4),0)
#'
#' divergence(x, y) 
#' divergence(x, y, type = "cross-entropy") 
#'
#' @export divergence
divergence <- function(x, y, type = c("Kullback-Leibler", "cross-entropy")) {
  type = type[1]
  if(!is.vector(x) | !is.vector(y))
    stop("x and y must be numeric of character vectors")
  if(any(type %in% c("Kullback-Leibler", "cross-entropy")==FALSE))
    stop("Not a valid option for statistic type")
  q <- table(x) / sum(table(x))  # observed or approximated
  p <- table(y) / sum(table(y))  # estimated or probability 
  classes <- intersect(names(q), names(p))
    p <- p[which(names(p) %in% classes)]
    q <- q[which(names(q) %in% classes)]
  if(type == "cross-entropy") {
    message("Cross-Entropy", "\n")
    return( -sum( q, log(p) ) ) 
  } else if(type == "Kullback-Leibler") {
    message("Kullback-Leibler", "\n")
    return( sum( p * log(p / q) ) )
  }
}  
