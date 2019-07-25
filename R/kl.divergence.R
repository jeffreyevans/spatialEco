#' @title Kullback-Leibler divergence (relative entropy)
#'
#' @description Calculates the Kullback-Leibler divergence (relative entropy) between 
#'              unweighted theoretical component distributions. Divergence is calculated as: 
#'              int [f(x) (log f(x) - log g(x)) dx] for distributions with densities f() and g(). 
#' 
#' @param object  Matrix or dataframe object with >=2 columns
#' @param eps  Probabilities below this threshold are replaced by this threshold for numerical stability.
#' @param overlap  Logical, do not determine the KL divergence for those pairs where for 
#'                   each point at least one of the densities has a value smaller than eps.
#'
#' @return pairwise Kullback-Leibler divergence index (matrix)   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Kullback S., and R. A. Leibler (1951) On information and sufficiency. The Annals of 
#'             Mathematical Statistics 22(1):79-86
#'        
#' @examples 
#' x <- seq(-3, 3, length=200)
#' y <- cbind(n=dnorm(x), t=dt(x, df=10))
#'   matplot(x, y, type='l')
#'     kl.divergence(y)
#'    
#' # extract value for last column
#'   kl.divergence(y[,1:2])[3:3]
#'
#' @export  
kl.divergence <- function(object, eps = 10^-4, overlap = TRUE) {
    if (!is.numeric(object)) 
        stop("object must be a numeric matrix\n")
    z <- matrix(NA, nrow = ncol(object), ncol = ncol(object))
    colnames(z) <- rownames(z) <- colnames(object)
    w <- object < eps
    if (any(w)) 
        object[w] <- eps
    object <- sweep(object, 2, colSums(object), "/")
    for (k in seq_len(ncol(object) - 1)) {
      for (l in 2:ncol(object)) {
        ok <- (object[, k] > eps) & (object[, l] > eps)
          if (!overlap | any(ok)) {
              z[k, l] <- sum(object[, k] * (log(object[, k]) - log(object[, l])))
              z[l, k] <- sum(object[, l] * (log(object[, l]) - log(object[, k])))
          }
      }
    }
    diag(z) <- 0
    return(z)
} 
