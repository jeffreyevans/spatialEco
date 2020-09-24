#' @title spectral separability
#' @description Calculates spectral separability by class  
#'           
#' @param x                   data.frame, matrix or vector of spectral values must,
#'                              match classes defined in y
#' @param y                   A vector or factor with grouping classes, must match
#'                              row wise values in x
#' @param jeffries.matusita   (TRUE/FALSE) Return J-M distance (default) else Bhattacharyya  
#'
#' @return A matrix of class-wise Jeffries-Matusita or Bhattacharyya distance 
#'         separability values
#'
#' @description
#' Available statistics:
#' * Bhattacharyya distance (Bhattacharyya 1943; Harold 2003) measures the similarity
#'   of two discrete or continuous probability distributions.
#'
#' * Jeffries-Matusita (default) distance (Bruzzone et al., 2005; Swain et al., 1971)  
#'   is a scaled (0-2) version of Bhattacharyya. The J-M distance is asymptotic to 2, 
#'   where 2 suggest complete separability.
#' @md 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Bhattacharyya, A. (1943) On a measure of divergence between two statistical 
#'   populations defined by their probability distributions'. Bulletin of the 
#'   Calcutta Mathematical Society 35:99-109
#' @references
#' Bruzzone, L., F. Roli, S.B. Serpico (1995) An extension to multiclass cases of 
#'   the Jefferys-Matusita distance. IEEE Transactions on Pattern Analysis and  
#'   Machine Intelligence 33:1318-1321
#' @references
#' Kailath, T., (1967) The Divergence and Bhattacharyya measures in signal  
#'   selection. IEEE Transactions on Communication Theory 15:52-60  
#' 
#' @examples 
#'#' # Create example data 
#' require(MASS)                
#' d <- 6                 # Number of bands
#' n.class <- 5           # Number of classes
#' n <- rep(1000, 5)                
#' mu <- round(matrix(rnorm(d*n.class, 128, 1), 
#'             ncol=n.class, byrow=TRUE), 0)
#' 
#' x <- matrix(double(), ncol=d, nrow=0)
#'   classes <- integer()
#'     for (i in 1:n.class) {
#'       f <- svd(matrix(rnorm(d^2), ncol=d))
#'       sigma <- t(f$v) %*% diag(rep(10, d)) %*% f$v
#'       x <- rbind(x, mvrnorm(n[i], mu[, i], sigma))
#'       classes <- c(classes, rep(i, n[i]))
#'     }
#' colnames(x) <- paste0("band", 1:6)
#' classes <- factor(classes, labels=c("water", "forest",
#'                   "shrub", "urban", "ag"))
#' 
#' # Separability for multi-band (multivariate) spectra 
#' spectral.separability(x, classes)
#'
#' # Separability for single-band (univariate) spectra 
#' spectral.separability(x[,1], classes)
#'
#' @export spectral.separability 
spectral.separability <- function(x, y, jeffries.matusita = TRUE) {
  if(!any(class(x)[1] == c("data.frame", "matrix", "numeric")))
    stop("x must be a matrix, data.frame or vector")
  if(!any(class(y)[1] == c("numeric", "character", "factor")))
    stop("y must be a vector")
  if(length(unique(y)) < 2) 
    stop("There are not multiple classes to test")  
  if(any(class(x)[1] == c("numeric")))
    x <- matrix(x, ncol=1)
  if(nrow(x) != length(y))
    stop("x and y do not match")	
  if(class(y)[1] == "factor") {
    cls <- levels(y) 
  } else {
    cls <- unique(y) 
  }  
  bhattacharyya <- function(m1, s1, m2, s2, jm = FALSE) {
    mahalanobis <- function(m1, m2, sigma) {
      m <- m1 - m2; m %*% solve(sigma, m)
    }
    d <- function(u) determinant(u, logarithm=TRUE)$modulus 
    s <- (s1 + s2)/2                           
    b <- mahalanobis(m1, m2, s)/8 + (d(s) - d(s1)/2 - d(s2)/2)/2
    if(jm)
      b <- sqrt(2*(1-exp(-b)))
    return( b ) 
  } 
  message("Calculating distances for ", length(cls), " classes and ", ncol(x), " spectra" )
  class.stats <- by(x, y, function(y) list(mean=apply(y, 2, mean), cov=stats::cov(y)))
  distances <- matrix(0.0, length(cls), length(cls))
    for (i in 2:length(cls)) {
      m1 <- class.stats[[i]]$mean
      s1 <- class.stats[[i]]$cov
        for (j in 1:(i-1)) {
          m2 <- class.stats[[j]]$mean
    	  s2 <- class.stats[[j]]$cov
          distances[i,j] <- distances[j,i] <- bhattacharyya(m1,s1,m2,s2,jm=jeffries.matusita)
        }
    }
	dimnames(distances) <- list(cls, cls) 
  return(distances)
}
