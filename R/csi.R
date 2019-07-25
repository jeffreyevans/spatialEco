#' @title Cosine Similarity Index 
#' @description Calculates the cosine similarity and angular similarity on two 
#'              vectors or a matrix
#' 
#' @param x A vector or matrix object 
#' @param y If x is a vector, then a vector object 
#'
#' @return If x is a matrix, a list object with: similarity and angular.similarity matrices 
#' @return If x and y are vectors, a vector of similarity and angular.similarity   
#'
#' @note The cosine similarity index is a measure of similarity between two vectors of an 
#' inner product space. This index is bested suited for high-dimensional positive variable 
#' space. One useful application of the index is to measure separability of clusters derived 
#' from algorithmic approaches (e.g., k-means). It is a good common practice to center the data before calculating the index. It should be noted that the cosine similarity index is mathematically, and often numerically, equivalent to the Pearson's correlation coefficient   
#'
#' @note cosine similarity index is derived: 
#' @note s(xy) = x * y / ||x|| * ||y||
#' @note expected 1.0 (perfect similarity) to -1.0 (perfect dissimilarity) 
#' @note A normalized angle between the vectors can be used as a bounded similarity 
#' function within [0,1]
#' @note angular similarity  = 1 - (cos(s)^-1/pi)
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' # Compare two vectors (centered using scale)
#'   x=runif(100)
#'   y=runif(100)^2
#'   csi(as.vector(scale(x)),as.vector(scale(y)))
#'   
#'   #' # Compare columns (vectors) in a matrix (centered using scale)
#'   x <- matrix(round(runif(100),0),nrow=20,ncol=5)
#'   ( s <- csi(scale(x)) )
#'     
#' # Compare vector (x) to each column in a matrix (y)
#' y <- matrix(round(runif(500),3),nrow=100,ncol=5)
#' x=runif(100) 
#' csi(as.vector(scale(x)),scale(y))
#'
#' @export   
csi <- function (x, y = NULL) {
    if (is.matrix(x) && is.null(y)) {
      s = array(0, c(ncol(x), ncol(x)))
      f = colnames(x)
      dimnames(s) = list(f, f)
        for (i in 2:ncol(x)) {
          for (j in 1:(i - 1)) { s[i, j] = csi(x[, i], x[, j])[1] }
        }
      s = s + t(s)
	  acs  <- 1 - (cos(s)^ -1 / pi)
	  diag(s) = 1
      diag(acs) = 1
      return(list(similarity=s, angular.similarity=acs))
    }
    else if (is.vector(x) && is.vector(y)) {
      cs <- crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
	  acs <- 1 - (cos(cs)^ -1 / pi)
	  s <- c(cs,acs)
	  names(s) <- c("similarity", "angular.similarity")
	  return(s)
    }
    else if (is.vector(x) && is.matrix(y)) {
      s = vector(mode = "numeric", length = ncol(y))
      names(s) = colnames(y)
        for (i in 1:ncol(y)) { s[i] = csi(x, y[, i])[1] }
	  acs  <- 1 - (cos(s)^ -1 / pi)
	  s <- rbind(s,acs)
	  rownames(s) <-c("similarity", "angular.similarity")
      return(s)
    } else {
      stop("Error: function supports two vectors or single matrix")
    }
  }
