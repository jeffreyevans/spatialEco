#' @title Gaussian Kernel
#' @description Creates a Gaussian Kernel of specified size and sigma
#'
#' @param sigma          sigma (standard deviation) of kernel (defaults 2)
#' @param n              size of symmetrical kernel (defaults to 5x5)
#'
#' @return Symmetrical (NxN) matrix of a Gaussian distribution
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples 
#'   par(mfrow=c(2,2))
#'   persp(gaussian.kernel(sigma=1, n=27), theta = 135, phi = 30, col = "grey", 
#'         ltheta = -120, shade = 0.6, border=NA )
#'   persp(gaussian.kernel(sigma=2, n=27), theta = 135, phi = 30, col = "grey", 
#'         ltheta = -120, shade = 0.6, border=NA )		
#'   persp(gaussian.kernel(sigma=3, n=27), theta = 135, phi = 30, col = "grey", 
#'         ltheta = -120, shade = 0.6, border=NA )				
#'   persp(gaussian.kernel(sigma=4, n=27), theta = 135, phi = 30, col = "grey", 
#'         ltheta = -120, shade = 0.6, border=NA )					
#'			
#' @export
gaussian.kernel <- function(sigma=2, n=5) {
   m <- matrix(ncol=n, nrow=n)
     mcol <- rep(1:n, n)
     mrow <- rep(1:n, each=n)
       x <- mcol - ceiling(n/2)
       y <- mrow - ceiling(n/2)
     m[cbind(mrow, mcol)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
   m / sum(m)
}
