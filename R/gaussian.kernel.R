#' @title Gaussian Kernel
#' @description Creates a Gaussian Kernel of specified size and sigma
#'
#' @param sigma  sigma (standard deviation) of kernel (defaults 2)
#' @param s      scale defining the number of rows and columns for kernel (default 5)
#'
#' @return Symmetrical (NxN) matrix of a Gaussian distribution
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples 
#' opar <- par()
#'   par(mfrow=c(2,2))
#'   persp(gaussian.kernel(sigma=1, s=27), theta = 135, 
#'         phi = 30, col = "grey", ltheta = -120, shade = 0.6, 
#'         border=NA )
#'   persp(gaussian.kernel(sigma=2, s=27), theta = 135, phi = 30,
#'         col = "grey", ltheta = -120, shade = 0.6, border=NA )	
#'   persp(gaussian.kernel(sigma=3, s=27), theta = 135, phi = 30,
#'         col = "grey", ltheta = -120, shade = 0.6, border=NA )	
#'   persp(gaussian.kernel(sigma=4, s=27), theta = 135, phi = 30,
#'         col = "grey", ltheta = -120, shade = 0.6, border=NA )	
#'  par(opar) 			
#' @export
gaussian.kernel <- function(sigma=2, s=5) {
    m <- matrix(ncol=s, nrow=s)
     mcol <- rep(1:s, s)
     mrow <- rep(1:s, each=s)
       x <- mcol - ceiling(s/2)
       y <- mrow - ceiling(s/2)
     m[cbind(mrow, mcol)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
   m / sum(m)
}
