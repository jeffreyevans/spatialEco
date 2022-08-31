#' @title Gaussian Kernel
#' @description Creates a Gaussian Kernel of specified size and sigma
#'
#' @param sigma  sigma (standard deviation) of kernel (defaults 2)
#' @param nr      number of rows for kernel (defaults to 5)
#' @param nc      number of columns for kernel (defaults to same as nr)
#'
#' @return Symmetrical (NxN) matrix of a Gaussian distribution
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @examples 
#'   par(mfrow=c(2,2))
#'   persp(gaussian.kernel(sigma=1, nr=27), theta = 135, 
#'         phi = 30, col = "grey", ltheta = -120, shade = 0.6, 
#'         border=NA )
#'   persp(gaussian.kernel(sigma=2, nr=27), theta = 135, phi = 30,
#'         col = "grey", ltheta = -120, shade = 0.6, border=NA )	
#'   persp(gaussian.kernel(sigma=3, nr=27), theta = 135, phi = 30,
#'         col = "grey", ltheta = -120, shade = 0.6, border=NA )	
#'   persp(gaussian.kernel(sigma=4, nr=27), theta = 135, phi = 30,
#'         col = "grey", ltheta = -120, shade = 0.6, border=NA )	
#'			
#' @export
gaussian.kernel <- function(sigma=2, nr=5, nc=NULL) {
  if(is.null(nc)) nc = nr
   m <- matrix(ncol=nc, nrow=nr)
     mcol <- rep(1:nc, nc)
     mrow <- rep(1:nr, each=nr)
       x <- mcol - ceiling(nc/2)
       y <- mrow - ceiling(nr/2)
     m[cbind(mrow, mcol)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
   m / sum(m)
}
