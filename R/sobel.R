#' @title Sobel-Feldman operator
#' @description An isotropic image gradient operator using a 3x3 window
#' 
#' @param x            A raster class object
#' @param method       Type of operator ("intensity", "direction", "edge")
#' @param ...          Additional arguments passed to raster::overlay or, 
#'                     if method="edge", raster::focal (if you want a file 
#'                      written to disk use filename = "" argument)
#'
#' @return A raster class object or raster written to disk 
#'
#' @description 
#' The Sobel-Feldmanh operator is a discrete differentiation operator, deriving an 
#' approximation of the gradient of the intensity function. abrupt discontinuity 
#' in the gradient function represents edges, making this a common approach for edge 
#' detection. The Sobel-Feldman operator is based on convolving the image with a small, 
#' separable, and integer matrix in the horizontal and vertical directions. The operator 
#' uses two 3x3 kernels which are convolved with the original image to calculate 
#' approximations of the derivatives - one for horizontal changes, and one for vertical. 
#' Where x is defined here as increasing in the right-direction, and y as increasing in 
#' the down-direction. At each pixel in the raster, the resulting gradient can be combined 
#' to give the gradient intensity, using: SQRT( Gx^2 Gy^2 ). This can be expanded into the 
#' gradient direction using atan(Gx/Gy)
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references
#' Sobel, I., & G. Feldman, (1969) A 3x3 Isotropic Gradient Operator for Image Processing, 
#'   presented at the Stanford Artificial Intelligence Project (SAIL). 
#' 
#' @examples
#' library(raster)
#' r <- brick(system.file("external/rlogo.grd", package="raster")) 
#' s.int <- sobal(r[[1]])
#' s.dir <- sobal(r[[1]], method = "direction")
#' s.edge <- sobal(r[[1]], method = "edge")
#' par(mfrow=c(2,2))
#'   plot(r[[1]])
#'   plot(s.int, main="intensity") 
#'   plot(s.dir, main="direction") 
#'   plot(s.edge, main="edge")
#'   
#' @export sobal
sobal <- function(x, method = "intensity", ...) {
  sobal.y <- matrix( c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow=3, ncol=3)
  sobal.x <- matrix( c(-1, -2, -1, 0, 0, 0, 1, 2, 1), nrow=3, ncol=3)
    if (method == "direction") {
	  Gx = raster::focal(x, w=sobal.x, "sum")
      Gy = raster::focal(x, w=sobal.y, "sum")
      return( raster::overlay(Gy, Gx, fun=atan2, ...) )
    } else if (method == "intensity") {
	  Gx = raster::focal(x, w=sobal.x, "sum")
      Gy = raster::focal(x, w=sobal.y, "sum")
      return( raster::overlay( Gx, Gy, fun=function(x, y) { sqrt( x^2 + y^2) }, ... ) )
    } else {
	return( raster::focal(x, w=(sobal.y / 4), "sum", ...) )
	}
}
