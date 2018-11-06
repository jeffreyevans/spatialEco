#' @title Savitzky-Golay smoothing filter
#' @description Smoothing of time-series data using Savitzky-Golay convolution smoothing 
#'
#' @param x      A vector to be smoothed  
#' @param f      Filter type (default 4 for quartic, specify 2 for quadratic)  
#' @param l      Convolution filter length, must be odd number (default 51). Defines degree of smoothing
#' @param d      First derivative (default 1)
#' @param na.rm  NA behavior
#' @param ...    not used
#'
#' @return  A vector of the smoothed data equal to length of x. Please note; NA values are retained
#'
#' @author Jeffrey S. Evans    <jeffrey_evans<at>tnc.org>
#'
#' @references Savitzky, A., & Golay, M.J.E. (1964). Smoothing and Differentiation of Data by 
#'               Simplified Least Squares Procedures. Analytical Chemistry. 36(8):1627-39
#'
#' @examples 
#'   y <- c(0.112220988, 0.055554941, 0.013333187, 0.055554941, 0.063332640, 0.014444285, 
#'          0.015555384, 0.057777140, 0.059999339, 0.034444068, 0.058888242, 0.136665165, 
#'          0.038888458, 0.096665606,0.141109571, 0.015555384, 0.012222088, 0.012222088, 
#'          0.072221428, 0.052221648, 0.087776810,0.014444285, 0.033332966, 0.012222088, 
#'          0.032221869, 0.059999339, 0.011110989, 0.011110989,0.042221759, 0.029999670, 
#'          0.018888680, 0.098887801, 0.016666483, 0.031110767, 0.061110441,0.022221979, 
#'          0.073332526, 0.012222088, 0.016666483, 0.012222088, 0.122220881, 0.134442955, 
#'          0.094443403, 0.128887475, 0.045555055, 0.152220547, 0.071110331, 0.018888680,
#'          0.022221979, 0.029999670, 0.035555165, 0.014444285, 0.049999449, 0.074443623, 
#'          0.068888135, 0.062221535, 0.032221869, 0.095554501, 0.143331751, 0.121109776,
#'          0.065554835, 0.074443623, 0.043332856, 0.017777583, 0.016666483, 0.036666263, 
#'          0.152220547, 0.032221869, 0.009999890, 0.009999890, 0.021110879, 0.025555275,
#'          0.099998899, 0.015555384, 0.086665712, 0.008888791, 0.062221535, 0.044443958, 
#'          0.081110224, 0.015555384, 0.089999005, 0.082221314, 0.056666043, 0.013333187,
#'          0.048888352, 0.075554721, 0.025555275, 0.056666043, 0.146665052, 0.118887581, 
#'          0.125554174, 0.024444176, 0.124443069, 0.012222088, 0.126665279, 0.048888352,
#'          0.046666153, 0.141109571, 0.015555384, 0.114443190)
#'   
#'   plot(y, type="l", lty = 3, main="Savitzky-Golay with l = 51, 25, 10")
#'     lines(sg.smooth(y),col="red", lwd=2)
#'     lines(sg.smooth(y, l = 25),col="blue", lwd=2)
#'     lines(sg.smooth(y, l = 10),col="green", lwd=2)
#'   
#' @export
sg.smooth <- function(x, f = 4, l = 51, d = 1, na..rm, ...) {
  na.idx <- which(is.na(x))
    x <- stats::na.omit(x)
  fc <- (l-1)/2                       
    X  <- outer(-fc:fc, 0:f, FUN="^") 
      s <- svd(X)
      Y  <- s$v %*% diag(1/s$d) %*% t(s$u)
    T2 <- stats::convolve(x, rev(Y[d,]), type="o")
    sg <- T2[(fc+1):(length(T2)-fc)]
  if(length(na.idx) > 0) { sg <- spatialEco::insert.values(sg, NA, na.idx) }	
  return( sg ) 
}
