#' @title Kendall tau trend with continuity correction for raster time-series 
#' @description Calculates a nonparametric statistic for a monotonic trend 
#'              based on the Kendall tau statistic and the Theil-Sen slope 
#'              modification
#'
#' @param x             A multiband terra SpatRaster object with at least 5 layers
#' @param tau           (FALSE/TRUE) return a raster with the pixel wise 
#'                       tau correlation values
#' @param intercept     (FALSE/TRUE) return a raster with the pixel 
#'                       wise intercept values 
#' @param p.value       (FALSE/TRUE) return a raster with the pixel 
#'                       wise p.values
#' @param confidence    (FALSE/TRUE) return a raster with the pixel 
#'                       wise 95 pct confidence levels
#' @param min.obs       The threshold of minimum number of observations (default 6)
#' @param method        Kendall method to use c("zhang", "yuepilon","none"), see kendall function
#' @param ...           Additional arguments passed to the terra app function 
#'
#' @details 
#' This function implements Kendall's nonparametric test for a monotonic trend 
#' using the Theil-Sen (Theil 1950; Sen 1968; Siegel 1982) method to estimate 
#' the slope and related confidence intervals.  
#'
#' @return Depending on arguments, a raster layer or rasterBrick object containing:
#'   * raster layer 1 - slope for trend, always returned
#'   * raster layer 2 - Kendall's tau two-sided test, reject null at 0, if tau TRUE
#'   * raster layer 3 - intercept for trend if intercept TRUE
#'   * raster layer 4 - p value for trend fit if p.value TRUE
#'   * raster layer 5 - lower confidence level at 95 pct, if confidence TRUE
#'   * raster layer 6 - upper confidence level at 95 pct, if confidence TRUE
#' @md
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Theil, H. (1950) A rank invariant method for linear and polynomial regression 
#'   analysis.  Nederl. Akad. Wetensch. Proc. Ser. A 53:386-392 (Part I), 
#'   53:521-525 (Part II), 53:1397-1412 (Part III).
#'  
#' Sen, P.K. (1968) Estimates of Regression Coefficient Based on Kendall's tau. 
#'   Journal of the American Statistical Association. 63(324):1379-1389.
#'  
#' Siegel, A.F. (1982) Robust Regression Using Repeated Medians. 
#'   Biometrika, 69(1):242-244
#'
#' @examples
#' \donttest{
#'  library(terra)
#'
#'  # note; nonsense example with n=9
#'  r <- c(rast(system.file("ex/logo.tif", package="terra")),
#'         rast(system.file("ex/logo.tif", package="terra")),
#'         rast(system.file("ex/logo.tif", package="terra"))) 
#'  
#'  # Calculate trend slope with p-value and confidence level(s)
#'  # ("slope","intercept", "p.value","z.value", "LCI","UCI","tau")
#'    k <- raster.kendall(r, method="none")
#'    plot(k)
#' }
#'
#' @seealso \code{\link[zyp]{zyp.trend.vector}} for model details
#' @seealso \code{\link[terra]{app}} for available ... arguments
#'
#' @export
raster.kendall <- function(x, intercept = TRUE, p.value = TRUE, 
                           confidence = TRUE, tau = TRUE, min.obs = 6,  
						   method=c("zhang", "yuepilon","none"), ...) {
  if(length(find.package("zyp", quiet = TRUE)) == 0)
    stop("please install zyp package before running this function")
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if(min.obs < 6)
    warning("Setting the time-series threshold (n) to fewer than 6 obs may invalidate 
	  the statistic and <= 4 will result in NA's") 
  if( terra::nlyr(x) < min.obs) 
    stop("Too few layers to calculate a trend")
  idx <- which(c(TRUE, tau, intercept, p.value, rep(confidence,2)))	
  out.names <- c("slope", "tau", "intercept", "p-value", "limits.LCL", "limits.UCL")[idx]
    trend.slope <- function(y, metrics=idx, m=method[1]) {
      kendall(y, method = m, threshold = min.obs)[metrics]
    }
  k <- terra::app(x, fun=trend.slope, ...)
    names(k) <- out.names
  return( k )
}
