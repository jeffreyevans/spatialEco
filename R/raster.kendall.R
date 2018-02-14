#' @title Kendall's tau trend with continuity correction for raster time-series 
#' @description Calculates a nonparametric statistic for a monotonic trend based on Kendall's tau statistic and the Theil-Sen slope modification
#'
#' @param x             A rasterStack object with at least 5 layers
#' @param intercept     (FALSE/TRUE) return a raster with the pixel-wise intercept values 
#' @param p.value       (FALSE/TRUE) return a raster with the pixel-wise p.values
#' @param confidence    (FALSE/TRUE) return a raster with the pixel-wise +/- 95% confidence levels
#' @param tau           (FALSE/TRUE) return a raster with the pixel-wise tau values
#' @param ...           Additional arguments passed to the raster overlay function
#'
#' @return Depending on arguments, a raster layer or rasterBrick object
#' \itemize{  
#'   \item slope trend (always returned)
#'   \item intercept (if intercept = TRUE) 
#'   \item p-value (if p.value = TRUE) 
#'   \item lower confidence level at 95% (if confidence = TRUE) 
#'   \item upper confidence level at 95% (if confidence = TRUE) 
#'   \item Kendall's tau (two-sided test, reject null at 0) (if tau = TRUE) 
#' }
#'
#' @details This function implements Kendall's nonparametric test for a monotonic trend using the Theil-Sen (Theil 1950; Sen 1968; Siegel 1982) method to estimate the slope and related confidence intervals.  
#' 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Theil, H. (1950) A rank invariant method for linear and polynomial regression analysis.  Nederl. Akad. Wetensch. Proc. Ser. A 53, 386-392 (Part I), 521-525 (Part II), 1397-1412 (Part III).
#' @references Sen, P.K. (1968) Estimates of Regression Coefficient Based on Kendall's tau. Journal of the American Statistical Association. 63(324):1379-1389.
#' @references Siegel, A.F. (1982) Robust Regression Using Repeated Medians. Biometrika, 69(1):242-244
#'
#' @examples
#' \dontrun{
#'  library(raster)
#'  r.logo <- stack(system.file("external/rlogo.grd", package="raster"),
#'                  system.file("external/rlogo.grd", package="raster"),
#'  			    system.file("external/rlogo.grd", package="raster")) 
#'  
#'  # Calculate trend slope with p-value and confidence level(s)
#'  logo.trend <- raster.kendall(r.logo, p.value=TRUE, confidence=TRUE)
#'    names(logo.trend) <- c("slope","p.value","LCI","UCI")
#'      plot(logo.trend)
#' }
#'
#' @seealso \code{\link[EnvStats]{kendallTrendTest}} for model details
#' @seealso \code{\link[raster]{overlay}} for available ... arguments
#'
#' @export
raster.kendall <- function(x, intercept = FALSE, p.value = FALSE,   
                           confidence = FALSE, tau = FALSE, ...) {
  if(!any(class(x) %in% c("RasterBrick","RasterStack"))) stop("x is not a raster stack or brick object")
    if( nlayers(x) < 5) stop("Too few layers (n<5) to calculate a trend")
  trend.slope <- function(y, p.value.pass = p.value, tau.pass = tau, confidence.pass = confidence,
                          intercept.pass = intercept) {
    options(warn=-1)
    fit <- EnvStats::kendallTrendTest(y ~ 1)
      fit.results <- fit$estimate[2]
        if(p.value.pass == TRUE) { fit.results <- c(fit.results, fit$p.value) } 
  	      if(confidence.pass == TRUE) { 
		    ci <- unlist(fit$interval["limits"])
		      if( length(ci) == 2) { 
		        fit.results <- c(fit.results, ci)
              } else {
                fit.results <- c(fit.results, c(NA,NA))
              }			  
		  }
        if(intercept.pass == TRUE) { fit.results <- c(fit.results, fit$estimate[3]) }  
		  if(tau.pass == TRUE) { fit.results <- c(fit.results, fit$estimate[1]) }  
    return( fit.results )
  }
    options(warn=0)
  return( raster::overlay(x, fun=trend.slope, ...) )
}
