#' @title Kendall tau trend with continuity correction for raster time-series 
#' @description Calculates a nonparametric statistic for a monotonic trend based on the Kendall tau statistic and the Theil-Sen slope modification
#'
#' @param x             A rasterStack object with at least 5 layers
#' @param intercept     (FALSE/TRUE) return a raster with the pixel wise intercept values 
#' @param p.value       (FALSE/TRUE) return a raster with the pixel wise p.values
#' @param confidence    (FALSE/TRUE) return a raster with the pixel wise 95 pct confidence levels
#' @param tau           (FALSE/TRUE) return a raster with the pixel wise tau values
#' @param ...           Additional arguments passed to the raster overlay function
#'
#' @return Depending on arguments, a raster layer or rasterBrick object containing:
#' \itemize{
#'   \item {raster layer 1} {slope for trend, always returned}
#'   \item {raster layer 2} {Kendall's tau two-sided test, if tau TRUE}
#'   \item {raster layer 3} {intercept for trend if intercept TRUE}
#'   \item {raster layer 4} {p value for trend fit if p.value TRUE}
#'   \item {raster layer 5} {Z value for trend fit if z.value TRUE, }
#'   \item {raster layer 6} {lower confidence level at 95 pct, if confidence TRUE}
#'   \item {raster layer 7} {upper confidence level at 95 pct, if confidence TRUE}
#' }
#'
#' @details This function implements Kendall's nonparametric test for a monotonic trend using the Theil-Sen (Theil 1950; Sen 1968; Siegel 1982) method to estimate the slope and related confidence intervals.  
#' @details Critical values are Z > 1.96 represents a significant increasing trend and a Z < −1.96 a significant decreasing trend (p < 0.05). The null hypothesis can be rejected if Tau = 0. 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Theil, H. (1950) A rank invariant method for linear and polynomial regression analysis.  Nederl. Akad. Wetensch. Proc. Ser. A 53:386-392 (Part I), 53:521-525 (Part II), 53:1397-1412 (Part III).
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
#'  logo.trend <- raster.kendall(r.logo, tau = TRUE, intercept = TRUE,  p.value = TRUE, 
#'                               z.value = FALSE, confidence = TRUE)
#'    names(logo.trend) <- c("slope","tau", "intercept", "p.value", "z.value", "LCI", "UCI")
#'      plot(logo.trend)
#' }
#'
#' @seealso \code{\link[EnvStats]{kendallTrendTest}} for model details
#' @seealso \code{\link[raster]{overlay}} for available ... arguments
#'
#' @export
raster.kendall <- function(x, tau = FALSE, intercept = FALSE,  p.value = FALSE,    
                           z.value = FALSE, confidence = FALSE,  ...) {
  if(!any(class(x) %in% c("RasterBrick","RasterStack"))) stop("x is not a raster stack or brick object")
    if( raster::nlayers(x) < 5) stop("Too few layers (n<5) to calculate a trend")
  trend.slope <- function(y, p.value.pass = p.value, z.pass = z.value, tau.pass = tau, 
                          confidence.pass = confidence, intercept.pass = intercept) {
    options(warn=-1)
    fit <- EnvStats::kendallTrendTest(y ~ 1)
      fit.results <- fit$estimate[2]
	    if(tau.pass == TRUE) { fit.results <- c(fit.results, fit$estimate[1]) }
		  if(intercept.pass == TRUE) { fit.results <- c(fit.results, fit$estimate[3]) }  
        if(p.value.pass == TRUE) { fit.results <- c(fit.results, fit$p.value) } 
		  if(z.pass == TRUE) { fit.results <- c(fit.results, fit$statistic) }
  	       if(confidence.pass == TRUE) { 
		     ci <- unlist(fit$interval["limits"])
		       if( length(ci) == 2) { 
		         fit.results <- c(fit.results, ci)
               } else {
                 fit.results <- c(fit.results, c(NA,NA))
               }			  
		   }   		  
    return( fit.results )
  }
    options(warn=0)
  return( raster::overlay(x, fun=trend.slope, ...) )
}

