#' @title Kendall tau trend with continuity correction for raster time-series 
#' @description Calculates a nonparametric statistic for a monotonic trend based on the Kendall 
#'                tau statistic and the Theil-Sen slope modification
#'
#' @param x                A rasterStack object with at least 8 layers
#' @param tau              (FALSE/TRUE) return a raster with the pixel wise tau values
#' @param intercept        (FALSE/TRUE) return a raster with the pixel wise intercept values 
#' @param p.value          (FALSE/TRUE) return a raster with the pixel wise p.values
#' @param z.value          (FALSE/TRUE) return a raster with the pixel wise z values
#' @param confidence       (FALSE/TRUE) return a raster with the pixel wise 95 pct confidence levels
#' @param autocorrelation  (FALSE/TRUE) Apply autocorrelation correction using prewhitening 
#' @param ...              Additional arguments passed to the raster overlay function
#'
#' @return Depending on arguments, a raster layer or rasterBrick object containing:
#' \itemize{
#'   \item {raster layer 1} {Theil-Sen slope, always returned}
#'   \item {raster layer 2} {Kendall's tau two-sided test, if tau TRUE}
#'   \item {raster layer 3} {intercept for trend if intercept TRUE, not if autocorrelation}
#'   \item {raster layer 4} {p value for trend fit if p.value TRUE}
#'   \item {raster layer 5} {Z value for trend fit if z.value TRUE}
#'   \item {raster layer 6} {lower confidence level at 95-pct if confidence TRUE, not if autocorrelation}
#'   \item {raster layer 7} {upper confidence level at 95-pct if confidence TRUE, not if autocorrelation}
#' }
#'
#' @details This function implements Kendall's nonparametric test for a monotonic trend using the 
#'          Theil-Sen (Theil 1950; Sen 1968; Siegel 1982) method to estimate the slope and related 
#'          confidence intervals. Critical values are Z > 1.96 representing a significant increasing 
#'          trend and a Z < -1.96 a significant decreasing trend (p < 0.05). The null hypothesis can 
#'          be rejected if Tau = 0. There is also an option for autocorrelation correction using
#'          the method proposed in Yue & Wang (2002). 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Theil, H. (1950) A rank invariant method for linear and polynomial regression analysis.  
#'               Nederl. Akad. Wetensch. Proc. Ser. A 53:386-392 (Part I),  
#'               53:521-525 (Part II), 53:1397-1412 (Part III).
#' @references Sen, P.K. (1968) Estimates of Regression Coefficient Based on Kendall's tau. 
#'               Journal of the American Statistical Association. 63(324):1379-1389.
#' @references Siegel, A.F. (1982) Robust Regression Using Repeated Medians. Biometrika, 69(1):242-244
#' @references Yue, S., & Wang, C. Y. (2002). Applicability of prewhitening to eliminate the influence 
#'               of serial correlation on the Mann-Kendall test. Water Resources Research, 38(6):41-47. 
#'
#' @examples
#' \dontrun{
  library(raster)
  r.logo <- stack(system.file("external/rlogo.grd", package="raster"),
                  system.file("external/rlogo.grd", package="raster"),
  			      system.file("external/rlogo.grd", package="raster")) 
  
  # Calculate trend slope with p-value and confidence level(s)
  logo.trend <- raster.kendall(r.logo, tau = TRUE, intercept = TRUE,  p.value = TRUE,
                               z.value = TRUE, confidence = TRUE, autocorrelation = TRUE)
    names(logo.trend) <- c("slope","tau", "intercept", "p.value", "z.value", "LCI", "UCI")
      plot(logo.trend)
	  	  	  
#' }
#'
#' @seealso \code{\link[raster]{overlay}} for available ... arguments
#'
#' @export raster.kendall
raster.kendall <- function(x, tau = FALSE, intercept = FALSE,  p.value = FALSE,    
                           z.value = FALSE, confidence = FALSE,  
						   autocorrelation = FALSE, ...) {
	knames <- c("slope","tau", "intercept", "p.value", "z.value", "LCI", "UCI")
	if(!any(class(x) %in% c("RasterBrick","RasterStack")))
	  stop("x is not a raster stack or brick object")
	if( raster::nlayers(x) < 8) 
	  stop("Too few layers (n < 8) to calculate a trend")
	if(autocorrelation == TRUE) {
	  knames <- knames[-c(3,6,7)]					   
      message("Please note that with autocorrelation correction only the:
        Sen Slope, Tau, p-value and z-value outputs are available")	
	  intercept = FALSE
	  confidence = FALSE
	}
	a <- c(tau = tau, intercept = intercept,  p.value = p.value,    
              z.value = z.value, confidence = confidence,  
		      prewhiten = autocorrelation)			  
	  message(cat("Outputting:", knames, "\n"))
    tslp <- function(x) { kendall(x, tau = a[1], intercept = a[2],  
	                 p.value = a[3], z.value = a[4], 
			confidence = a[5], prewhiten = a[6]) }
  return( raster::overlay(x, fun = tslp, ...) )  
}
