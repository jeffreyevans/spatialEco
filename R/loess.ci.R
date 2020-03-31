#' @title Loess with confidence intervals
#' @description Calculates a local polynomial regression fit 
#'              with associated confidence intervals
#' 
#' @param y Dependent variable, vector 
#' @param x Independent variable, vector 
#' @param plot Plot the fit and confidence intervals
#' @param p Percent confidence intervals (default is 0.95)
#' @param ... Arguments passed to loess 
#' 
#' @return A list object with:
#' * loess Predicted values   
#' * se Estimated standard error for each predicted value
#' * lci Lower confidence interval 
#' * uci Upper confidence interval
#' * df Estimated degrees of freedom 
#' * rs Residual scale of residuals used in computing the 
#'            standard errors 
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' W. S. Cleveland, E. Grosse and W. M. Shyu (1992) Local regression models. 
#'   Chapter 8 of Statistical Models in S eds J.M. Chambers and T.J. Hastie, 
#'   Wadsworth & Brooks/Cole. 
#' 
#' @examples 
#'  x <- seq(-20, 20, 0.1)
#'  y <- sin(x)/x + rnorm(length(x), sd=0.03)
#'  p <- which(y == "NaN")
#'    y <- y[-p]	
#'    x <- x[-p]
#'  
#' opar <- par(no.readonly=TRUE)
#'   par(mfrow=c(2,2))  
#'     lci <- loess.ci(y, x, plot=TRUE, span=0.10)
#'     lci <- loess.ci(y, x, plot=TRUE, span=0.30)
#'     lci <- loess.ci(y, x, plot=TRUE, span=0.50)
#'     lci <- loess.ci(y, x, plot=TRUE, span=0.80)
#' par(opar)
#'
#' @export
loess.ci <- function(y, x, p=0.95, plot=FALSE, ...) {
  plx <- stats::predict(stats::loess(y ~ x, ...), se=TRUE)
  lci = plx$fit - stats::qt(p, plx$df) * plx$se.fit
  uci = plx$fit + stats::qt(p, plx$df) * plx$se.fit
    if(plot == TRUE) {
      graphics::plot(x, y, type="n", main="Loess fit", 
	    sub=paste("confidence intervals at", p))
      graphics::polygon(c(x,rev(x)), c(lci, rev(uci)), col="grey86")
      graphics::points(x, y, pch=20, cex=0.70)
      graphics::lines(x, plx[["fit"]], lty=3)
    }
  return( list(loess=plx$fit, se=plx$se.fit, lci=lci, uci=uci,  
               df=plx$df, rs=plx$residual.scale ) )  
}    
 