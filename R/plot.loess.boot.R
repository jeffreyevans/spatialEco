#' @title Plot Loess Bootstrap 
#' @description Plot function for loess.boot object 
#'
#' @param  x      A loess.boot object
#' @param  ...    Additional arguments passed to plot
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Cleveland, WS, (1979) Robust Locally Weighted Regression and Smoothing Plots Journal of the American Statistical Association 74:829-836
#' @references
#' Efron, B., and R. Tibshirani (1993) An Introduction to the Bootstrap Chapman and Hall, New York
#' @references
#' Hardle, W., (1989) Applied Nonparametric Regression Cambridge University Press, NY.
#' @references
#' Tibshirani, R. (1988) Variance stabilization and the bootstrap. Biometrika 75(3):433-44.
#'
#' @examples 
#'  n=1000
#'  x <- seq(0, 4, length.out=n)	 
#'  y <- sin(2*x)+ 0.5*x + rnorm(n, sd=0.5)
#'  sb <- loess.boot(x, y, nreps = 99, confidence = 0.90, span = 0.40)
#'  plot(sb)
#'                    
#' @method plot loess.boot 
#' @export    	                               
plot.loess.boot <- function(x, ...) {
  dots <- as.list(match.call(expand.dots = TRUE)[-1])
  dots[["x"]] <- x$data$x
  dots[["y"]] <- x$data$y
	if (is.null(dots[["pch"]]) & "pch" %in% names(dots) == FALSE) dots[["pch"]] <-  20
	if (is.null(dots[["cex"]]) & "cex" %in% names(dots) == FALSE) dots[["cex"]] <-  0.55
	if (is.null(dots[["xlab"]]) & "xlab" %in% names(dots) == FALSE) dots[["xlab"]] <-  "x"
	if (is.null(dots[["ylab"]]) & "ylab" %in% names(dots) == FALSE) dots[["ylab"]] <-  "loess fit"
	if (is.null(dots[["main"]]) & "main" %in% names(dots) == FALSE) dots[["main"]] <-  paste0("Loess bootstrap n = ", x$nreps)   
	if (is.null(dots[["sub"]]) & "sub" %in% names(dots) == FALSE) dots[["sub"]] <-  paste0("Confidence region - ", x$confidence)   
	do.call("plot", dots)
	  graphics::polygon(c(x$fit$x, rev(x$fit$x)), c(x$fit$up.lim, rev(x$fit$low.lim)),  
	            col=grDevices::rgb(0.75, 0.75, 0.75, 0.5))				
      graphics::lines(stats::spline(x$fit$x, x$fit$y.fit), lwd=0.75, lty=2) 
}	
