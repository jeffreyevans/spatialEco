#' @title Polynomial trend
#' @description Fits a polynomial trend using specified order
#'
#' @param x        Vector of x
#' @param y        Vector of y
#' @param degree   Polynomial order (default 3)
#' @param ci       +/- confidence interval (default 0.95) 
#' @param plot     Plot results (TRUE/FALSE)
#' @param ...      Additional arguments passed to plot 
#'
#' @details 
#' A fit using a lm(y ~ x + I(X^2) + I(X^3)) form will be correlated which,  
#' can cause problems. The function avoids undue correlation using orthogonal 
#' polynomials
#'
#' @return A poly.trend class (list) containing  
#' * trend        data.frame of fit polynomial and upper/lower confidence intervals 
#' * model        Class lm model object fit with poly term       
#' * prameterCI   Intercept confidence intervals of Nth order polynomials    
#' * order        Specified polynomial order
#' @md
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#'  set.seed(42)
#'  x <- seq(from=0, to=20, by=0.1)
#'  y <- (500 + 0.4 * (x-10)^3)
#'  noise <- y + rnorm(length(x), mean=10, sd=80) 
#'  
#'  p <- poly_trend(x, noise, degree = 3, ci = 0.95,
#'                  main="3rd degree polynomial")
#'  
#'  dev.new(height=6, width=12)
#'    layout(matrix(c(1,2), 1, 2, byrow = TRUE))
#'    p <- poly_trend(x, noise, degree = 3, 
#'                    main="3rd degree polynomial")
#'    p <- poly_trend(x, noise, degree = 6, 
#'                    main="6th degree polynomial")
#' 
#' cat("Confidence intervals for", "1 -", p$order, "polynomials",  "\n")
#'   p$prameterCI
#'
#' @export poly_trend
poly_trend <- function(x, y, degree, ci = 0.95, plot=TRUE, ...) {
  poly.f <- function(x, a, b, d) {
    (a * x^2) + (b * x) + d
  }
  fit <- stats::lm(y ~ stats::poly(x,degree))
    conf <- stats::confint(fit, level=ci)
      trend <- stats::predict(fit, data.frame(x=x),
                       interval='confidence',
                       level=ci)
  if (plot == TRUE) { 
    #dots <- as.list(match.call(expand.dots = TRUE)[-1])
	dots <- list(...)
	if(any(names(dots) %in% c("degree", "ci"))) {
	  dots <- dots[-which(names(dots) %in% c("degree", "ci"))]
	}  
      dots[["x"]] <- x
      dots[["y"]] <- y
	if(is.null(dots[["pch"]]) & "pch" %in% names(dots) == FALSE) dots[["pch"]] <- 20
	if(is.null(dots[["xlab"]]) & "xlab" %in% names(dots) == FALSE) dots[["xlab"]] <- "x"
	if(is.null(dots[["ylab"]]) & "ylab" %in% names(dots) == FALSE) dots[["ylab"]] <- "y"
	if(is.null(dots[["main"]]) & "main" %in% names(dots) == FALSE) dots[["main"]] <- "polynomial trend"  
	if(is.null(dots[["col"]]) & "col" %in% names(dots) == FALSE) dots[["col"]] <- "deepskyblue4"
	  do.call("plot", dots)
         graphics::lines(x, trend[,1], col='black', lty=1, lwd=2)
           graphics::lines(x, trend[,2], col='black', lty=3, lwd=2)
           graphics::lines(x, trend[,3], col='black', lty=3, lwd=2)
      graphics::legend("bottomright", c("trend", "upper-lower confidence"), 
	  				 lwd=c(2,2), lty=c(1,3), col=c('black', 'black'))
  }
      p <- list(trend=trend, model=fit, prameterCI=conf, order=degree)
    class(p) <- "poly.trend"
  return(p)
}
