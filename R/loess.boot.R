#' @title Loess Bootstrap 
#' @description Bootstrap of a Local Polynomial Regression (loess) 
#' 
#' @param  y            Dependent variable
#' @param  x            Independent variable  
#' @param  nreps        Number of bootstrap replicates 
#' @param  confidence   Fraction of replicates contained in confidence 
#'                      region 
#' @param  ...          Additional arguments passed to loess function
#'                      
#' @return list object containing  
#' * nreps        Number of bootstrap replicates 
#' * confidence   Confidence interval (region)
#' * span         alpha (span) parameter used loess fit
#' * degree       polynomial degree used in loess fit
#' * normalize    Normalized data (TRUE/FALSE)
#' * family       Family of statistic used in fit
#' * parametric   Parametric approximation (TRUE/FALSE) 
#' * surface      Surface fit, see loess.control
#' * data         data.frame of x,y used in model
#' * fit          data.frame including:
#'   1) x - Equally-spaced x index (see NOTES)         
#'   2) y.fit - loess fit
#'   3) up.lim - Upper confidence interval 
#'   4) low.lim - Lower confidence interval 
#'   5) stddev - Standard deviation of loess fit at each x value
#' @md
#'
#' @description
#' The function fits a loess curve and then calculates a symmetric nonparametric 
#' bootstrap with a confidence region. Fitted curves are evaluated at a fixed number 
#' of equally-spaced x values, regardless of the number of x values in the data. Some 
#' replicates do not include the values at the lower and upper end of the range of x   
#' values. If the number of such replicates is too large, it becomes impossible to 
#' construct a confidence region that includes a fraction "confidence" of the bootstrap 
#' replicates. In such cases, the left and/or right portion of the confidence region 
#' is truncated.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Cleveland, WS, (1979) Robust Locally Weighted Regression and Smoothing Plots Journal  
#'   of the American Statistical Association 74:829-836
#' @references
#' Efron, B., and R. Tibshirani (1993) An Introduction to the Bootstrap Chapman and 
#'   Hall, New York
#' @references
#' Hardle, W., (1989) Applied Nonparametric Regression Cambridge University Press, NY.
#' @references
#' Tibshirani, R. (1988) Variance stabilization and the bootstrap. 
#'   Biometrika 75(3):433-44.
#'
#' @examples 
#'  n=1000
#'  x <- seq(0, 4, length.out=n)	 
#'  y <- sin(2*x)+ 0.5*x + rnorm(n, sd=0.5)
#'  sb <- loess.boot(x, y, nreps=99, confidence=0.90, span=0.40)
#'  plot(sb)
#'                      	
#' @export    	                               
loess.boot <- function(x, y, nreps=100, confidence=0.95, ...){
    dat <- stats::na.omit(data.frame(x=x,y=y))
      if(nrow(dat) == 0) stop ( "Error in dropping NA's")
        ndx <- order(dat$x)
          dat$x <- dat$x[ndx]
        dat$y <- dat$y[ndx]
      r <- range(dat$x, na.rm=TRUE)
    x.out <- seq(r[1], r[2], length.out=40)
    f <- stats::loess(y~x, data=dat, ...)
      y.fit <- stats::approx(f$x, stats::fitted(f), x.out,rule=2)$y
    len <- length(dat$x)
    mat <- matrix(0,nreps,length(x.out))
    for(i in seq(nreps)){
      ndx <- sample(len,replace=TRUE)
        x.repl <- x[ndx]
          y.repl <- y[ndx]
            f <- stats::loess(y.repl~x.repl, ...)
        mat[i,] <- stats::predict(f, newdata=x.out)
      }
    n.na <- apply(is.na(mat), 2, sum)  
      nx <- ncol(mat)
        up.lim <- rep(NA, nx)
      low.lim <- rep(NA, nx)
    stddev <- rep(NA, nx)
    for(i in 1:nx) {
        if(n.na[i] > nreps*(1.0-confidence)) {
            next
        }
        conf <- confidence*nreps/(nreps-n.na[i])
          pr <- 0.5*(1.0 - conf)
            up.lim[i] <- stats::quantile(mat[,i], 1.0-pr, na.rm=TRUE)
          low.lim[i] <- stats::quantile(mat[,i], pr, na.rm=TRUE)
        stddev[i] <- stats::sd(mat[,i], na.rm=TRUE)
      }
    ndx <- !is.na(up.lim)   
    fit <- data.frame(x=x.out[ndx], y.fit=y.fit[ndx], up.lim=up.lim[ndx],
                      low.lim=low.lim[ndx], stddev=stddev[ndx])
	fit.boot <- list(nreps=nreps, confidence=confidence,
                     span=f$pars$span, degree=f$pars$degree, 
				     normalize=f$pars$normalize, family=f$pars$family,
				     parametric=f$pars$parametric, surface=f$pars$surface,
                     data=dat, fit=fit)
    class( fit.boot ) <- "loess.boot"
  return( fit.boot )
}
