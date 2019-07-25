#' @title Local Polynomial Regression
#' @description Calculates a Local Polynomial Regression for smoothing or imputation of missing data.
#' 
#' @param y          Vector to smooth or impute NA values
#' @param x          Optional x covariate data (must match dimensions of y)
#' @param s          Smoothing parameter (larger equates to more smoothing)
#' @param impute     (FALSE/TRUE) Should NA values be inputed
#' @param na.only    (FALSE/TRUE) Should only NA values be change in y
#' @param ci         (FALSE/TRUE) Should confidence intervals be returned
#' @param ...        Additional arguments passed to loess 
#'
#' @return If ci = FALSE, a vector of smoothed values otherwise a list object with:
#' @return   loess      A vector, same length of y, representing the smoothed or inputed data 
#' @return   lower.ci   Lower confidence interval 
#' @return   upper.ci   Upper confidence interval
#'
#' @note 
#' This is a wrapper function for loess that simplifies data smoothing and imputation of missing values. The function allows for smoothing a vector, based on an index (derived automatically) or covariates. If the impute option is TRUE NA values are imputed, otherwise the returned vector will still have NA's present. 
#' If impute and na.only are both TRUE the vector is returned, without being smoothed but with imputed NA values filled in. 
#' @note 
#' The loess weight function is defined using the tri-cube weight function w(x) = (1-|x|^3)^3 where; x is the distance of a data point from the point on the curve being fitted.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                              
#' @examples 
#'  x <- seq(-20, 20, 0.1)
#'  y <- sin(x)/x + rnorm(length(x), sd=0.03)
#'  p <- which(y == "NaN")
#'  y <- y[-p]
#'  r <- poly.regression(y, ci=TRUE, s=0.30)
#'  
#'  plot(y,type="l", lwd=0.5, main="s = 0.10")
#'    y.polygon <- c((r$lower.ci)[1:length(y)], (r$upper.ci)[rev(1:length(y))])
#'    x.polygon <- c(1:length(y), rev(1:length(y)))
#'    polygon(x.polygon, y.polygon, col="#00009933", border=NA) 
#'       lines(r$loess, lwd=1.5, col="red")
#'  
#'  # Impute NA values, replacing only NA's
#'  y.na <- y
#'  y.na[c(100,200,300)] <- NA 
#'  p.y <- poly.regression(y.na, s=0.10, impute = TRUE, na.only = TRUE)
#'  y - p.y
#'  
#'  plot(p.y,type="l", lwd=1.5, col="blue", main="s = 0.10")
#'    lines(y, lwd=1.5, col="red")
#'
#' @seealso \code{\link[stats]{loess}} for loess ... model options  
#'
#' @export
poly.regression <- function(y, x = NULL, s = 0.75, impute = FALSE, 
                            na.only = FALSE, ci = FALSE, ...) {
  na.idx <- which(is.na(y))
    if(length(na.idx) > 0 )
      {
        y.na <- y 
	    y <- y[!is.na(y)]
      }
  if(is.null(x)) { 
    x <- 1:length(y)
    fmla <- stats::as.formula(y ~ x)
	r <- stats::loess(fmla, span = s, ...)
  } else {
    if(!is.data.frame(x)) x <- as.data.frame(x) 
      x.names <- names(x)
	  dat <- data.frame(y=y, x)
    fmla <- stats::as.formula(paste(paste("y", "~", sep=""), paste(x.names, collapse= "+")))
	r <- stats::loess(fmla, data=dat, span = s, ...)
  }
  if(ci == TRUE) { 
    r.pred <- stats::predict(r, se = TRUE)
      low.ci <- r.pred$fit - 1.96 * r.pred$se.fit
      up.ci <- r.pred$fit + 1.96 * r.pred$se.fit
  }
  if(impute == TRUE && length(na.idx) >= 1) {
    p <- stats::predict(r, data.frame(x=na.idx))
	  if(na.only) {
	    r <- y.na
	    r[na.idx] <- p
	  } else {
        r <- spatialEco::insert.values(r$fitted, p, na.idx)
	  }
  } else {		
    if(length(na.idx) > 0) { 
	  r <- spatialEco::insert.values(r$fitted, NA, na.idx)
	} else {
	  r <- r$fitted
    }	
  }  
  if(ci == TRUE) {
    return( list( loess = r, lower.ci = low.ci, upper.ci = up.ci) )
  } else {	
    return( r )
  }
}
