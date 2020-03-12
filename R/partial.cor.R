#' @title Partial and Semi-partial correlation
#' @description Calculates a partial or semi-partial correlation using 
#'              with parametric and nonparametric options
#'
#' @param x          A vector, data.frame or matrix with 3 columns
#' @param y          A vector same length as x 
#' @param z          A vector same length as x
#' @param method     Type of correlation: "partial" or "semipartial" 
#' @param statistic  Correlation statistic, options are: "kendall", 
#'                   "pearson", "spearman" 
#'
#' @return data.frame containing:
#' \itemize{
#'   \item {correlation} {correlation coefficent}
#'   \item {p.value} {p-value of correlation}
#'   \item {test.statistic} {test statistic}
#'   \item {n} {sample size}
#'   \item {Method} {indicating partial or semipartial correlation}
#'   \item {Statistic} {the correlation statistic used}
#' }
#'
#' @details
#' Partial and semipartial correlations show the association between two 
#' variables when one or more peripheral variables are controlled 
#' to hold them constant. 
#'
#' Suppose we have three variables, X, Y, and Z. Partial correlation holds 
#' constant one variable when computing the relations two others. Suppose we 
#' want to know the correlation between X and Y holding Z constant for both 
#' X and Y. That would be the partial correlation between X and Y controlling 
#' for Z. Semipartial correlation holds Z constant for either X or Y, but not 
#' both, so if we wanted to control X for Z, we could compute the semipartial 
#' correlation between X and Y holding Z constant for X.
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' air.flow = stackloss[,1]
#' water.temperature = stackloss[,2]
#' acid = stackloss[,3]
#' 
#' # Partial using Kendall (nonparametric) correlation
#' partial.cor(air.flow, water.temperature, acid)
#' 
#' scholar <- data.frame(
#'   HSGPA=c(3.0, 3.2, 2.8, 2.5, 3.2, 3.8, 3.9, 3.8, 3.5, 3.1), 
#' 	 FGPA=c(2.8, 3.0, 2.8, 2.2, 3.3, 3.3, 3.5, 3.7, 3.4, 2.9),
#'   SATV =c(500, 550, 450, 400, 600, 650, 700, 550, 650, 550)) 
#' 
#' # Standard Pearson's correlations between HSGPA and FGPA  
#' cor(scholar[,1], scholar[,2])
#'
#' # Partial correlation using Pearson (parametric) between HSGPA 
#' #   and FGPA, controlling for SATV
#' partial.cor(scholar, statistic="pearson")
#' 
#' # Semipartial using Pearson (parametric) correlation 
#' partial.cor(x=scholar[,2], y=scholar[,1], z=scholar[,3], 
#'             method="semipartial", statistic="pearson")
#'
#' @export partial.cor 
partial.cor <- function (x, y, z, method = c("partial", "semipartial"),
                         statistic = c("kendall", "pearson", "spearman")) {
  method <- match.arg(method)
  statistic <- match.arg(statistic)  
  if(any(class(x) == c("data.frame", "matrix")) ) {
    if(!missing(y))
	  stop("if x, y and z are defined, x must be a vector")
    if(dim(x)[2] > 3)
	  stop("Only 3 variables are supported for partial correlation")
    xyz <- stats::na.omit(as.matrix(x))
  } else {
    if(missing(y) | missing(z))
	  stop("Must have x, y and z vectors defined for partial correlation")
    e = sapply(list(x[!is.na(x)],y[!is.na(y)],z[!is.na(z)]), length)
      e = range(e) / mean(e)
    if(!isTRUE(all.equal(e[1], e[2])))
      stop("x, y, and z vectors must be same length, check for NA's")    
    xyz <- stats::na.omit(cbind(x, y, z))
  }
  if (!(is.numeric(xyz) || is.logical(xyz))) 
    stop("x must be numeric")
  stopifnot(is.atomic(xyz))   
  pcor.fun <- function (x, method) {
      n <- dim(x)[1]
      gp <- dim(x)[2] - 2
      cvx <- stats::cov(x, method = method)
      if (det(cvx) < .Machine$double.eps) {
        warning("The inverse of variance-covariance matrix is being calculated 
		         using the Moore-Penrose generalized matrix inverse due to a 
				 determinant of zero in the covariance")
      icvx <- MASS::ginv(cvx)
      } else { 
  	  icvx <- solve(cvx)
  	}
      pcor <- -stats::cov2cor(icvx)
        diag(pcor) <- 1
      if (method == "kendall") {
        statistic <- pcor/sqrt(2 * (2 * (n - gp) + 5)/(9 * (n - gp) * (n - 1 - gp)))
          p.value <- 2 * stats::pnorm(-abs(statistic))
      } else {
        statistic <- pcor * sqrt((n - 2 - gp) / (1 - pcor^2))
          p.value <- 2 * stats::pt(-abs(statistic), (n - 2 - gp))
      }
        diag(statistic) <- 0
      diag(p.value) <- 0
    return( list(estimate = pcor, p.value = p.value, 
	             statistic = statistic, n = n) )
  }
  spcor.fun <- function (x, method) {
    x <- as.matrix(x)
      n <- dim(x)[1]
        gp <- dim(x)[2] - 2
          cvx <- stats::cov(x, method = method)
    if(det(cvx) < .Machine$double.eps) {
      warning("The inverse of variance-covariance matrix is being calculated 
	          using the Moore-Penrose generalized matrix inverse due to a 
	  		  determinant of zero in the covariance")      
	  icvx <- MASS::ginv(cvx)
    } else { 
  	  icvx <- solve(cvx)
  	}
    spcor <- -stats::cov2cor(icvx) / sqrt(diag(cvx)) / sqrt(abs(diag(icvx) - 
	          t(t(icvx^2) / diag(icvx))))
        diag(spcor) <- 1
      if (method == "kendall") {
        statistic <- spcor/sqrt(2 * (2 * (n - gp) + 5)/(9 * (n - gp) * (n - 1 - gp)))
          p.value <- 2 * stats::pnorm(-abs(statistic))
      } else {
        statistic <- spcor * sqrt((n - 2 - gp)/(1 - spcor^2))
          p.value <- 2 * stats::pt(-abs(statistic), (n - 2 - gp))
      }
          diag(statistic) <- 0
        diag(p.value) <- 0
    return( list(estimate = spcor, p.value = p.value, 
  	             statistic = statistic, n = n,  
                 method = method) )
  }
  if(method == "partial") {	
    pcor <- pcor.fun(xyz, method = statistic)
  } else if(method == "semipartial") { 
    pcor <- spcor.fun(xyz, method = statistic) 
  } 
  return(data.frame(correlation = pcor$est[1, 2], 
                    p.value = pcor$p.value[1, 2], 
	                test.statistic = pcor$statistic[1, 2], 
					n = pcor$n, Method = method, 
                    Statistic = statistic) )
}
