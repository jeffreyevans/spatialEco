#' @title Kendall tau trend with continuity correction for time-series 
#' @description 
#' Calculates a nonparametric statistic for a monotonic trend based 
#' on the Kendall tau statistic and the Theil-Sen slope modification
#'
#' @param y            A vector representing a timeseries with >= 8 obs
#' @param tau          (FALSE/TRUE) return tau values
#' @param intercept    (FALSE/TRUE) return intercept values 
#' @param p.value      (FALSE/TRUE) return p.values
#' @param confidence   (FALSE/TRUE) return 95 pct confidence levels
#' @param method       Method for deriving tau and slope ("zhang", "yuepilon", "none")
#' @param threshold    The threshold for number of minimum observations in the time-series
#' @param ...          Not used
#'
#' @return Depending on arguments, a vector containing: 
#' * Theil-Sen slope, always returned 
#' * Kendall's tau two-sided test, if tau TRUE
#' * intercept for trend if intercept TRUE
#' * p value for trend fit if p.value TRUE
#' * lower confidence level at 95-pct if confidence TRUE
#' * upper confidence level at 95-pct if confidence TRUE
#' @md
#'
#' @details 
#' This function implements Kendall's nonparametric test for a monotonic trend 
#' using the Theil-Sen (Theil 1950; Sen 1968; Siegel 1982) method to estimate 
#' the slope and related confidence intervals. Critical values are Z > 1.96 
#' representing a significant increasing trend and a Z < -1.96 a significant 
#' decreasing trend (p < 0.05). The null hypothesis can be rejected if Tau = 0. 
#' Autocorrelation in the time-series is addressed using a prewhitened linear trend 
#' following the Zhang et al., (2000) or  Yue & Pilon (2002) methods. If you do not
#' have autocorrelation in the data, the "none" or "yuepilon" method is recommended. 
#' Please note that changing the threshold to fewer than 6 observations (ideally 8) may 
#' prevent the function from failing but, will likely invalidate the statistic. 
#' A threshold of <=4 will yield all NA values. If method= "none" a modification of the
#' EnvStats::kendallTrendTest code is implemented.       
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Theil, H. (1950) A rank invariant method for linear and polynomial regression 
#'   analysis. Nederl. Akad. Wetensch. Proc. Ser. A 53:386-392 (Part I),  
#'   53:521-525 (Part II), 53:1397-1412 (Part III).
#' @references 
#' Sen, P.K. (1968) Estimates of Regression Coefficient Based on Kendall's tau. 
#'   Journal of the American Statistical Association. 63(324):1379-1389.
#' @references 
#' Siegel, A.F. (1982) Robust Regression Using Repeated Medians. 
#'   Biometrika, 69(1):242-244
#' @references 
#' Yue, S., P. Pilon, B. Phinney and G. Cavadias, (2002) The influence of autocorrelation 
#'   on the ability to detect trend in hydrological series. 
#'   Hydrological Processes, 16: 1807-1829.
#' @references 
#' Zhang, X., Vincent, L.A., Hogg, W.D. and Niitsoo, A., (2000) Temperature 
#'   and Precipitation Trends in Canada during the 20th Century. 
#'   Atmosphere-Ocean 38(3): 395-429. 
#'
#' @examples
#' data(EuStockMarkets)
#' d <- as.vector(EuStockMarkets[,1])
#' kendall(d)
#' 
#' @seealso \code{\link[zyp]{zyp.trend.vector}} for model details
#'
#' @export kendall
kendall <- function(y, tau = TRUE, intercept = TRUE, p.value = TRUE, 
                    confidence = TRUE, method=c("zhang", "yuepilon", "none"),
					threshold = 6, ...) {
	if(any(method %in% c("zhang", "yuepilon"))) {
      if(!any(which(utils::installed.packages()[,1] %in% "zyp")))
        stop("please install zyp package before running this function")
	}    
    if(threshold < 6)
      warning("Setting the time-series threshold to fewer than 6 obs may invalidate 
	    the statistic and n <= 4 will always result in NA's") 
    if(length(y[!is.na(y)]) < threshold) 
      warning("The Kendall Tau should have at least 6 observations")	  

    out.names <- c("slope", "tau", "intercept", "p-value", "limits.LCL", "limits.UCL")[
	               which(c(TRUE, tau, intercept, p.value, rep(confidence,2)))]  
    idx <- 2
      if(tau == TRUE) { idx = append(idx, 5) }
        if(intercept == TRUE) { idx = append(idx, 11) }  
          if(p.value == TRUE) { idx = append(idx, 6) }
	        if(confidence == TRUE) { idx = append(idx, c(1,4)) }
  mk.trend <- function (y, x = seq(along = y), alternative = "two.sided", 
                        conf.level = 0.95, ...)  {
    out.names <- c("slope", "tau", "intercept", "p-value", "limits.LCL", "limits.UCL")	
    n <- length(y)
    alternative <- match.arg(alternative, c("two.sided", "greater", "less"))						
      vark <- function(x, y) {
          ties.x <- rle(sort(x))$lengths
          ties.y <- rle(sort(y))$lengths
          n <- length(x)
          t1 <- n * (n - 1) * (2 * n + 5)
          t2 <- sum(ties.x * (ties.x - 1) * (2 * ties.x + 5))
          t3 <- sum(ties.y * (ties.y - 1) * (2 * ties.y + 5))
          v1 <- (t1 - t2 - t3)/18
          if (n > 2) {
            t1 <- sum(ties.x * (ties.x - 1) * (ties.x - 2))
            t2 <- sum(ties.y * (ties.y - 1) * (ties.y - 2))
            v2 <- (t1 * t2)/(9 * n * (n - 1) * (n - 2))
          } else  {
          v2 <- 0
            t1 <- sum(ties.x * (ties.x - 1)) * sum(ties.y * (ties.y - 1))
            v3 <- t1/(2 * n * (n - 1))
            v1 + v2 + v3
        }
        return(v1)
      }						
   if(length(stats::na.omit(y)) < threshold) { 
     v <- c(NA,NA,NA,NA, NA, NA) 
	   names(v) <- out.names
    } else {
    index <- 2:n
    S <- sum(sapply(index, function(i, x, y) {
        sum(sign((x[i] - x[1:(i - 1)]) * (y[i] - y[1:(i - 1)])))}, x, y))
    tau <- (2 * S)/(n * (n - 1))
    slopes <- unlist(lapply(index, function(i, x, y) (y[i] -
                     y[1:(i - 1)])/(x[i] - x[1:(i - 1)]), x, y))
    slopes <- sort(slopes[is.finite(slopes)])
    slope <- stats::median(slopes)
    intercept <- stats::median(y) - slope * stats::median(x)
    estimate <- c(tau, slope, intercept)
    names(estimate) <- c("tau", "slope", "intercept")
    var.S <- vark(x, y)       
	stat <- S/sqrt(var.S)
        names(stat) <- "z"
      p.value <- switch(alternative, greater = 1 - stats::pnorm(stat),
          less = stats::pnorm(stat), two.sided = 2 * stats::pnorm(-abs(stat)))		
    N.prime <- length(slopes)
    type <- switch(alternative, two.sided = "two-sided",
        greater = "lower", less = "upper")
    alpha <- 1 - conf.level
    Z <- ifelse(type == "two-sided", stats::qnorm(1 - alpha/2),
                 stats::qnorm(conf.level))
    C.alpha <- Z * sqrt(var.S)
    M1 <- (N.prime - C.alpha)/2
    M2 <- (N.prime + C.alpha)/2
    limits <- switch(type, `two-sided` = stats::approx(1:N.prime,
        slopes, xout = c(M1, M2 + 1))$y, lower = c(stats::approx(1:N.prime,
        slopes, xout = M1)$y, Inf), upper = c(-Inf, stats::approx(1:N.prime,
        slopes, xout = M2 + 1)$y))
          names(limits) <- c("LCL", "UCL")
    v <-c(estimate[c(2,1,3)], p.value, limits) 
      names(v) <- out.names	  
    }
    return(v)
  }
  if(length(y[!is.na(y)]) < threshold) { 
    fit.results <- rep(NA,length(out.names))
  } else if(method[1] == "none") { 
    fit.results <- mk.trend(y)
  }  else {
    fit.results <- zyp::zyp.trend.vector(y, method=method[1])[idx]	
  }	
    names(fit.results) <- out.names  
  return(fit.results)
}	
