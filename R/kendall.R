#' @title Kendall tau trend with continuity correction for time-series 
#' @description 
#' Calculates a nonparametric statistic for a monotonic trend based 
#' on the Kendall tau statistic and the Theil-Sen slope modification
#'
#' @param y          A vector representing a timeseries with >= 8 obs
#' @param tau        (FALSE/TRUE) return tau values
#' @param intercept  (FALSE/TRUE) return intercept values 
#' @param p.value    (FALSE/TRUE) return p.values
#' @param z.value    (FALSE/TRUE) return z values
#' @param confidence (FALSE/TRUE) return 95 pct confidence levels
#' @param prewhiten  (FALSE/TRUE) Apply autocorrelation correction using 
#'                    pre-whitening 
#' @param na.rm      (FALSE/TRUE) Remove NA values
#' @param ...         Not used
#'
#' @return Depending on arguments, a vector containing:
#' \itemize{
#'   \item {value 1} { Theil-Sen slope, always returned }
#'   \item {value 2} {Kendall's tau two-sided test, if tau TRUE}
#'   \item {value 3} {intercept for trend if intercept TRUE, not 
#'                    if prewhitened}
#'   \item {value 4} {p value for trend fit if p.value TRUE}
#'   \item {value 5} {Z value for trend fit if z.value TRUE}
#'   \item {value 6} {lower confidence level at 95-pct if confidence 
#'                    TRUE, not if prewhitened}
#'   \item {value 7} {upper confidence level at 95-pct if confidence 
#'                    TRUE, not if prewhitened}
#' }
#'
#' @details 
#' This function implements Kendall's nonparametric test for a monotonic trend 
#' using the Theil-Sen (Theil 1950; Sen 1968; Siegel 1982) method to estimate 
#' the slope and related confidence intervals. Critical values are Z > 1.96 
#' representing a significant increasing trend and a Z < -1.96 a significant 
#' decreasing trend (p < 0.05). The null hypothesis can be rejected if Tau = 0. 
#' There is also an option for autocorrelation correction using the method proposed 
#' in Yue & Wang (2002). 
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
#' Yue, S., & Wang, C. Y. (2002). Applicability of prewhitening to eliminate 
#' the influence of serial correlation on the Mann-Kendall test. Water 
#' Resources Research, 38(6):41-47. 
#'
#' @export kendall
kendall <- function(y, tau = TRUE, p.value = TRUE, z.value = TRUE, 
                    confidence = TRUE, intercept = TRUE,
					prewhiten = FALSE, na.rm, ...) {
    if(length(y[!is.na(y)]) < 8) 
      stop("The Kendall Tau needs at least 8 observations")
    pass.sum <- 0
	if(prewhiten) {
	  confidence = FALSE
	  intercept = FALSE 
    }
      if( p.value ) pass.sum = pass.sum + 1
	    if( z.value ) pass.sum = pass.sum + 1
	    if( tau ) pass.sum = pass.sum + 1
	  if( confidence ) pass.sum = pass.sum + 2
	if( intercept ) pass.sum = pass.sum + 1
      fit.results <- c(rep(NA,pass.sum + 1))
    if(!prewhiten) {
      fit <- EnvStats::kendallTrendTest(y ~ 1)
      fit.results <- fit$estimate[2]
        if(tau == TRUE) { fit.results <- c(fit.results, fit$estimate[1]) }
            if(intercept == TRUE) { fit.results <- c(fit.results, fit$estimate[3]) }  
              if(p.value == TRUE) { fit.results <- c(fit.results, fit$p.value) } 
                if(z.value == TRUE) { fit.results <- c(fit.results, fit$statistic) }
	        if(confidence == TRUE) { 
          ci <- unlist(fit$interval["limits"])
            if( length(ci) == 2) { 
              fit.results <- c(fit.results, ci)
              } else {
                fit.results <- c(fit.results, c(NA,NA))
              }			  
         }
    } else {
    # kendall autocorrelation correction (pre-whitening)
    x = y
	  z = NULL
        pval = NULL
        S = 0
      var.S = NULL
    Tau = NULL
      if (any(is.finite(x) == FALSE)) {
        x <- x[-c(which(is.finite(x) == FALSE))]
      }
      n <- length(x)
    V <- rep(NA, n * (n - 1)/2)
    k = 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          k = k + 1
          V[k] = (x[j] - x[i])/(j - i)
        }
      }
    slp <- stats::median(V, na.rm = TRUE)
      t1 = 1:length(x)
        xt <- (x[1:n]) - ((slp) * (t1))
    ro <- stats::acf(xt, lag.max = 1, plot = FALSE)$acf[-1]
      a = 1:(length(xt) - 1)
        b = 2:(length(xt))
          xp <- (xt[b] - (xt[a] * ro))
          l <- length(xp)
        q = 1:l
      y <- (xp[1:l] + ((slp) * (q)))
    n1 <- length(y)
      for (i in 1:(n1 - 1)) {
        for (j in (i + 1):n1) {
          S = S + sign(y[j] - y[i])
        }
      }
    var.S = n1 * (n1 - 1) * (2 * n1 + 5) * (1/18)
      if (length(unique(y)) < n1) {
        aux <- unique(y)
          for (i in 1:length(aux)) {
            tie <- length(which(y == aux[i]))
              if (tie > 1) {
                var.S = var.S - tie * (tie - 1) * (2 * tie + 5) * (1/18)
              }
          }
      }
    if (S == 0) { z = 0 }
    if (S > 0) {
      z = (S - 1)/sqrt(var.S)
    } else {
      z = (S + 1)/sqrt(var.S)
    }
    pval = 2 * stats::pnorm(-abs(z))
      Tau = S/(0.5 * n1 * (n1 - 1))
        W <- rep(NA, n1 * (n1 - 1)/2)
          m = 0
    for (i in 1:(n1 - 1)) {
      for (j in (i + 1):n1) {
        m = m + 1
        W[m] = (y[j] - y[i])/(j - i)
      }
    }
    slp1 <- stats::median(W, na.rm = TRUE)
	  fit.results <- slp1 
	  if(tau == TRUE) { fit.results <- c(fit.results, Tau) }
        if(p.value == TRUE) { fit.results <- c(fit.results, pval) } 
          if(z.value == TRUE) { fit.results <- c(fit.results, z) }
	  fit.results <- as.numeric(fit.results) 
    }
  return(c(fit.results))
}	
  