#' @title Print spatial cross correlation
#' @description print method for class "cross.cor"
#'
#' @param x    Object of class cross.cor
#' @param ...  Ignored
#'
#' @return 
#' When not simulated k=0, prints functions list object containing:
#' * I - Global autocorrelation statistic
#' * SCI - - A data.frame with two columns representing the xy and yx autocorrelation
#' * nsim - value of NULL to represent p values were derived from observed data (k=0)
#' * p - Probability based observations above/below confidence interval
#' * t.test - Probability based on t-test
#'
#' When simulated (k>0), prints functions list object containing: 
#'  * I - Global autocorrelation statistic
#'  * SCI - A data.frame with two columns representing the xy and yx autocorrelation
#'  * nsim - value representing number of simulations
#'  * global.p - p-value of global autocorrelation statistic
#'  * local.p - Probability based simulated data using successful rejection of t-test
#'  * range.p - Probability based on range of probabilities resulting from paired t-test
#' 
#' @method print cross.cor
#'
#' @md
#' @export
print.cross.cor <- function(x, ...) {
  if(!is.null(x$nsim)) {
  cat("Moran's-I under randomization assumptions...", "\n")
    cat("  First-order Moran's-I: ", x$I, "\n")
  	cat("  First-order p-value: ", x$global.p, "\n")
  cat("Chen's SCI under randomization assumptions...", "\n")
	cat("\n", "Summary statistics of local partial cross-correlation [xy]", "\n")
      print( summary(x$SCI[,1]) )
        cat("", "\n")  
  	cat("  p-value based on 2-tailed t-test: ", x$local.p, "\n")	
    cat("  p-value based on 2-tailed t-test observations above/below CI: ", x$range.p, "\n")	
    if(!is.null(x[["clusters"]])){
	  cat("\n", "Counts of cluster types")
	    print(table(x$cluster))
	}
  } else {
  cat("Moran's-I...", "\n")
      cat("  First-order Moran's-I: ", x$I, "\n")
  	  cat("  First-order p-value: ", x$global.p, "\n")
  cat("", "\n") 
  cat("Chen's SCI under randomization assumptions...", "\n")
	cat("\n", "Summary statistics of local partial cross-correlation [xy]", "\n")
      print( summary(x$SCI[,1]) )
	    cat("", "\n") 
  	cat("    non-simulated second-order p-value based on 2-tailed t-test: ", x$t.test, "\n")
	cat("    p-value based on 2-tailed t-test observations above/below CI: ", x$p, "\n")	
    if(!is.null(x[["clusters"]])) {
	  cat("\n", "Counts of cluster types")
	    print(table(x$cluster))
	}
  }
}
