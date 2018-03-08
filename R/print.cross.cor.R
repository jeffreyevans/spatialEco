#' @title Print spatial cross correlation
#' @description print method for class "cross.cor"
#' @param x    Object of class cross.cor
#' @param ...  Ignored
#'
#' @method print cross.cor
#'
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
    if( exists(x$clusters) )
	  cat("\n", "Counts of cluster types")
	    print(table(x$cluster))
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
    if( exists(x$clusters) )
	  cat("\n", "Counts of cluster types")
	    print(table(x$cluster))
  }
}
