#' @title Summary of spatial cross correlation
#' @description summary method for class "cross.cor"
#' @param x    Object of class cross.cor
#' @param ...  Ignored
#'
#' @method summary cross.cor
#'
#' @export
summary.cross.cor <- function(x, ...) {
  if(any(names(x)=="summary")) {
  cat("Moran's-I under randomization assumptions...", "\n")
    cat("    First-order Moran's-I: ", x$summary$I, "\n")
	cat("    Mean(I): ", x$summary$meanI, "\n")
	cat("    STDV(I): ", x$summary$stdI, "\n")
  	cat("    p-value (2-sided): ", x$summary$p, "\n")
	cat("\n", "Summary statistics of local partial cross-correlation", "\n")
      print(summary(x$lisa))
	cat("\n", "Counts of cluster types")
	  print(table(x$cluster))
  } else {
    cat("First-order Moran's-I: ", x$I, "\n")
    cat("\n", "Summary statistics of local partial cross-correlation", "\n")
      print(summary(x$lisa))
  	cat("\n", "Counts of cluster types")
  	  print(table(x$cluster))
  }
}
