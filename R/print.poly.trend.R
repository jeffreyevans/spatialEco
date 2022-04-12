#' @title Print poly_trend
#' @description print method for class "poly.trend"
#' @param x    Object of class poly.trend
#' @param ...  Ignored
#'
#' @method print poly.trend
#' @export
print.poly.trend <- function(x, ...) {
  cat("Polynomial trend model fit using a", x$order, 
     "order polynomial",  "\n")
    print(summary(x$model))
  cat("", "\n")
  cat("Summary of trend confidence intervals",  "\n")
	print(summary(x$trend[,2:3]))
}  