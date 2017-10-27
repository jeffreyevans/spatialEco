#' @title Print Loess bootstrap model
#' @description print method for class "loess.boot"
#' @param x    Object of class loess.boot
#' @param ...  Ignored
#'
#' @method print loess.boot
#' @export
print.loess.boot <- function(x, ...) {
  cat("Number of permutations: ", x$nreps, "\n", sep="")
  cat("Loess alpha (span) parameter: ", x$span, "\n", sep="")
  cat("Polynomial degree: ", x$degree, "\n", sep="")
  cat("Data normalized: ", x$normalize, "\n", sep="")
  cat("Model distribution: ", x$family, "\n", sep="")
  cat("parametric model: ", x$parametric, "\n", sep="")
}
