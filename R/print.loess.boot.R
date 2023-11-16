#' @title Print Loess bootstrap model
#' @description print method for class "loess.boot"
#'
#' @param x    Object of class loess.boot
#' @param ...  Ignored
#'
#' @return same as summary lowess.boot of data.frame including; 
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
#'   1) x - Equally-spaced x index       
#'   2) y.fit - loess fit
#'   3) up.lim - Upper confidence interval 
#'   4) low.lim - Lower confidence interval 
#'   5) stddev - Standard deviation of loess fit at each x value
#' @md 
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
