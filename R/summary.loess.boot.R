#' @title Summarizing Loess bootstrap models
#' @description Summary method for class "loess.boot".
#'
#' @param object  Object of class loess.boot
#' @param ... Ignored
#'
#'
#' @return same as print lowess.boot data.frame including; 
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
#'   1) x - Equally-spaced x index (see NOTES)         
#'   2) y.fit - loess fit
#'   3) up.lim - Upper confidence interval 
#'   4) low.lim - Lower confidence interval 
#'   5) stddev - Standard deviation of loess fit at each x value
#' @md 
#'
#' @method summary loess.boot
#' @export
summary.loess.boot <- function(object, ...) {
  cat("Number of permutations: ", object$nreps, "\n", sep="")
  cat("Loess alpha (span) parameter: ", object$span, "\n", sep="")
  cat("Polynomial degree: ", object$degree, "\n", sep="")
  cat("Data normalized: ", object$normalize, "\n", sep="")
  cat("Model distribution: ", object$family, "\n", sep="")
  cat("parametric model: ", object$parametric, "\n", sep="")
  cat("","\n", sep="")  
  cat("summary of fit: ","\n", sep="")
    summary(object$fit[,"y.fit"])
  cat("","\n", sep="")
  cat("summary of lower confidence limits: ","\n", sep="")
    summary(object$fit[,"low.lim"])
  cat("","\n", sep="")
  cat("summary of fit: ","\n", sep="")
    summary(object$fit[,"up.lim"])
}
