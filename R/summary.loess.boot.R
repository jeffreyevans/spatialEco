#' @title Summarizing Loess bootstrap models
#' @description Summary method for class "loess.boot".
#' @param object  Object of class loess.boot
#' @param ... Ignored
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
