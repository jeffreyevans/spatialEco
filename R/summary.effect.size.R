#' @title Summarizing effect size
#' @description Summary method for class "effect.size".
#' @param object  Object of class effect.size
#' @param ... Ignored
#'
#' @method summary effect.size
#' @export
summary.effect.size <- function(object, ...) {
  cat("Mean by groups in y: ", "\n")
    print( tapply(object$x, object$y, mean) )
	cat("", "\n") 
  cat("Standard deviation by groups in y: ", "\n")
    print( tapply(object$x, object$y, stats::sd) )
	cat("", "\n") 
  for(i in 1:nrow(object$effect.size)) {
    print(paste( paste0("Effect size for: ", rownames(object$effect.size)[i]), 
	      round(object$effect.size[,1][i],4), sep=" = ") )
	cat(paste0(paste(rep(" ", 4), collapse = ""), 
	    paste0( "Lower CI: ", round(object$effect.size[,2][i],4))))
    cat(paste0(paste(rep(" ", 4), collapse = ""), 
	    paste0( "Upper CI: ", round(object$effect.size[,3][i],3))))
	cat("", "\n")
	cat("", "\n")
  }
}
