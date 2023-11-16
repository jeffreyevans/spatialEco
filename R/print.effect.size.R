#' @title Print effect size
#' @description print method for class "effect.size"
#'
#' @param x    Object of class effect.size
#' @param ...  Ignored
#'
#' @return
#' Prints the output data.frame contaning; effect size with upper and lower confidence 
#' and, mean and sd by group  
#'
#' @method print effect.size
#' @export
print.effect.size <- function(x, ...) {
  for(i in 1:nrow(x$effect.size)) {
    print(paste( paste0("Effect size for: ", rownames(x$effect.size)[i]), 
	      round(x$effect.size[,1][i],4), sep=" = ") )
	cat(paste0(paste(rep(" ", 4), collapse = ""), 
	    paste0( "Lower CI: ", round(x$effect.size[,2][i],4))))
    cat(paste0(paste(rep(" ", 4), collapse = ""), 
	    paste0( "Upper CI: ", round(x$effect.size[,3][i],3))))
	cat("", "\n")
	cat("", "\n")
  }
}  
