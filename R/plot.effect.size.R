#' @title Plot effect size 
#' @description Plot function for effect.size object 
#'
#' @param  x      A effect.size object
#' @param  ...    Additional arguments passed to plot
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                    
#' @method plot effect.size 
#' @export    	                               
plot.effect.size <- function(x, ...) {
  dots <- as.list(match.call(expand.dots = TRUE)[-1])
  dots[["x"]] <- as.numeric(as.factor(rownames(x$effect.size)))
  dots[["y"]] <- x$effect.size[,1]
  dots[["xaxt"]] <- "n"
  dots[["ylim"]] <- c(min(x$effect.size), max(x$effect.size))  
	if (is.null(dots[["pch"]]) & "pch" %in% names(dots) == FALSE) dots[["pch"]] <- 20
    if (is.null(dots[["cex"]]) & "cex" %in% names(dots) == FALSE) dots[["cex"]] <- 1
	if (is.null(dots[["xlab"]]) & "xlab" %in% names(dots) == FALSE) dots[["xlab"]] <-  "class"
	if (is.null(dots[["ylab"]]) & "ylab" %in% names(dots) == FALSE) dots[["ylab"]] <-  "effect size"
	if (is.null(dots[["main"]]) & "main" %in% names(dots) == FALSE) dots[["main"]] <- paste("Effect size with", x$CI, "CI",sep=" ") 
	  do.call("plot", dots)
	    graphics::axis(side=1, at = as.numeric(as.factor(rownames(x$effect.size))), 
	                   labels = rownames(x$effect.size))
        for(i in 1:nrow(x$effect.size)) {
	      graphics::arrows(i, x$effect.size[,2][i], i, 
	  	                   x$effect.size[,3][i], 
	  			           length = 0.05, angle = 90, 
	  			           code = 3)
	    }
}	
