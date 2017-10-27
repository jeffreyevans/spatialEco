#' @title Cohen's-d effect size
#' @description Cohen's-d effect size with pooled sd for a control and experimental group 
#' 
#' @param y            A character or factor vector 
#' @param x            A numeric vector, same length as y
#' @param pooled       Pooled or population standard deviation (TRUE/FALSE)
#' @param conf.level   Specified confidence interval. Default is 0.95
#'
#' @return An effect.size class object with x, y and a data.frame with columns for effect size, lower confidence interval, lower confidence interval. The row names of the data frame represent the levels in y
#'
#' @note This implementation will iterate tourhg each class in y and treating a given class as the experimental group and all other classes as a control case. Each class had d and the confidence interval derived. A negative d indicate directionality with same magnitude. The expected range for d is 0 - 3 
#' @note d is derived; ( mean(experimental group) - mean(control group) ) / sigma(p)
#' @note pooled standard deviation is derived; sqrt(  ( (Ne - 1) * sigma(e)^2 + (Nc - 1) * sigma(c)^2 ) / (Ne + Nc - 2) )   where; Ne, Nc = n of experimental and control groups.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'         
#' @references  Cohen, J., (1988) Statistical Power Analysis for the Behavioral Sciences (second ed.). Lawrence Erlbaum Associates.
#' @references  Cohen, J (1992) A power primer. Psychological Bulletin 112(1):155-159
#'                                                          
#' @examples
#'  ( es <- effect.size(iris$Species, iris$Sepal.Length) )
#'    plot(es)
#'
#' @exportClass effect.size 
#' @export
effect.size <- function(y, x, pooled = TRUE, conf.level = 0.95) {
  effect.size <- vector()
  low.ci <- vector()
  up.ci <- vector()
    for(i in levels(y)){
      y.i <- ifelse( y == i, i, "control")
	    g.means <- tapply(x, y.i, mean)
	    g.sd <- tapply(x, y.i, stats::sd)
      n <- table(y)
	  if(pooled) {
        sd.pooled <- sqrt( ((n[1] - 1) * g.sd[1]^2 + (n[2] - 1) * g.sd[2]^2) / 
	                   (n[1] + n[2] - 2) )
	    } else {
          sd.pooled <- stats::sd(x)
      }
      delta.m <- g.means[i] - g.means["control"]  	
	    es <- delta.m / sd.pooled
		effect.size <- append(effect.size, es)
        deg.f = n[1] + n[2] - 2
          Sd <- sqrt(((n[1] + n[2])/(n[1] * n[2]) + 0.5 * es ^ 2 / deg.f) * 
		            ((n[1] + n[2]) / deg.f))
            Z <- -stats::qt((1 - conf.level) / 2, deg.f)
          conf.int <- c(es - Z * Sd, es + Z * Sd)
        low.ci <- append(low.ci, conf.int[1])
      up.ci <- append(up.ci, conf.int[2])	
	}
    effect.size <- list( effect.size = data.frame(row.names=unique(y), 
	                     effect.size = effect.size, lower.ci = low.ci,   
	                     upper.ci = up.ci), y = y, x = x, CI = conf.level)
	  						  
      class(effect.size) <- c("effect.size", "data.frame")
  return( effect.size )
}
