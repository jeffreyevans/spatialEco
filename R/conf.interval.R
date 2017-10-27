#' @title Confidence interval for mean or median
#' @description Calculates confidence interval for the mean or median of a distribution with unknown population variance
#' 
#' @param x             Vector to calculate confidence interval for
#' @param cl            Percent confidence level (default = 0.95)
#' @param stat          Statistic (mean or median)
#' @param std.error     Return standard error (TRUE/FALSE)
#'
#' @return lci          Lower confidence interval value
#' @return uci          Upper confidence interval value
#' @return mean         If stat = "mean", mean value of distribution
#' @return mean         Value of the mean or median
#' @return conf.level   Confidence level used for confidence interval  
#' @return std.error    If std.error = TRUE standard error of distribution
#'   
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#'  x <- runif(100)
#'  cr <- conf.interval(x, cl = 0.97) 
#'  print(cr)
#'
#'  d <- density(x)
#'  plot(d, type="n", main = "PDF with mean and 0.97 confidence interval")
#'    polygon(d, col="cyan3")
#'    abline(v=mean(x, na.rm = TRUE), lty = 2)
#'    segments( x0=cr[["lci"]], y0=mean(d$y), x1=cr[["uci"]], 
#'              y1=mean(d$y), lwd = 2.5, 
#'              col = "black")
#'  	legend("topright", legend = c("mean", "CI"), 
#'  	       lty = c(2,1), lwd = c(1,2.5)) 
#'
#' @export
conf.interval <- function(x, cl = 0.95, stat = "mean", std.error = TRUE) {
  se <- function(x) { sqrt(stats::var(x, na.rm = TRUE) / length(stats::na.omit(x))) }
  x <- x[!is.na(x)]
  if( stat == "mean") {
    e = stats::qt(cl, df = length(x)-1) * se(x)
    cie <- mean(x) + c(-e, e)
	cie <- list("lci"=cie[1], "uci"=cie[2], "mean"=mean(x), "conf.level"=cl)
  } else {
  cie <- stats::wilcox.test(x, conf.int = TRUE, conf.level = cl)
  cie <- list("lci"=cie$conf.int[1], "uci"=cie$conf.int[2], "median"=cie$estimate,
             "conf.level"=cl)
  }
  if( std.error ) { 
    cie[["std error"]] <- se(x)
  }	  
  return(cie)
}
