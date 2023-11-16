#' @title Class breaks
#' @description Finds class breaks in a distribution
#'
#' @param x     A vector to find breaks for
#' @param n     Number of breaks
#' @param type  Statistic used to find breaks c("equal", "quantile", "std", "geometric")
#'
#' @details 
#' The robust std method uses sqrt(sum(x^2)/(n-1)) to center the data before deriving "pretty" breaks.
#'
#' @return 
#' A vector containing class break values the length is n+1 to allow for 
#' specification of ranges
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples
#'  y <- rnbinom(100, 10, 0.5)
#'    classBreaks(y, 10)  
#'    classBreaks(y, 10, type="quantile")
#'  
#' opar <- par(no.readonly=TRUE)
#'    par(mfrow=c(2,2))
#'      d <- density(y)
#'      plot(d, type="n", main="Equal Area breaks")
#'        polygon(d, col="cyan")
#'        abline(v=classBreaks(y, 10)) 
#'      plot(d, type="n", main="Quantile breaks")
#'        polygon(d, col="cyan")
#'        abline(v=classBreaks(y, 10, type="quantile"))
#'      plot(d, type="n", main="Robust Standard Deviation breaks")
#'        polygon(d, col="cyan")
#'        abline(v=classBreaks(y, 10, type="std"))
#'      plot(d, type="n", main="Geometric interval breaks")
#'        polygon(d, col="cyan")
#'        abline(v=classBreaks(y, 10, type="geometric"))
#'  par(opar)
#' 	
#'  ( y.breaks <- classBreaks(y, 10) )   	
#'  cut(y, y.breaks, include.lowest = TRUE, labels = 1:10)
#'
#' @export
classBreaks <- function(x, n, type = c("equal", "quantile", "std", "geometric") ) {
  if(type[1] == "equal") { 
    return( seq(min(x), max(x), length.out=(n+1)) )
  } else if(type[1] == "quantile") {	
    return( c(stats::quantile(x=x, probs=seq(0,1,1/10))) )
  } else if(type[1] == "std") {
    svar <- scale(x)
    return( c((pretty(x = svar, n = n) * attr(svar, "scaled:scale")) + 
               attr(svar, "scaled:center")) )
  } else if(type[1] == "geometric") {
    breaks <- c(min(x), max(x))
      r <- exp((log(max(x)) - log(min(x)))/n)
        i.min <- breaks[1]
          for (i in 1:(n - 1)) {
            breaks <- c(breaks, i.min * r)
              i.min <- i.min * r
            breaks <- sort(breaks)
            }
    return(breaks)
  } else {
    stop("Not a valid statistic")
  }
}
