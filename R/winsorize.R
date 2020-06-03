#' @title Winsorize transformation 
#' @description Removes extreme outliers using a winsorization transformation
#' 
#' @param x           A numeric vector
#' @param min.value   A fixed lower bounds, all values lower than this will be 
#'                    replaced by this value. The default is set to the 5th-quantile 
#'                    of x.
#' @param max.value   A fixed upper bounds, all values higher than this will be replaced 
#'                    by this value. The default is set to the 95th-quantile of x.
#' @param p           A numeric vector of 2 representing the probabilities used in the 
#'                    quantile function. 
#' @param na.rm       (FALSE/TRUE) should NAs be omitted? 
#' 
#' @return 
#' A transformed vector the same length as x, unless na.rm is TRUE, then x is length 
#' minus number of NA's
#' 
#' @description 
#' Winsorization is the transformation of a distribution by limiting extreme values 
#' to reduce the effect of spurious outliers. This is done by shrinking outlying 
#' observations to the border of the main part of the distribution. 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references 
#' Dixon, W.J. (1960) Simplified Estimation from Censored Normal Samples. Annals of Mathematical 
#'   Statistics. 31(2):385-391 
#' 
#' @examples
#' set.seed(1234)     
#' x <- rnorm(100)     
#' x[1] <- x[1] * 10  
#' winsorize(x)       
#' 
#' plot(x, type="l", main="Winsorization transformation")
#'   lines(winsorize(x), col="red", lwd=2)
#'     legend("bottomright", legend=c("Original distribution","With outliers removed"),
#'	        lty=c(1,1), col=c("black","red"))
#' 
#' # Behavior with NA value(s)
#' x[4] <- NA
#' winsorize(x)             # returns x with original NA's 
#' winsorize(x, na.rm=TRUE) # removes NA's 
#' 
#' @export winsorize
winsorize <- function(x, min.value = NULL, max.value = NULL,  
                      p = c(0.05, 0.95), na.rm = FALSE) {			  
    if(na.rm == TRUE) { 
      x <- x[!is.na(x)]
    } else {
	  na.idx <- which(is.na(x) == TRUE)
	    if(length(na.idx) > 0) x <- x[-na.idx]
    }	 
	if (is.null(min.value) || is.null(max.value)) {
      xq <- stats::quantile(x = x, probs = p, na.rm = TRUE)
        if (is.null(min.value)) min.value <- xq[1]
          if (is.null(max.value)) max.value <- xq[2]
    }
      x[x < min.value] <- min.value
      x[x > max.value] <- max.value
	if(na.rm == FALSE) { x[na.idx] <- NA }
  return(x)
}
