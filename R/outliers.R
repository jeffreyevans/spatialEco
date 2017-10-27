#' @title Outliers 
#' @description Identify outliers using modified Z-score
#'
#' @param x          A numeric vector
#' @param s          Scaling factor for mad statistic 
#'
#' @return value for the modified Z-score
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#'  Iglewicz, B. & D.C. Hoaglin (1993) How to Detect and Handle Outliers, American Society for Quality Control, Milwaukee, WI.
#'
#' @examples 
#'  # Create data with 3 outliers
#'     x <- seq(0.1, 5, length=100) 
#'     x[98:100] <- c(100, 55, 250)
#'  
#'  # Calculate Z score
#'      Z <- outliers(x) 
#'  
#'  # Show number of extreme outliers using Z-score
#'      length(Z[Z > 9.9])
#'  
#'  # Remove extreme outliers 
#'      x <- x[-which(Z > 9.9)]
#'
#' @export  
outliers <- function(x, s = 1.4826) {
 e <- (length(x) - 1) / sqrt(length(x)) 
 mad <- function (x, center=stats::median(x), constant=s,
                  low=FALSE, high=FALSE) {
  n <- length(x)
  constant * if ((low || high) && n%%2 == 0) {
      if (low && high) 
          stop("'low' and 'high' cannot be both TRUE")
      n2 <- n%/%2 + as.integer(high)
      sort(abs(x - center), partial = n2)[n2]
    }
   else stats::median(abs(x - center))
  }                         
   return( ( (0.6745 * (x - stats::median(x))) / mad(x) ) )
}
