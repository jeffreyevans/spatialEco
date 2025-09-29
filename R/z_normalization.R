#' @title z normalization 
#' @description Z-normalizes a time series by subtracting its mean and dividing by the standard deviation or meadian and MAD
#'
#' @param
#' x
#' method    Use the standard z normalization or modified version
#' na        Behavior of NA values, keep or remove
#'
#' @note 
#' The original method is to subtract x rom the mean then divide by sd whereas, modified uses the median and mad. 
#' For NA's if the argument is keep, an NA index is built and the NA's are re-inserted in to the resulting vector.
#' The behavior of remove will entirely remove them and result in a vector length different than the input.  
#' 
#' @return vector of z normalized data
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Iglewicz, B. & D.C. Hoaglin (1993) How to Detect and Handle Outliers, 
#'   American Society for Quality Control, Milwaukee, WI.
#'
#' @examples 
#'  # add data
#'  data(EuStockMarkets)
#'  d <- as.vector(EuStockMarkets[,1])
#' 
#' # Calculate Z score
#' summary(d)
#' summary(Z_org <- z_normalization(d) ) 
#' summary(Z_mod <- z_normalization(d, method="modified") )
#'   par(mfrow=c(3,1))
#'     plot(density(d), main="original timeseries")
#'     plot(density(Z_org), main="original z norm")
#'     plot(density(Z_mod), main="modified z norm")
#' 
#' # Check NA behavior, insert some NA's
#' d[c(100, 500, 1000, 1400)] <- NA
#' length(d)
#' length( z_normalization(d, na="keep") ) 
#' length( z_normalization(d, na="remove") ) 
#'  
#' @export
z_normalization <- function(x, method=c("original", "modified"), 
                            na = c("keep", "remove")) {
  if(!is.numeric(x))
    stop("x must be nuumeric vector")
  na.idx <- which(is.na(x))
    if(length(na.idx) > 0) x <- x[-na.idx]
  if(na[1] == "keep" & length(na.idx) > 0) {
    message(length(na.idx), " NA's found and will be retained")
  } else if(na[1] == "remove" & length(na.idx) > 0) {
	message(length(na.idx), " NA's found and will be removed")
  }
  if(method[1] == "original") {
    z <- (x - mean(x)) / stats::sd(x)
  } else if(method[1] == "modified") {
    z <- (0.6745 * (x - stats::median(x))) / stats::mad(x)
  }
  if(na[1] == "keep" & length(na.idx) > 0) {
    zv <- z
      z <- numeric(length(zv) + length(na.idx))
        z[na.idx] <- NA
          z[-na.idx] <- zv
  }  
  return(z)
}
