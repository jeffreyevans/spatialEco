#' @title moments
#'
#' @description Calculate statistical moments of a distribution
#'
#' @param x numeric vector  
#' @param plot plot of distribution (TRUE/FALSE)
#'
#' @return A vector with the following values
#' @return     min Minimum           
#' @return     25th  25th percentile     
#' @return     mean  Arithmetic mean 
#' @return     gmean  Geometric mean
#' @return     hmean  Harmonic mean
#' @return     median  50th percentile      
#' @return     7th5  75th percentile      
#' @return     max  Maximum          
#' @return     stdv  Standard deviation         
#' @return     var  Variance     
#' @return     cv  Coefficient of variation (percent)         
#' @return     mad  Median absolute deviation         
#' @return     skew  Skewness    
#' @return     kurt  Kurtosis     
#' @return     nmodes  Number of modes   
#' @return     mode  Mode (dominate)
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'
#' @examples 
#'     x <- runif(1000,0,100)
#'     ( d <- moments(x, plot=TRUE) )
#'     ( mode.x <- moments(x, plot=FALSE)[16] )
#'  
#' @export 
moments <- function(x, plot = FALSE) {
    if (!length(x) >= 3) 
        stop("Not enought values to represent a distribution")
    .skew <- function(x, na.rm = FALSE) {
        if (na.rm) 
            x <- x[!is.na(x)]
        sum((x - mean(x))^3)/(length(x) * stats::sd(x)^3)
    }
    .kurt <- function(x, na.rm = FALSE) {
        if (na.rm) 
            x <- x[!is.na(x)]
        sum((x - mean(x))^4)/(length(x) * stats::var(x)^2) - 3
    }
    .cv <- function(x) {
        (stats::sd(x)/mean(x)) * 100
    }
    .means <- function(x) {
        if (any(x < 0)) 
            stop("need positive data")
        geometric <- function(x) {
            (exp(mean(log(x))))
        }
        harmonic <- function(x) {
            length(x)/sum(1/x)
        }
        arithmetic <- function(x) {
            sum(x)/length(x)
        }
        (x <- c(arithmetic(x), harmonic(x), geometric(x)))
    }
    .dmode <- function(x) {
        den <- stats::density(x, kernel = c("gaussian"))
        (den$x[den$y == max(den$y)])
    }
    .n.modes <- function(x) {
        den <- stats::density(x, kernel = c("gaussian"))
        den.s <- stats::smooth.spline(den$x, den$y, all.knots = TRUE, spar = 0.8)
        s.0 <- stats::predict(den.s, den.s$x, deriv = 0)
        s.1 <- stats::predict(den.s, den.s$x, deriv = 1)
        s.derv <- data.frame(s0 = s.0$y, s1 = s.1$y)
        nmodes <- length(rle(den.sign <- sign(s.derv$s1))$values)/2
        if ((nmodes > 10) == TRUE) {
            nmodes <- 10
        }
        if (is.na(nmodes) == TRUE) {
            nmodes <- 0
        }
        (nmodes)
    }
    r <- c(min(x), stats::quantile(x, 0.25, na.rm = TRUE), .means(x)[1], .means(x)[2], .means(x)[3], 
	       stats::quantile(x, 0.5, na.rm = TRUE), stats::quantile(x, 0.75, na.rm = TRUE), 
           max(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE), stats::var(x, na.rm = TRUE), .cv(x), 
		   stats::mad(x, na.rm = TRUE), .skew(x), .kurt(x), .n.modes(x), .dmode(x))
    names(r) <- c("min", "25th", "mean", "hmean", "gmean", "median", "75th", "max", "stdv", "var", "cv", "mad", "skew", 
        "kurt", "nmodes", "mode")
    if (plot == TRUE) {
        graphics::plot(stats::density(x), type = "n", main = "", ylab = "DENSITY", xlab = "RANGE", )
        graphics::polygon(stats::density(x), col = "blue")
        graphics::abline(v = min(x), lty = 1, lwd = 1, col = "black")
        graphics::abline(v = max(x), lty = 1, lwd = 1, col = "black")
        graphics::abline(v = stats::quantile(x, 0.25), lty = 2, lwd = 1, col = "black")
       graphics::abline(v = stats::quantile(x, 0.75), lty = 2, lwd = 1, col = "black")
        graphics::abline(v = .dmode(x), lty = 3, lwd = 1, col = "red")
        graphics::legend("topright", lty = c(1, 1, 2, 2, 3), lwd = c(1, 1, 1, 1, 1), bty = "n", legend = c("MIN", "MAX", "25th", 
            "75th", "MODE"), col = c("black", "black", "black", "black", "red"))
    }
    return(r)
} 
