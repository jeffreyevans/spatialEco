#' @title trend.line
#' @description Calculated specified trend line of x,y
#'
#' @param x      Vector of x
#' @param y      Vector of y
#' @param type   Trend line types are: 'linear', 'exponential', 
#'               'logarithmic', 'polynomial'
#' @param plot   plot results (TRUE/FALSE)
#' @param ...    Additional arguments passed to plot 
#'
#' @return A list class object with the following components:
#' * for type = 'linear'  x is slope and y is intercept
#' * for type = 'exponential', 'logarithmic', or 'polynomial'
#'       x is original x variable and y is vector of fit 
#'       regression line
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' x <- 1:10
#' y <- jitter(x^2)
#'
#' opar <- par(no.readonly=TRUE)
#'   par(mfcol=c(2,2))
#'     trend.line(x,y,type='linear',plot=TRUE,pch=20,main='Linear')
#'     trend.line(x,y,type='exponential',plot=TRUE,pch=20,main='Exponential')
#'     trend.line(x,y,type='logarithmic',plot=TRUE,pch=20,main='Logarithmic')
#'     trend.line(x,y,type='polynomial',plot=TRUE,pch=20,main='Polynomial')
#'  par(opar)
#' 
#' @export  
trend.line <- function(x, y, type = "linear", plot = TRUE, ...) {
    if (type == "linear") {
        fit <- stats::glm(y ~ x)
        trend <- c(as.numeric(stats::coef(fit)[1]), as.numeric(stats::coef(fit)[2]))
    }
    if (type == "exponential") {
        exp.f <- function(x, a, b) {
            a * exp(b * x)
        }
        fit <- stats::nls(y ~ exp.f(x, a, b), start = c(a = 1, b = 1))
        trend <- exp.f(x, a = stats::coef(fit)[1], b = stats::coef(fit)[2])
    }
    if (type == "logarithmic") {
        log.f <- function(x, a, b) {
            a * log(x) + b
        }
        fit <- stats::nls(y ~ log.f(x, a, b), start = c(a = 1, b = 1))
        trend <- log.f(x, a = stats::coef(fit)[1], b = stats::coef(fit)[2])
    }
    if (type == "polynomial") {
        poly.f <- function(x, a, b, d) {
            (a * x^2) + (b * x) + d
        }
        fit <- stats::nls(y ~ poly.f(x, a, b, d), start = c(a = 1, b = 1, d = 1))
        trend <- poly.f(x, a = stats::coef(fit)[1], b = stats::coef(fit)[2], d = stats::coef(fit)[3])
    }
    if (plot == TRUE) {
        graphics::plot(x, y, ...)
        if (type == "linear") {
            graphics::abline(a = trend[1], b = trend[2], col = "black", lwd = 2)
        } else {
            graphics::lines(x = x, y = trend, col = "black", lwd = 2)
        }
    }
    if (type == "linear") 
        return(list(x = trend[1], y = trend[2])) else return(list(x = x, y = trend))
} 
