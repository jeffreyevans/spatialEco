#' @title Probability density plot by group
#'
#' @param y Numeric vector (density variable)
#' @param x Numeric, character or factorial vector of grouping 
#'          variable (must be same length as y)
#' @param col Optional line colors (see par, col)
#' @param lty Optional line types (see par, lty)
#' @param lwd Optional line widths (see par, lwd)
#' @param lx Position of legend (x coordinate or 'topright', 'topleft', 
#'           'bottomright', 'bottomleft') 
#' @param ly Position of legend (y coordinate)
#' @param ... Additional arguments passed to plot
#'
#' @description Creates a probability density plot of y for 
#'              each group of x 
#'
#' @return Plot of grouped PDF's
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'
#' @references
#' Simonoff, J. S. (1996). Smoothing Methods in Statistics. Springer-Verlag, New York.
#'
#' @examples 
#' y=dnorm(runif(100))
#' x=rep(c(1,2,3), length.out=length(y)) 
#' group.pdf(x=as.factor(x), y=y, main='Probability Density of y by group(x)', 
#' ylab='PDF', xlab='Y', lty=c(1,2,3))
#'
#' @export
group.pdf <- function(x, y, col = NULL, lty = NULL, lwd = NULL, 
                     lx = "topleft", ly = NULL, ...) {
  oops <- options() 
    on.exit(options(oops)) 
    if (!is.numeric(y)) 
        stop("y MUST BE NUMERIC")
    if (length(x) != length(y)) 
        stop("x AND y HAVE UNEQUAL LENGTHS")
    if (!is.factor(x)) 
        x <- as.factor(x)
    if (is.null(col)) 
        col <- seq(1:nlevels(x))
    if (is.null(lty)) 
        lty <- 1
    if (is.null(lwd)) 
        lwd <- 1.5
    ydat <- split(y, x)
    cden <- lapply(ydat, stats::density)
    xlim <- range(cden[[1]]$x, cden[[2]]$x)
    ylim <- range(cden[[1]]$y, cden[[2]]$y)
    graphics::plot(cden[[1]], type = "n", xlim = xlim, ylim = ylim, ...)
    for (i in 1:nlevels(x)) {
        graphics::lines(cden[[i]], col = col[i], lty = (if (length(lty) > 1) 
            lty[i] else lty), lwd = (if (length(lwd) > 1) 
            lwd[i] else lwd))
    }
    graphics::legend(lx, ly, names(cden), col = col, lty = lty, lwd = lwd)
} 
