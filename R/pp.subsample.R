#' @title Point process random subsample
#' @description Generates random subsample based on density estimate of observations
#'
#' @param x An sp class SpatialPointsDataFrame or SpatialPoints object
#' @param n Number of random samples to generate
#' @param window Type of window (hull or extent)
#' @param sigma Bandwidth selection method for KDE, default is 'Scott'. Options are 'Scott', 'Stoyan', 'Diggle', 'likelihood', and 'geometry'
#' @param wts Optional vector of weights corresponding to point pattern
#' @param gradient A scaling factor applied to the sigma parameter used to adjust the gradient decent of the density estimate. The default is 1, for no adjustment (downweight < 1 | upweight > 1)   
#' @param edge Apply Diggle edge correction (TRUE/FALSE)
#'
#' @return sp class SpatialPointsDataFrame containing random subsamples
#'
#' @note
#' The window type creates a convex hull by default or, optionally, uses the maximum extent (envelope). 
#' 
#' Available bandwidth selection methods are:
#'   Scott (Scott 1992), Scott's Rule for Bandwidth Selection (1st order)
#'   Diggle (Berman & Diggle 1989), Minimise the mean-square error via cross validation (2nd order)  
#'   likelihood (Loader 1999), Maximum likelihood cross validation (2nd order)
#'   geometry, Bandwidth is based on simple window geometry (1st order)
#'   Stoyan (Stoyan & Stoyan 1995), Based on pair-correlation function (strong 2nd order)
#'
#' Note; resulting bandwidth can vary widly by method. the 'diggle' method is intended for selecting bandwidth representing 2nd order spatial variation whereas the 'scott' method will represent 1st order trend. the 'geometry' approach will also represent 1st order trend. for large datasets, caution should be used with the 2nd order 'likelihood' approach, as it is slow and computationally expensive. finally, the 'stoyan' method will produce very strong 2nd order results. '
#'
#' @note Depends: sp, spatstat
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Berman, M. and Diggle, P. (1989) Estimating weighted integrals of the second-order intensity of a spatial point process. Journal of the Royal Statistical Society, series B 51, 81-92. 
#' Berman, M. and Diggle, P. (1989) Estimating weighted integrals of the second-order intensity of a spatial point process. Journal of the Royal Statistical Society, series B 51, 81-92. 
#' @references
#' Fithian, W & T. Hastie (2013) Finite-sample equivalence in statistical models for presence-only data. Annals of Applied Statistics 7(4): 1917-1939
#' @references
#' Hengl, T., H. Sierdsema, A. Radovic, and A. Dilo (2009) Spatial prediction of species distributions from occurrence-only records: combining point pattern analysis, ENFA and regression-kriging. Ecological Modelling, 220(24):3499-3511  
#' @references
#' Loader, C. (1999) Local Regression and Likelihood. Springer, New York. 
#' @references
#' Scott, D.W. (1992) Multivariate Density Estimation. Theory, Practice and Visualization. New York, Wiley. 
#' @references
#' Stoyan, D. and Stoyan, H. (1995) Fractals, random shapes and point fields: methods of geometrical statistics. John Wiley and Sons. 
#' @references
#' Warton, D.i., and L.C. Shepherd (2010) Poisson Point Process Models Solve the Pseudo-Absence Problem for Presence-only Data in Ecology. The Annals of Applied Statistics, 4(3):1383-1402
#'
#' @examples  
#' require(spatstat)
#' require(sp) 
#' data(bei)  
#'   trees <- as(bei, 'SpatialPoints')
#'     n=round(length(trees) * 0.10, digits=0)  
#'       trees.wrs <- pp.subsample(trees, n=n, window='hull')   
#'         plot(trees, pch=19, col='black')
#'           plot(trees.wrs, pch=19, col='red', add=TRUE) 
#'             box()
#'              title('10% subsample')
#'           legend('bottomright', legend=c('Original sample', 'Subsample'), 
#'                  col=c('black','red'),pch=c(19,19))   
#'
#' @export
pp.subsample <- function(x, n, window = "hull", sigma = "Scott", wts = NULL, gradient = 1, edge = FALSE) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
    if (is.null(window)) 
        stop("Please specify a valid window type hull | extent")
    bw.Scott <- function(X) {
        stopifnot(spatstat::is.ppp(X))
        n <- spatstat::npoints(X)
        sdx <- sqrt(stats::var(X$x))
        sdy <- sqrt(stats::var(X$y))
        return(c(sdx, sdy) * n^(-1/6))
    }
    bw.Stoyan <- function(X, co = 0.15) {
        stopifnot(spatstat::is.ppp(X))
        n <- spatstat::npoints(X)
        W <- spatstat::as.owin(X)
        a <- spatstat::area.owin(W)
        stoyan <- co/sqrt(5 * n/a)
        return(stoyan)
    }
    bw.geometry <- function(X, f = 1/4) {
        X <- spatstat::as.owin(X)
        g <- spatstat::distcdf(X)
        r <- with(g, .x)
        Fr <- with(g, .y)
        iopt <- min(which(Fr >= f))
        return(r[iopt])
    }
    bw.likelihood <- function(X, srange = NULL, ns = 16) {
	  check.range <- function (x, fatal = TRUE) {
        xname <- deparse(substitute(x))
        if (is.numeric(x) && identical(x, range(x, na.rm = TRUE))) 
          return(TRUE)
        if (fatal) 
          stop(paste(xname, "should be a vector of length 2 giving (min, max)"))
        return(FALSE)
      }
        stopifnot(spatstat::is.ppp(X))
        if (!is.null(srange)) 
            check.range(srange) else {
            nnd <- spatstat::nndist(X)
            srange <- c(min(nnd[nnd > 0]), spatstat::diameter(spatstat::as.owin(X))/2)
        }
        sigma <- exp(seq(log(srange[1]), log(srange[2]), length = ns))
        cv <- numeric(ns)
        for (i in 1:ns) {
            si <- sigma[i]
            lamx <- spatstat::density.ppp(X, sigma = si, at = "points", leaveoneout = TRUE)
            lam <- spatstat::density.ppp(X, sigma = si)
            cv[i] <- sum(log(lamx)) - spatstat::integral.im(lam)
        }
        result <- spatstat::bw.optim(cv, sigma, iopt = which.max(cv), criterion = "Likelihood Cross-Validation")
        return(result)
    }
    if (window == "hull") {
        win <- spatstat::convexhull.xy(sp::coordinates(x))
    } else {
        if (window == "extent") {
            e <- as.vector(sp::bbox(x))
            win <- spatstat::as.owin(c(e[1], e[3], e[2], e[4]))
        }
    }
    x.ppp <- spatstat::as.ppp(sp::coordinates(x), win)
    if (sigma == "Diggle") {
        bw <- spatstat::bw.diggle(x.ppp)
    } else {
        if (sigma == "Scott") {
            bw <- bw.Scott(x.ppp)
        } else {
            if (sigma == "Stoyan") {
                bw <- bw.Stoyan(x.ppp)
            } else {
                if (sigma == "geometry") {
                  bw <- bw.geometry(x.ppp)
                } else {
                  if (sigma == "likelihood") {
                    bw <- bw.likelihood(x.ppp)
                  } else {
                    if (is.numeric(sigma)) {
                      bw <- sigma
                    }
                  }
                }
            }
        }
    }
    den <- spatstat::density.ppp(x.ppp, weights = wts, sigma = bw, adjust = gradient, diggle = edge, at = "points")
    point.den <- data.frame(X = x.ppp$x, Y = x.ppp$y, KDE = as.vector(den * 10000))
    point.den$KDE <- point.den$KDE/max(point.den$KDE)
    point.den <- point.den[order(point.den[["KDE"]]), ]
    point.den$KDE <- rev(point.den$KDE)
    sp::coordinates(point.den) <- ~X + Y
    point.rs <- point.den[sample(seq(1:nrow(point.den)), n, prob = point.den@data$KDE), ]
    point.rs
} 
