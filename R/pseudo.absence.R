#' @title Pseudo-absence random samples 
#' @description Generates pseudo-absence samples based on density estimate of known locations 
#'
#' @param x An sp class SpatialPointsDataFrame or SpatialPoints object
#' @param n Number of random samples to generate
#' @param window Type of window (hull OR extent), overridden if mask provided
#' @param Mask Optional rasterLayer class mask raster. The resolution of the density estimate will match mask.        
#' @param s Optional resolution passed to window argument. Caution should be used due to long processing times associated with high resolution. In contrast, coarse resolution can exclude known points.            
#' @param sigma Bandwidth selection method for KDE, default is 'Scott'. Options are 'Scott', 'Stoyan', 'Diggle', 'likelihood', and 'geometry'
#' @param wts Optional vector of weights corresponding to point pattern
#' @param KDE save KDE raster (TRUE/FALSE)
#' @param gradient A scaling factor applied to the sigma parameter used to adjust the gradient decent of the density estimate. The default is 1, for no adjustment (downweight < 1 | upweight > 1)   
#' @param p Minimum value for probability distribution (must be >  0)
#' @param edge Apply Diggle edge correction (TRUE/FALSE)
#' 
#' @return A list class object with the following components:
#' \itemize{ 
#' \item   sample SpatialPointsDataFrame containing random samples
#' \item   kde sp RasterLayer class of KDE estimates (IF KDE = TRUE)
#' \item   sigma Selected bandwidth of KDE 
#'  }
#'
#' @details
#' The window type creates a convex hull by default or, optionally, uses the maximum extent (envelope). If a mask is provided the kde will represent areas defined by the mask and defines the area that pseudo absence data will be generated.
#' Available bandwidth selection methods are:
#' \itemize{
#' \item   Scott (Scott 1992), Scott's Rule for Bandwidth Selection (1st order)
#' \item   Diggle (Berman & Diggle 1989), Minimize the mean-square error via cross validation (2nd order)  
#' \item   likelihood (Loader 1999), Maximum likelihood cross validation (2nd order)
#' \item   geometry, Bandwidth is based on simple window geometry (1st order)
#' \item   Stoyan (Stoyan & Stoyan 1995), Based on pair-correlation function (strong 2nd order)
#'  }
#'
#' Note; resulting bandwidth can vary widely by method. the 'diggle' method is intended for selecting bandwidth representing 2nd order spatial variation whereas the 'scott' method will represent 1st order trend. the 'geometry' approach will also represent 1st order trend. for large datasets, caution should be used with the 2nd order 'likelihood' approach, as it is slow and computationally expensive. finally, the 'stoyan' method will produce very strong 2nd order results.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references
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
#'  library(raster) 
#'  library(sp)
#'  data(meuse)
#'  data(meuse.grid)
#'    coordinates(meuse) = ~x+y   
#'    coordinates(meuse.grid) = ~x+y
#'    proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#'    gridded(meuse.grid) = TRUE
#'    r <- raster(meuse.grid)
#'   
#'    pa <- pseudo.absence(meuse, n=100, window='hull', KDE=TRUE, Mask = r, 
#'                         sigma='Diggle', s=50) 
#'      col.br <- colorRampPalette(c('blue','yellow'))
#'        plot(pa$kde, col=col.br(10))
#'          plot(meuse, pch=20, cex=1, add=TRUE)
#'            plot(pa$sample, col='red', pch=20, cex=1, add=TRUE)
#'              legend('top', legend=c('Presence', 'Pseudo-absence'), 
#'                     pch=c(20,20),col=c('black','red'))
#'
#'  # With clustered data
#'  library(sp)
#'  library(spatstat)
#'  data(bei)
#'    trees <- as(bei, 'SpatialPoints')
#'      trees <- SpatialPointsDataFrame(coordinates(trees), 
#'                          data.frame(ID=1:length(trees))) 
#'        trees.abs <- pseudo.absence(trees, n=100, window='extent', KDE=TRUE)
#'  
#'  col.br <- colorRampPalette(c('blue','yellow'))
#'    plot(trees.abs$kde, col=col.br(10))
#'     plot(trees, pch=20, cex=0.50, add=TRUE)
#'        plot(trees.abs$sample, col='red', pch=20, cex=1, add=TRUE)
#'          legend('top', legend=c('Presence', 'Pseudo-absence'), 
#'                 pch=c(20,20),col=c('black','red'))
#'      
#' @export     
pseudo.absence <- function(x, n, window = "hull", Mask = NULL, s = NULL, sigma = "Scott", wts = NULL, 
                           KDE = FALSE, gradient = 1, p = NULL, edge = FALSE) {
    if (!class(x) == "SpatialPointsDataFrame" & !class(x) == "SpatialPoints") 
        stop(deparse(substitute(x)), " MUST BE A sp POINTS OBJECT")
    if (!is.null(Mask)) {
        if (!class(Mask) == "RasterLayer") 
            stop(deparse(substitute(Mask)), " MUST BE A RasterLayer OBJECT")
    }
    if (is.null(p)) p <- 1e-09
      a <- 10000
      options(warn = -1)
      raster.as.im <- function(im) {
                        spatstat::as.im(as.matrix(im)[nrow(im):1, ], 
	                                    xrange = sp::bbox(im)[1, ], 
	  				                    yrange = sp::bbox(im)[2, ])
      }
	  
	if (is.null(Mask)) {
      if (window == "hull") {
        win <- spatstat::convexhull.xy(sp::coordinates(x))
        win <- spatstat::as.mask(win, eps = s)
        }
      if (window == "extent") {
        e <- as.vector(sp::bbox(x))
        win <- spatstat::as.owin(c(e[1], e[3], e[2], e[4]))
        win <- spatstat::as.mask(win, eps = s)
        }
    } else {
        win <- raster.as.im(Mask)
        win <- spatstat::as.mask(win, eps = raster::res(Mask)[1])
    }
	
  x.ppp <- spatstat::as.ppp(sp::coordinates(x), win)
	
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
	
    if (sigma == "Diggle") {
        bw <- spatstat::bw.diggle(x.ppp)
      } else if(sigma == "Scott") { 
          bw <- bw.Scott(x.ppp)
        } else if(sigma == "Stoyan") {
            bw <- bw.Stoyan(x.ppp)
          } else if(sigma == "geometry") {
              bw <- bw.geometry(x.ppp)
            } else if(sigma == "likelihood") {
                bw <- bw.likelihood(x.ppp)
              } else if(is.numeric(sigma)) {
                  bw = sigma
                } else {
	              stop("Not a valid bandwidth option")
                }          
      den <- raster(spatstat::density.ppp(x.ppp, weights = wts, sigma = bw, 
                                   adjust = gradient, diggle = edge)) * a

      den <- 1 - (den/raster::maxValue(den))
      den[den <= p] <- p
      den.sp <- raster::rasterToPoints(den, spatial=TRUE)
        names(den.sp@data)[1] <- "KDE"
      den.sp <- den.sp[sample(1:nrow(den.sp@data), size = n, prob = den.sp@data$KDE), ]
    if (KDE == TRUE) {
      return(list(sample = den.sp, kde = den, sigma = bw))
    } else {
      return(list(sample = den.sp), sigma = bw)
    }
}
