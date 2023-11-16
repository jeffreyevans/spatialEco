#' @title Pseudo-absence random samples 
#' @description Generates pseudo-absence samples based on density estimate 
#'              of known locations 
#'
#' @param x         An sf POINT geometry object
#' @param n         Number of random samples to generate
#' @param window    Type of window (hull OR extent), overridden if mask provided
#' @param ref       Optional terra SpatRaster class raster. The resolution of the 
#'                  density estimate will match mask.        
#' @param s         Optional resolution passed to window argument. Caution should be 
#'                  used due to long processing times associated with high 
#'                  resolution. In contrast, coarse resolution can exclude 
#'                  known points.            
#' @param sigma     Bandwidth selection method for KDE, default is 'Scott'. 
#'                  Options are 'Scott', 'Stoyan', 'Diggle', 'likelihood', 
#'                  and 'geometry'
#' @param wts       Optional vector of weights corresponding to point pattern
#' @param KDE       Return KDE raster (TRUE/FALSE)
#' @param gradient  A scaling factor applied to the sigma parameter used to 
#'                  adjust the gradient decent of the density estimate. The 
#'                  default is 1, for no adjustment (downweight < 1 | upweight > 1)   
#' @param p         Minimum value for probability distribution (must be >  0)
#' @param edge      Apply Diggle edge correction (TRUE/FALSE)
#'
#' @details
#' The window type creates a convex hull by default or, optionally, uses the 
#' maximum extent (envelope). If a mask is provided the kde will represent 
#' areas defined by the mask and defines the area that pseudo absence data 
#' will be generated.
#' 
#' Available bandwidth selection methods are:
#'   * Scott (Scott 1992), Scott's Rule for Bandwidth Selection (1st order)
#'   * Diggle (Berman & Diggle 1989), Minimize the mean-square error via cross 
#'   * validation (2nd order)  
#'   * likelihood (Loader 1999), Maximum likelihood cross validation (2nd order)
#'   * geometry, Bandwidth is based on simple window geometry (1st order)
#'   * Stoyan (Stoyan & Stoyan 1995), Based on pair-correlation function (strong 2nd order)
#'   * User defined numeric distance bandwidth
#' @md
#'
#' @note
#' resulting bandwidth can vary widely by method. the 'diggle' method 
#' is intended for selecting bandwidth representing 2nd order spatial variation 
#' whereas the 'scott' method will represent 1st order trend. the 'geometry' approach 
#' will also represent 1st order trend. For large datasets, caution should be used with 
#' the 2nd order 'likelihood' approach, as it is slow and computationally expensive. 
#' finally, the 'stoyan' method will produce very strong 2nd order results.
#' 
#' @return 
#' A list class object with the following components:
#'   * sample  A sf POINT geometry object containing random samples
#'   * kde     A terra SpatRaster class of inverted Isotropic KDE estimates 
#'             used as sample weights (IF KDE = TRUE)
#'   * sigma   Selected bandwidth of KDE 
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references
#' Berman, M. and Diggle, P. (1989) Estimating weighted integrals of the second-order 
#'   intensity of a spatial point process. Journal of the Royal Statistical Society, 
#'   series B 51, 81-92. 
#' 
#' Fithian, W & T. Hastie (2013) Finite-sample equivalence in statistical models for 
#'   presence-only data. Annals of Applied Statistics 7(4): 1917-1939
#' 
#' Hengl, T., H. Sierdsema, A. Radovic, and A. Dilo (2009) Spatial prediction of species 
#'   distributions from occurrence-only records: combining point pattern analysis, 
#'   ENFA and regression-kriging. Ecological Modelling, 220(24):3499-3511  
#' 
#' Loader, C. (1999) Local Regression and Likelihood. Springer, New York. 
#' 
#' Scott, D.W. (1992) Multivariate Density Estimation. Theory, Practice and Visualization. 
#'   New York, Wiley. 
#' 
#' Stoyan, D. and Stoyan, H. (1995) Fractals, random shapes and point fields: methods of 
#'   geometrical statistics. John Wiley and Sons. 
#' 
#' Warton, D.i., and L.C. Shepherd (2010) Poisson Point Process Models Solve the Pseudo-Absence 
#'   Problem for Presence-only Data in Ecology. The Annals of Applied Statistics, 4(3):1383-1402
#'
#' @examples
#'  p = c("sf", "sp", "terra", "spatstat.data")
#'  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
#'    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
#'    message("Can't run examples, please install ", paste(p[m], collapse = " "))
#'  } else {
#'    invisible(lapply(p, require, character.only=TRUE))
#'   
#' data(meuse, package = "sp")
#' meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                   agr = "constant") 
#' 
#' # Using a raster mask   
#' r <- rast(ext(meuse), resolution=40, crs=crs(meuse))
#'   r[] <- rep(1,ncell(r))
#'   
#' pa <- pseudo.absence(meuse, n=100, window='hull', KDE=TRUE, ref = r, 
#'                      sigma='Diggle', s=50) 
#'   col.br <- colorRampPalette(c('blue','yellow'))
#'     plot(pa$kde, col=col.br(10))
#'       plot(st_geometry(meuse), pch=20, cex=1, add=TRUE)
#'         plot(st_geometry(pa$sample), col='red', pch=20, cex=1, add=TRUE)
#'           legend('top', legend=c('Presence', 'Pseudo-absence'), 
#'                  pch=c(20,20),col=c('black','red'), bg="white")
#' 
#' # With clustered data
#' data(bei, package = "spatstat.data")
#'   trees <- st_as_sf(bei)
#'     trees <- trees[-1,]
#' 
#' trees.abs <- pseudo.absence(trees, n=100, window='extent', KDE=TRUE)
#'   col.br <- colorRampPalette(c('blue','yellow'))
#'     plot(trees.abs$kde, col=col.br(10))
#'      plot(st_geometry(trees), pch=20, cex=0.50, add=TRUE)
#'         plot(st_geometry(trees.abs$sample), col='red', pch=20, cex=1, add=TRUE)
#'           legend('top', legend=c('Presence', 'Pseudo-absence'), 
#'                  pch=c(20,20),col=c('black','red'),bg="white")
#' }     
#' @export     
pseudo.absence <- function(x, n, window = "hull", ref = NULL, s = NULL, sigma = "Scott", 
                           wts = NULL, KDE = FALSE, gradient = 1, p = NULL, edge = FALSE) {

  if(!inherits(x, c("sf", "sfc")))	
    stop(deparse(substitute(x)), " must be a sf, or sfc object")
  if(unique(as.character(sf::st_geometry_type(x))) != "POINT")
      stop(deparse(substitute(x)), " must be single-part POINT geometry") 
  if (!is.null(ref)) {
    if(!inherits(ref, "SpatRaster")) 
      stop(deparse(substitute(ref)), " must be a terra SpatRaster object")
  }
  if (is.null(p)) p <- 1e-09
    a <- 10000    
    raster.as.im <- function(im) {
	  r <- terra::res(im)[1]
      orig <- as.numeric(sf::st_bbox(im)) + 0.5 * r
      dm <- dim(im)[2:1]
        xx <- unname(orig[1] + cumsum(c(0, rep(r[1], dm[1] - 1))))
        yy <- unname(orig[2] + cumsum(c(0, rep(r[1], dm[2] - 1))))
      return(spatstat.geom::im(matrix(terra::values(im), ncol = dm[1], 
             nrow = dm[2], byrow = TRUE)[dm[2]:1, ], 
		     xcol = xx, yrow = yy))
    }
	if (is.null(ref)) {
      if (window == "hull") {
        win <- spatstat.geom::convexhull.xy(sp::coordinates(x))
        win <- spatstat.geom::as.mask(win, eps = s)
        }
      if (window == "extent") {
        e <- as.vector(sf::st_bbox(x))
        win <- spatstat.geom::as.owin(c(e[1], e[3], e[2], e[4]))
        win <- spatstat.geom::as.mask(win, eps = s)
        }
    } else {
        win <- raster.as.im(ref)
        win <- spatstat.geom::as.mask(win, eps = terra::res(ref)[1])
    }	
    x.ppp <- suppressWarnings(
      spatstat.geom::as.ppp(sf::st_coordinates(x)[,1:2], win))
	  
    bw.Scott <- function(X) {
        stopifnot(spatstat.geom::is.ppp(X))
        n <- spatstat.geom::npoints(X)
        sdx <- sqrt(stats::var(X$x))
        sdy <- sqrt(stats::var(X$y))
        return(c(sdx, sdy) * n^(-1/6))
    }
    bw.Stoyan <- function(X, co = 0.15) {
        stopifnot(spatstat.geom::is.ppp(X))
        n <- spatstat.geom::npoints(X)
        W <- spatstat.geom::as.owin(X)
        a <- spatstat.geom::area.owin(W)
        stoyan <- co/sqrt(5 * n/a)
        return(stoyan)
    }
    bw.geometry <- function(X, f = 1/4) {
        X <- spatstat.geom::as.owin(X)
        g <- spatstat.explore::distcdf(X)
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
        stopifnot(spatstat.geom::is.ppp(X))
        if (!is.null(srange)) 
            check.range(srange) else {
            nnd <- spatstat.geom::nndist(X)
            srange <- c(min(nnd[nnd > 0]), spatstat.geom::diameter(spatstat.geom::as.owin(X))/2)
        }
        sigma <- exp(seq(log(srange[1]), log(srange[2]), length = ns))
        cv <- numeric(ns)
        for (i in 1:ns) {
            si <- sigma[i]
            lamx <- spatstat.explore::density.ppp(X, sigma = si, at = "points", leaveoneout = TRUE)
            lam <- spatstat.explore::density.ppp(X, sigma = si)
            cv[i] <- sum(log(lamx)) - spatstat.geom::integral.im(lam)
        }
      result <- spatstat.explore::bw.optim(cv, sigma, iopt = which.max(cv), 
	                    criterion = "Likelihood Cross-Validation")
    return(result)
    }
    if (sigma == "Diggle") {
        bw <- spatstat.explore::bw.diggle(x.ppp)
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
    den <- terra::rast(spatstat.explore::density.ppp(x.ppp, weights = wts,  
                       sigma = bw, adjust = gradient, diggle = edge)) * a
        den <- 1 - (den/terra::global(den,"max",na.rm=TRUE)[,1])
          den[den <= p] <- p
    den.sp <- sf::st_as_sf(terra::as.points(den))
      names(den.sp)[1] <- "KDE"
        den.sp <- den.sp[sample(1:nrow(den.sp), size = n, prob = den.sp$KDE), ]
    if (KDE == TRUE) {
      return(list(sample = den.sp, kde = den, sigma = bw))
    } else {
      return(list(sample = den.sp, sigma = bw) )
    }
}
