#' @title Spatial kernel density estimate
#' @description A weighted or unweighted Gaussian Kernel Density estimate for spatial data
#'
#' @param x             sp SpatialPointsDataFrame object
#' @param y             Optional values, associated with x coordinates, to be used as weights
#' @param bw            Distance bandwidth of Gaussian Kernel, must be units of projection
#' @param newdata       A Rasterlayer, any sp class object or c[xmin,xmax,ymin,ymax] vector to estimate the kde extent
#' @param n             Number of cells used in creating grid. If not defined a value based on extent or existing raster will be used
#' @param standardize   Standardize results to 0-1 (FALSE/TRUE)
#' @param scale.factor  Optional numeric scaling factor for the KDE (eg., 10000), to account for small estimate values
#' @param mask          (TRUE/FALSE) mask resulting raster if newdata is provided
#'
#' @return  Raster class object containing kernel density estimate 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \dontrun{ 
#'  library(sp)
#'    data(meuse)
#'    coordinates(meuse) <- ~x+y
#'  
#'  # Weighted KDE using cadmium and spatial locations 
#'  cadmium.kde <- sp.kde(x = meuse, y = meuse$cadmium, bw = 1000, n = 5000, 
#'                        standardize = TRUE, scale.factor = 10000  )
#'  				
#'  # Unweighted KDE (spatial locations only)				
#'  pt.kde <- sp.kde(x = meuse, bw = 1000, standardize = TRUE, n = 5000, 
#'                   scale.factor = 10000  )
#'  
#'  # Plot results
#'  par(mfrow=c(1,2))
#'    plot(cadmium.kde, main="weighted kde")
#'      points(meuse, pch=20, col="red")
#'    plot(pt.kde, main="Unweighted kde")
#'      points(meuse, pch=20, col="red") 
#'  
#'  # Using existing raster
#'  library(raster)
#'  data(meuse.grid)
#'  coordinates(meuse.grid) = ~x+y
#'  proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#'  gridded(meuse.grid) = TRUE
#'  meuse.grid <- raster(meuse.grid)
#'  
#'  cadmium.kde <- sp.kde(x = meuse, y = meuse$cadmium, newdata = meuse.grid, bw = 1000, 
#'                        standardize = TRUE, scale.factor = 10000)
#'    plot(cadmium.kde, main="weighted kde")
#'      points(meuse, pch=20, cex=0.5, col="red")
#' }
#'
#' @export
sp.kde <- function(x, y = NULL, bw = NULL, newdata = NULL, n = NULL,  
                   standardize = FALSE, scale.factor, mask = TRUE) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
  if(is.null(bw)){ 
    bw <- c(MASS::bandwidth.nrd(sp::coordinates(x)[,1]), 
	        MASS::bandwidth.nrd(sp::coordinates(x)[,2]))
	  cat("Using", bw, "for bandwidth", "\n")
  } else {
    bw <- c(bw,bw)
  } 
    if(is.null(newdata)) { 
	  ext <- as.vector(raster::extent(x)) 
	    if(is.null(n)) {
		  newdata <- raster::raster(raster::extent(ext))
          n <- c(raster::nrow(newdata), raster::ncol(newdata))		
		  warning(paste0("n not defined, defaulting to ", raster::ncell(newdata), " values"))
        } else {
		  newdata <- raster::raster(raster::extent(ext), nrow=n/2, ncol=n/2)
		  n = c(raster::nrow(newdata),raster::ncol(newdata))
		}	
    } else if(!is.null(newdata)) {
      if( class(newdata) == "numeric") {
        if(length(newdata) != 4) stop("Need xmin, xmax, ymin, ymax coordinates")
          ext <- raster::raster(raster::extent(newdata), nrow=n/2, ncol=n/2)		
		    ext[] <- rep(1, raster::ncell(ext))
	      if(missing(n)) {
            n <- c(raster::nrow(ext), raster::ncol(ext))		
	  	  warning(paste0("n not defined, defaulting to ", raster::ncell(newdata), " values"))
          } else {
	  	  raster::nrow(ext) <- n/2
	  	  raster::ncol(ext) <- n/2
		  n <- c(n/2,n/2)
	  	  }
	  } else if(class(newdata) == "RasterLayer") { 
	      ext <- as.vector(raster::extent(newdata))
        if(missing(n)) {		  
	      n = c(raster::nrow(newdata), raster::ncol(newdata)) 
	    } else {
		  raster::nrow(newdata) <- n/2
	  	  raster::ncol(newdata) <- n/2		  
	      n = c(n/2,n/2) 
		  warning(paste0("changing raster dimensions to: ", n/2, " - ", n/2))
	    }	
      } else if( class(newdata) == "SpatialPixelsDataFrame" | class(newdata) == "SpatialGridDataFrame" ) {
        gp = sp::gridparameters(newdata)	  
	    ext <- as.vector(raster::extent(newdata))
        if(missing(n)) { n <- gp$cells.dim } else { n <- c(n/2,n/2) }  
      }
	  newdata <- ext
    }	
  #### weighted kde function, modification of MASS::kde2d 
    fhat <- function (x, y, h, w, n = 25, lims = c(range(x), range(y))) {
      nx <- length(x)
        if (length(y) != nx) 
            stop("data vectors must be the same length")
        if (length(w) != nx & length(w) != 1) 
            stop("weight vectors must be 1 or length of data")
        if (missing(h)) { 
          h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
        } else { 
	      h <- rep(h, length.out = 2L)
	    }	
      if (any(h <= 0)) stop("bandwidths must be strictly positive")
        if (missing(w)) { w <- numeric(nx) + 1 }
	  gx <- seq(lims[1], lims[2], length = n[1])
        gy <- seq(lims[3], lims[4], length = n[2])
            h <- h/4
          ax <- outer(gx, x, "-") / h[1]
        ay <- outer(gy, y, "-") / h[2]
      z <- ( matrix(rep(w, n[1]), nrow = n[1], ncol = nx, byrow = TRUE) * 
             matrix(stats::dnorm(ax), n[1], nx) ) %*% t(matrix(stats::dnorm(ay), n[2], nx)) /
	        ( sum(w) * h[1] * h[2] )
      return(list(x = gx, y = gy, z = z))
    }
  if(!is.null(y)) {
    cat("\n","calculating weighted kde","\n")
    k  <- fhat(sp::coordinates(x)[,1], sp::coordinates(x)[,2], w = y, 
	           h = bw, n = n, lims = as.vector(raster::extent(newdata)) )
  } else {
	cat("\n","calculating unweighted kde","\n")
	k <- MASS::kde2d(sp::coordinates(x)[,1], sp::coordinates(x)[,2], h = bw, 
	                 n = n, lims = as.vector(raster::extent(newdata)) )
  }
	if(!is.null(scale.factor)) { k$z <- k$z * scale.factor }	
	if( standardize == TRUE ) { k$z <- (k$z - min(k$z)) / (max(k$z) - min(k$z)) }		
    kde.est <- raster::raster(sp::SpatialPixelsDataFrame(sp::SpatialPoints(expand.grid(k$x, k$y)), 
	                          data.frame(kde = as.vector(array(k$z,length(k$z))))))
      if(is.null(newdata) == FALSE & mask == TRUE) {
	    kde.est <- raster::mask(raster::resample(kde.est, newdata), newdata) 
	  }
    sp::proj4string(kde.est) <- sp::proj4string(x)  
  return( kde.est )  
}  
