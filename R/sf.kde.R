#' @title Spatial kernel density estimate
#' @description A weighted or unweighted Gaussian Kernel Density estimate 
#'              for point spatial data
#'
#' @param x             sp SpatialPointsDataFrame object
#' @param y             Optional values, associated with x coordinates, 
#'                      to be used as weights
#' @param bw            Distance bandwidth of Gaussian Kernel, must be units 
#'                      of projection
#' @param ref           A terra SpatRaster, sf class object or c[xmin,xmax,ymin,ymax] 
#'                      vector to estimate the kde extent
#' @param res           Resolution of raster when ref not SpatRaster           
#' @param standardize   Standardize results to 0-1 (FALSE/TRUE)
#' @param scale.factor  Optional numeric scaling factor for the KDE (eg., 10000),  
#'                      to account for very small estimate values
#' @param mask          (TRUE/FALSE) mask resulting raster if ref is provided  
#'                      as a SpatRaster
#'
#' @return  a terra SpatRaster class object containing kernel density estimate 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \donttest{ 
#' library(sf) 
#' library(terra) 
#'   
#' data(meuse, package = "sp")
#' meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                   agr = "constant") 
#'   			
#' # Unweighted KDE (spatial locations only)				
#' pt.kde <- sf.kde(x = meuse, bw = 1000, standardize = TRUE, 
#'                  scale.factor = 10000, res=40)
#'   plot(pt.kde, main="Unweighted kde")
#'     plot(st_geometry(meuse), pch=20, col="red", add=TRUE) 
#' 
#' # Weighted KDE using cadmium and extent
#' ( e <- st_bbox(meuse)[c(1,3,2,4)] ) 
#' cadmium.kde <- sf.kde(x = meuse, y = meuse$cadmium, bw = 1000,  
#'                       ref = e, standardize = TRUE, 
#' 					    scale.factor = 10000, res=40)
#' plot(cadmium.kde)
#'   plot(st_geometry(meuse), pch=20, col="red", add=TRUE)
#'  			
#' }
#'
#' @export sp.kde sf.kde
#' @aliases sp.kde
sf.kde <- function(x, y = NULL, bw = NULL, ref = NULL, res = NULL,   
                   standardize = FALSE, scale.factor = NULL, 
				   mask = FALSE) {
  if(missing(x))
    stop("x argument must be provided")
  ref.flag = inherits(ref, "SpatRaster")
  if(!inherits(x, c("sf", "sfc") ))	
    stop(deparse(substitute(x)), " must be a sf, or sfc object")
  if(unique(as.character(sf::st_geometry_type(x))) != "POINT")
      stop(deparse(substitute(x)), " must be single-part POINT geometry") 
  if(is.null(bw)){ 
    bw <- c(MASS::bandwidth.nrd(sf::st_coordinates(x)[,1]), 
	        MASS::bandwidth.nrd(sf::st_coordinates(x)[,2]))
	  message("Using", bw, "for bandwidth", "\n")
  } else {
    bw <- c(bw,bw)
  }
  if(is.null(ref)) {
    if(!is.null(res)) {
      ref <- terra::rast(terra::ext(x), resolution =  res)
    } else {
      ref <- terra::rast(terra::ext(x))
       message("defaulting to ", terra::res(ref)[1], "x", terra::res(ref)[2], " cell resolution")
    }	
  }
  if(inherits(ref, "numeric")) {
    if(length(ref) != 4) 
      stop("Need xmin, xmax, ymin, ymax bounding coordinates")
      if(!is.null(res)) {
        ref <- terra::rast(terra::ext(ref), resolution =  res)
      } else {
        ref <- terra::rast(terra::ext(ref))
        message("defaulting to ", terra::res(ref)[1], "x", terra::res(ref)[2], " cell resolution")
      }	
  } else {
    if(!inherits(ref, "SpatRaster"))
      stop(deparse(substitute(ref)), " must be a terra SpatRast object")
  }
  n <- c(terra::nrow(ref), terra::ncol(ref))
  if(is.null(scale.factor)) scale.factor = 1 
  
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
    message("\n","calculating weighted kde","\n")
    k  <- fhat(sf::st_coordinates(x)[,1], sf::st_coordinates(x)[,2], w = y, 
	           h = bw, n = n, lims = as.vector(terra::ext(ref)) )
  } else {
	message("\n","calculating unweighted kde","\n")
	k <- MASS::kde2d(sf::st_coordinates(x)[,1], sf::st_coordinates(x)[,2], h = bw, 
	                 n = n, lims = as.vector(terra::ext(ref)) )
  }
  k$z <- k$z * scale.factor	
	if( standardize == TRUE ) { k$z <- (k$z - min(k$z)) / (max(k$z) - min(k$z)) }
	kde.est <- terra::flip(terra::rast(t(k$z), extent=terra::ext(ref)), direction="v")
	#xyz <- data.frame(expand.grid(k$x, k$y), kde = as.vector(array(k$z,length(k$z))))
	#  names(xyz)[1:2] <- c("x","y")
    #xyz <- st_as_sf(xyz, coords = c("x", "y"), crs = sf::st_crs(x), 
    #                agr = "constant") 
    #kde.est2 <- terra::rasterize(terra::vect(xyz), ref, field="kde", fun="mean")  
      if(mask) {
	    if(!ref.flag) {
		  message("Since a raster was not used as ref, there is nothing to mask")
		} else {  
	      kde.est <- terra::mask(kde.est, ref) 
		}
	  }  
    terra::crs(kde.est) <- terra::crs(x)
  return( kde.est )  
}
sp.kde = sf.kde  
