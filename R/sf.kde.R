#' @title Spatial kernel density estimate
#' @description A weighted or unweighted Gaussian Kernel Density estimate 
#'              for point spatial data
#'
#' @param x             sf POINT object
#' @param y             Optional values, associated with x coordinates, 
#'                      to be used as weights
#' @param bw            Distance bandwidth of Gaussian Kernel, must be units 
#'                      of projection
#' @param ref           A terra SpatRaster, sf class object or c[xmin,xmax,ymin,ymax] 
#'                      vector to estimate the kde extent
#' @param res           Resolution of raster when ref not SpatRaster           
#' @param standardize   Standardize results to 0-1 (FALSE/TRUE)
#' @param scale.factor  Numeric scaling factor for the KDE (defaults to 10000),  
#'                      to account for very small estimate values
#' @param mask          (TRUE/FALSE) mask resulting raster if ref is provided  
#'                      as a SpatRaster
#'
#' @details
#' Please note that ks methods for estimation has been reverted to the Gussian method proposed
#' in Venables & Ripley (2002). There was not enought evendence that the Chacon & Duong (2018)
#' multivariate method(s) for bandwidth selection and kernal estimation were suitable for 
#' spatial random fields. 
#'
#' @return  a terra SpatRaster class object containing kernel density estimate 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Duong, T. & Hazelton, M.L. (2005) Cross-validation bandwidth matrices for multivariate 
#'   kernel density estimation. Scandinavian Journal of Statistics, 32, 485-506. 
#'
#' Wand, M.P. & Jones, M.C. (1994) Multivariate plug-in bandwidth selection. Computational 
#'   Statistics, 9, 97-116. 
#'
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. 
#'   Fourth edition. Springer. 
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
#' # Unweighted KDE (spatial locations only) with 40m resoultion  				
#' pt.kde <- sf.kde(x = meuse, bw = 1000, standardize = TRUE, res=40)
#'   plot(pt.kde, main="Unweighted kde")
#'     plot(st_geometry(meuse), pch=20, col="red", add=TRUE) 
#' 
#' # cadmium weighted KDE usign extent with 40m resoultion and 500m and 1000m bw 
#' cadmium.kde.500 <- sf.kde(x = meuse, y = meuse$cadmium, res=40, 
#'                           bw = 500, standardize = TRUE)
#' cadmium.kde.1000 <- sf.kde(x = meuse, y = meuse$cadmium, res=40, 
#'                           bw = 1000, standardize = TRUE)						  
#'   plot(c(cadmium.kde.500, cadmium.kde.1000))
#'   			  			
#' }
#' 
#' @export sp.kde sf.kde
#' @aliases sp.kde
sf.kde <- function(x, y = NULL, bw = NULL, ref = NULL, res = NULL,  
                   standardize = FALSE, scale.factor = 10000, 
				   mask = FALSE) {
  if(missing(x))
    stop("x argument must be provided")
  if(!inherits(x, c("sf", "sfc") ))	
    stop(deparse(substitute(x)), " must be a sf, or sfc object")
  if(unique(as.character(sf::st_geometry_type(x))) != "POINT")
    stop(deparse(substitute(x)), " must be single-part POINT geometry") 
  if(is.null(scale.factor)) scale.factor = 1     
  ref.flag = inherits(ref, "SpatRaster")
  if(is.null(res) &&  ref.flag == TRUE) {
    res <- terra::res(ref)
  }
  if(is.null(res) &&  ref.flag == FALSE) {
    res <- 100
	message("resoultion not defined, defaulting to 100")
  }
  if(is.null(ref)) {
    ref <- terra::rast(terra::ext(x), resolution =  res, 
		               crs=terra::crs(x))
  } else if(inherits(ref, "numeric")) {
    if(length(ref) != 4) 
      stop("Need xmin, xmax, ymin, ymax bounding coordinates")
    ref <- terra::rast(terra::ext(ref), resolution =  res, 
		               crs=terra::crs(x))
  } else {
    if(!inherits(ref, "SpatRaster"))
      stop(deparse(substitute(ref)), " must be a terra SpatRast object")
	if(terra::res(ref)[1] != res)
	  message("reference raster defined, res argument is being ignored")
  }

  # weighted kde function, modification of MASS::kde2d 
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
  
  if(is.null(bw)){ 
    bwf <- function(x){
      r <- stats::quantile(x, c(0.25, 0.75))
      h <- (r[2] - r[1])/1.34
      4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1/5)
    }
	bw <- c(bwf(sf::st_coordinates(x)[,1]), 
	        bwf(sf::st_coordinates(x)[,2]))
	  message("Using ", round(bw[1],3), ", ", round(bw[2],3), " for bandwidth", "\n")
  } else {
    bw <- c(bw,bw)
  }
  
  n <- c(terra::nrow(ref), terra::ncol(ref)) 
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
    pts <- data.frame(expand.grid(x=k$x, y=k$y), 
                         z=round(as.vector(array(k$z,length(k$z))) *
                         scale.factor, 10))
    kde.est <- terra::rast(pts, type="xyz", extent = terra::ext(ref) )
   if(mask == TRUE) {
     kde.est <- terra::mask(kde.est, ref) 
	}
    terra::crs(kde.est) <- terra::crs(x)  
  return( kde.est )  
}  
sp.kde = sf.kde

# sf.kde <- function(x, y = NULL, bw = NULL, ref = NULL, res = NULL,   
#                    standardize = FALSE, scale.factor = NULL, 
# 				   mask = FALSE) {
#   if(missing(x))
#     stop("x argument must be provided")
#   ref.flag = inherits(ref, "SpatRaster")
#   if(!inherits(x, c("sf", "sfc") ))	
#     stop(deparse(substitute(x)), " must be a sf, or sfc object")
#   if(unique(as.character(sf::st_geometry_type(x))) != "POINT")
#       stop(deparse(substitute(x)), " must be single-part POINT geometry") 
#   if(is.null(ref)) {
#     if(!is.null(res)) {
#       ref <- terra::rast(terra::ext(x), resolution =  res)
#     } else {
#       ref <- terra::rast(terra::ext(x))
#        message("defaulting to ", terra::res(ref)[1], "x", terra::res(ref)[2], " cell resolution")
#     }	
#   }
#   if(inherits(ref, "numeric")) {
#     if(length(ref) != 4) 
#       stop("Need xmin, xmax, ymin, ymax bounding coordinates")
#       if(!is.null(res)) {
#         ref <- terra::rast(terra::ext(ref), resolution =  res)
#       } else {
#         ref <- terra::rast(terra::ext(ref))
#         message("defaulting to ", terra::res(ref)[1], "x", terra::res(ref)[2], " cell resolution")
#       }	
#   } else {
#     if(!inherits(ref, "SpatRaster"))
#       stop(deparse(substitute(ref)), " must be a terra SpatRast object")
#   }
#   n <- c(terra::nrow(ref), terra::ncol(ref)) 
#   if(!is.null(bw)) {
#     message("Using specified bandwidth: "); print(bw)
#   } else {	
#     if(is.null(y)) {
# 	  bw = suppressWarnings(ks::hpi(sf::st_coordinates(x)[,1:2]))
# 	    message("Unweighted automatic bandwidth: "); print(bw)
# 	} else {
# 	  bw = suppressWarnings(ks::Hscv.diag(cbind(sf::st_coordinates(x)[,1:2],y)))
# 	    message("Weighted automatic CV bandwidth: "); print(bw)
# 	} 
#   } 
#   if(!is.null(y)) {
#     message("\n","calculating weighted kde","\n")	
# 	kde.est <- suppressWarnings( 
# 	  terra::rast(matrix(ks::kde(sf::st_coordinates(x)[,1:2], 
#         h=bw, eval.points=terra::xyFromCell(ref, 1:terra::ncell(ref)), 
#         gridsize=n, w = y, density=TRUE)$estimate,
# 		nrow=n[1], ncol=n[2], byrow=TRUE),
#         extent=terra::ext(ref)) )				
#   } else {
# 	message("\n","calculating unweighted kde","\n")
# 	kde.est <- suppressWarnings( 
# 	  terra::rast(matrix(ks::kde(sf::st_coordinates(x)[,1:2], 
#         h=bw, eval.points=terra::xyFromCell(ref, 1:terra::ncell(ref)), 
#         gridsize=n, density=TRUE)$estimate,
# 		nrow=n[1], ncol=n[2], byrow=TRUE),
#         extent=terra::ext(ref)) )		
#   }
#   if(!is.null(scale.factor)) kde.est <- kde.est * scale.factor	
# 	if( standardize == TRUE ) { kde.est <- kde.est / 
# 	    terra::global(kde.est, "max", na.rm=TRUE)[,1] }	
#       if(mask) {
# 	    if(!ref.flag) {
# 		  message("Since a raster was not used as ref, there is nothing to mask")
# 		} else {  
# 	      kde.est <- terra::mask(kde.est, ref) 
# 		}
# 	  }  
#     terra::crs(kde.est) <- terra::crs(x)
#   return( kde.est )  
# }