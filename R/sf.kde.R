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
#' @param scale.factor  Optional numeric scaling factor for the KDE (eg., 10000),  
#'                      to account for very small estimate values
#' @param mask          (TRUE/FALSE) mask resulting raster if ref is provided  
#'                      as a SpatRaster
#'
#' @return  a terra SpatRaster class object containing kernel density estimate 
#'
#' @details
#' The automatic bandwidth selection for unweighted KDE uses the Wand & Jones (1994)
#' univariate plug-in whereas, weighted KDE's use the Duong & Hazelton (2005) smoothed
#' cross-validation for creating a diagonal bandwidth matrix.    
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
#' # Weighted KDE using cadmium and extent with automatic bandwidth selection
#' ( e <- st_bbox(meuse)[c(1,3,2,4)] ) 
#' cadmium.kde <- sf.kde(x = meuse, y = meuse$cadmium, ref = e,  
#'                       standardize = TRUE, 
#' 					  scale.factor = 10000, res=40)
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
  if(is.null(bw)) {
    if(is.null(y)) {
	  bw = suppressWarnings(ks::hpi(sf::st_coordinates(x)[,1:2]))
	    message("Unweighted automatic bandwidth: "); print(bw)
	} else {
	  bw = suppressWarnings(ks::Hscv.diag(cbind(sf::st_coordinates(x)[,1:2],y)))
	    message("Weighted automatic CV bandwidth: "); print(bw)
	}
  } else {
    message("Using specified bandwidth: "); print(bw)
  }
  if(!is.null(y)) {
    message("\n","calculating weighted kde","\n")	
	kde.est <- suppressWarnings( 
	  terra::rast(matrix(ks::kde(sf::st_coordinates(x)[,1:2], 
        h=bw, eval.points=terra::xyFromCell(ref, 1:terra::ncell(ref)), 
        gridsize=n, w = y, density=TRUE)$estimate,
		nrow=n[1], ncol=n[2], byrow=TRUE),
        extent=terra::ext(ref)) )				
  } else {
	message("\n","calculating unweighted kde","\n")
	kde.est <- suppressWarnings( 
	  terra::rast(matrix(ks::kde(sf::st_coordinates(x)[,1:2], 
        h=bw, eval.points=terra::xyFromCell(ref, 1:terra::ncell(ref)), 
        gridsize=n, density=TRUE)$estimate,
		nrow=n[1], ncol=n[2], byrow=TRUE),
        extent=terra::ext(ref)) )		
  }
  if(!is.null(scale.factor)) kde.est <- kde.est * scale.factor	
	if( standardize == TRUE ) { kde.est <- kde.est / 
	    terra::global(kde.est, "max", na.rm=TRUE)[,1] }	
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



# sp.kde <- function(x, y = NULL, bw = NULL, newdata = NULL, nr = NULL, nc = NULL,  
#                    standardize = FALSE, scale.factor = NULL, mask = TRUE) {
#   # if(class(x) == "sf") { x <- as(x, "Spatial") }
#   
#   if(is.null(bw)){ 
#     bwf <- function(x){
#       r <- quantile(x, c(0.25, 0.75))
#       h <- (r[2] - r[1])/1.34
#       4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
#     }
# 	bw <- c(bwf(sf::st_coordinates(x)[,1]), 
# 	        bwf(sf::st_coordinates(x)[,2]))
# 	  message("Using", bw, "for bandwidth", "\n")
#   } else {
#     bw <- c(bw,bw)
#   }
#   
#   if(is.null(scale.factor)) scale.factor = 1  
#     if(!is.null(nr) & !is.null(nr)) { n = c(nr, nc) } else { n = NULL }   
#   if(is.null(newdata)) { 
#     newdata <- as.vector(raster::extent(x))
#       message("Using extent of x to define grid")	
#   }
#   if(!is.null(newdata)) {
#     if( class(newdata) == "numeric") {
#       if(length(newdata) != 4) stop("Need xmin, xmax, ymin, ymax bounding coordinates")
# 	    if(is.null(n)) {
# 	      ext <- raster::raster(raster::extent(newdata))
#           n <- c(raster::nrow(ext), raster::ncol(ext))		
# 		    warning(paste0("defaulting to ", "nrow=", raster::nrow(ext), 
# 		  	      " & ", " ncol=", raster::ncol(ext)))
#         }
#       newdata <- terra::rast(terra::ext(newdata), nrow=n[1], ncol=n[2])		
# 	    newdata[] <- rep(1, terra::ncell(newdata)) 	
#     } else if(class(newdata) == "SpatRaster") { 	  
# 	  n = c(terra::nrow(newdata), terra::ncol(newdata))
#         message("using existing raster dimensions to define grid")		  
#     } 
#   }	
#   #### weighted kde function, modification of MASS::kde2d 
#     fhat <- function (x, y, h, w, n = 25, lims = c(range(x), range(y))) {
#       nx <- length(x)
#         if (length(y) != nx) 
#             stop("data vectors must be the same length")
#         if (length(w) != nx & length(w) != 1) 
#             stop("weight vectors must be 1 or length of data")
#         if (missing(h)) { 
#           h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
#         } else { 
# 	      h <- rep(h, length.out = 2L)
# 	    }	
#       if(any(h <= 0)) stop("bandwidths must be strictly positive")
#         if (missing(w)) { w <- numeric(nx) + 1 }
# 	  gx <- seq(lims[1], lims[2], length = n[1])
#       gy <- seq(lims[3], lims[4], length = n[2])
#             h <- h/4
#           ax <- outer(gx, x, "-") / h[1]
#           ay <- outer(gy, y, "-") / h[2]
#       z <- ( matrix(rep(w, n[1]), nrow = n[1], ncol = nx, byrow = TRUE) * 
#              matrix(stats::dnorm(ax), n[1], nx) ) %*% t(matrix(stats::dnorm(ay), n[2], nx)) /
# 	        ( sum(w) * h[1] * h[2] )
#       return(list(x = gx, y = gy, z = z))
#     }
#   if(!is.null(y)) {
#     message("\n","calculating weighted kde","\n")
#     k  <- fhat(sf::st_coordinates(x)[,1], sf::st_coordinates(x)[,2], w = y, 
# 	           h = bw, n = n, lims = as.vector(terra::ext(ref)) )
#   } else {
# 	message("\n","calculating unweighted kde","\n")
# 	k <- MASS::kde2d(sp::coordinates(x)[,1], sp::coordinates(x)[,2], h = bw, 
# 	                 n = n, lims = as.vector(raster::extent(newdata)) )
#   }
#   k$z <- k$z * scale.factor	
# 	if( standardize == TRUE ) { k$z <- (k$z - min(k$z)) / (max(k$z) - min(k$z)) }
# 
# pts <- data.frame(expand.grid(x=k$x, y=k$y), 
#           z=round(as.vector(array(k$z,length(k$z)))*scale.factor, 10))
#   sf.pts <- sf::st_as_sf(sf.pts, coords = c("x", "y"), crs = sf::st_crs(x), 
#                          agr = "constant") 
# 
# kde <- rast(pts, type="xyz", extent=e) 
# 	
# 	kde <- terra::rasterize(terra::vect(sf.pts), ref, field="z") 
# 	
# 	
#     kde.est <- terra::rast(sp::SpatialPixelsDataFrame(sp::SpatialPoints(expand.grid(k$x, k$y)), 
# 	                       data.frame(kde = as.vector(array(k$z,length(k$z))))))
#       if(is.null(newdata) == FALSE & mask == TRUE) {
# 	    kde.est <- raster::mask(raster::resample(kde.est, newdata), newdata) 
# 	  }
#     sp::proj4string(kde.est) <- sp::proj4string(x)  
#   return( kde.est )  
# }  
