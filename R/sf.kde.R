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
  if(is.null(bw)) {
    bw = ks::hpi(sf::st_coordinates(x)[,1:2])
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
  if(is.null(bw)) {
    if(is.null(y)) {
      bw = ks::hpi(sf::st_coordinates(x)[,1:2])
	    message("Unweighted automatic bandwidth: "); print(bw)
	} else {
	  bw = ks::Hscv.diag(cbind(sf::st_coordinates(x)[,1:2],y))
	    message("Weighted automatic CV bandwidth: "); print(bw)
	}
  } else {
    message("Using specified bandwidth: "); print(bw)
  }
  if(!is.null(y)) {
    message("\n","calculating weighted kde","\n")
    kde.est <- suppressWarnings(terra::flip(
	            terra::rast(ks::kde(sf::st_coordinates(x)[,1:2], h=bw, 
	                 gridsize=n, w = y, density=TRUE)$estimate,  
	                 extent=terra::ext(ref)), direction="v")) 
  } else {
	message("\n","calculating unweighted kde","\n")
    kde.est <- terra::flip(terra::rast(ks::kde(sf::st_coordinates(x)[,1:2], 
	                 h=bw, gridsize=n, density=TRUE)$estimate,  
	                 extent=terra::ext(ref)), direction="v") 
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
