#' @title Spatial K nearest neighbor
#' @description 
#' Find K nearest neighbors for two spatial objects
#'
#' @param y          Spatial sf object or coordinates matrix 
#' @param x          Spatial points or polygons object or coordinates matrix
#' @param k          Number of neighbors
#' @param d          Optional search radius
#' @param ids        Optional column of ID's in x
#' @param weights.y  A vector or matrix representing covariates of y
#' @param weights.x  A vector or matrix representing covariates of x
#' @param indexes    (FALSE/TRUE) Return row indexes of x neighbors
#'
#' @return 
#' A data.frame with row indexes (optional), rownames, ids (optional) and 
#' distance of k
#'
#' @description
#' Finds nearest neighbor in x based on y and returns rownames, index and distance,
#' If ids is NULL, rownames of x are returned. If coordinate matrix provided, 
#' columns need to be ordered [X,Y]. If a radius for d is specified than a maximum 
#' search radius is imposed. If no neighbor is found, a neighbor is not returned  
#'
#' You can specify weights to act as covariates for x and y. The vectors or matrices
#' must match row dimensions with x and y as well as columns matching between weights.
#' In other words, the covariates must match and be numeric.   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples 
#' \donttest{
#' library(sf)
#' 
#' if(require(sp, quietly = TRUE)) {
#'    data(meuse, package = "sp")
#'    meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                      agr = "constant")
#'  
#' # create reference and target obs
#' idx <- sample(1:nrow(meuse), 10) 
#'   pts <- meuse[idx,]
#'     meuse <- meuse[-idx,]
#'     meuse$IDS <- 1:nrow(meuse)
#'  
#' # Find 2 neighbors in meuse
#' ( nn <- knn(pts, meuse, k=2, ids = "IDS", indexes = TRUE) )
#'   plot( st_geometry(pts), pch=19, main="KNN")
#'     plot(st_geometry(meuse[nn[,1],]), pch=19, col="red", add=TRUE)
#' 
#' # Using covariates (weights)
#' wx = as.matrix(st_drop_geometry(meuse[,1:3]))
#' wy = as.matrix(st_drop_geometry(pts[,1:3]))
#' 
#' ( nn <- knn(pts, meuse, k=2, ids = "IDS", indexes = TRUE,
#'             weights.y=wy, weights.x=wx) )
#'   plot(st_geometry(pts), pch=19, main="KNN")
#'     plot(st_geometry(meuse[nn[,1],]), pch=19, col="red")
#' 	  
#' # Using coordinate matrices
#' y <- st_coordinates(pts)[,1:2]
#' x <- st_coordinates(meuse)[,1:2]
#' knn(y, x, k=2)
#' }  
#' }	  
#' @seealso \code{\link[RANN]{nn2}} for details on search algorithm 
#' @export knn
knn <- function(y, x, k = 1, d = NULL, ids = NULL, 
                weights.y = NULL, weights.x = NULL,
                indexes = FALSE) {
  if(!any(which(utils::installed.packages()[,1] %in% "RANN")))
      stop("please install RANN package before running this function")

  gtypes = c("POLYGON", "POINT")
  mtypes = c("MULTIPOLYGON", "MULTIPOINT") 

  if(!is.null(ids)) { 
    if(!ids %in% names(x)) 
      stop("ids do not exist in data")
    nidx <- which( names(x) %in% ids )
  }

  if(inherits(x, "matrix")) {
    if(ncol(x) > 2)
      stop("coordinate matrix has too many columns")
    xmat <- x
    x <- as.data.frame(x)		
  } else {
    xgeom = unique(as.character(sf::st_geometry_type(x)))  
    if(!inherits(x, c("sf", "sfc")))
      stop(deparse(substitute(x)), " must be an sf class object")	  
    if(any(xgeom == mtypes))
      stop(deparse(substitute(x)), " is multipart geometry and not supported ")
    if(!any(xgeom != gtypes))
      stop(deparse(substitute(x)), " must be one of ", 
  	  paste(gtypes, collopse=""))
    if(xgeom == gtypes[1]) {
	  cat("Warning, x has polygon geometry using centroid coordinates", "\n")
      xmat <- sf::st_coordinates(sf::st_centroid(x))[,1:2]
    } else {
      xmat <- sf::st_coordinates(x)[,1:2]
    }
  }
  
  if(inherits(y, "matrix")) {
    if(ncol(y) > 2)
      stop("coordinate matrix has too many columns")
    ymat <- y
    y <- as.data.frame(y)	
  } else {
    ygeom = unique(as.character(sf::st_geometry_type(y)))	
    if(!inherits(y, c("sf", "sfc")))
      stop(deparse(substitute(y)), " must be an sf class object")	  
    if(any(ygeom == mtypes))
      stop(deparse(substitute(y)), " is multipart geometry and not supported ")
    if(!any(ygeom != gtypes))
      stop(deparse(substitute(y)), " must be one of ", 
  	  paste(gtypes, collopse=""))
    if(ygeom == gtypes[1]) {
	  cat("Warning, y has polygon geometry using centroid coordinates", "\n")
      ymat <- sf::st_coordinates(sf::st_centroid(y))[,1:2]
    } else {
      ymat <- sf::st_coordinates(y)[,1:2]
    }	  
  }

  if(any(!is.null(weights.x), !is.null(weights.y))) {	
    if(any(is.null(weights.x), is.null(weights.y)))
      stop("Both x and y weights must be defined and represent the same covariates")
    if(is.vector(weights.x)) {	
	  weights.x <- matrix(weights.x, byrow=TRUE)
    }	
    if(is.vector(weights.x)) {	
	  weights.x <- matrix(weights.x, byrow=TRUE)
    }
    if(any(apply(xmat,2,is.numeric) == FALSE))
	  stop("weights must be numeric")
    if(any(apply(ymat,2,is.numeric) == FALSE))
	  stop("weights must be numeric")	
    if(nrow(weights.x) != nrow(xmat) )
	  stop("Row dimensions of x weights do not match")
	if(nrow(weights.y) != nrow(ymat) )
	  stop("Row dimensions of x weights do not match")
  	if(ncol(weights.y) != ncol(weights.y) )
	  stop("Weights of x and y must represent the same covariates")	
    xmat <- cbind(xmat, as.matrix(weights.x))
    ymat <- cbind(ymat, as.matrix(weights.y))	
  }  
  
  fun.args <- list(data = xmat, 
                   query = ymat,
                   treetype = "kd",
                   k = k)
    if(!is.null(d)) {
      if(!is.numeric(d)) 
	    stop("Distance must be numeric")
      fun.args[["radius"]] <-  d
      fun.args[["searchtype"]] <- "radius"     
    } 
  nn <- do.call(RANN::nn2, fun.args) 
  
  xrows <- list() 
    for(i in 1:ncol(nn$nn.idx)) {
      if(!is.null(ids)) {
        xrows[[i]] <- as.data.frame(x[nn$nn.idx[,i],])[,nidx]
      } else {
        xrows[[i]] <- row.names(x)[nn$nn.idx[,i]]
      }	
    }	
   xrows <- as.data.frame(do.call("cbind", xrows))
     names(xrows) <- paste0("ids", 1:ncol(xrows))
   xdist <- as.data.frame(nn$nn.dists)
     names(xdist) <- paste0("dist", 1:ncol(xdist))
  if( indexes ) {
    xidx <- as.data.frame(nn$nn.idx)
      names(xidx) <- paste0("idx", 1:ncol(xdist))
    return( data.frame(xidx, xrows, xdist) )	  
  } else { 
    return( data.frame(xrows, xdist) )
  }  
}
