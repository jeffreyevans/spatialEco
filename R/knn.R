#' @title Spatial K nearest neighbor
#' @description 
#' Find K nearest neighbors for two spatial objects
#'
#' @param y          Spatial points or polygons object or 
#'                   coordinates matrix
#' @param x          Spatial points or polygons object or 
#'                   coordinates matrix
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
#'  library(sp)
#'  data(meuse)
#'    coordinates(meuse) <- ~x+y
#'  
#'  idx <- sample(1:nrow(meuse), 10) 
#'    pts <- meuse[idx,]
#'    meuse <- meuse[-idx,]
#'      meuse$IDS <- 1:nrow(meuse)
#'  
#'  # Find 2 neighbors in meuse
#'  ( nn <- knn(pts, meuse, k=2, ids = "IDS", indexes = TRUE) )
#'     plot(pts, pch=19, main="KNN")
#'       points(meuse[nn[,1],], pch=19, col="red")
#' 
#' # Using covariates (weights)
#' wx = as.matrix(meuse@data[,1:3])
#' wy = as.matrix(pts@data[,1:3])
#' 
#' ( nn <- knn(pts, meuse, k=2, ids = "IDS", indexes = TRUE,
#'             weights.y=wy, weights.x=wx) )
#'     plot(pts, pch=19, main="KNN")
#'       points(meuse[nn[,1],], pch=19, col="red")
#' 	  
#'  # Using coordinate matrices
#'  y <- coordinates(pts)
#'  x <- coordinates(meuse)
#'  knn(y, x, k=2)
#'	  
#' @seealso \code{\link[RANN]{nn2}} for details on search algorithm 
#'
#' @export knn
knn <- function(y, x, k = 1, d = NULL, ids = NULL, 
                weights.y = NULL, weights.x = NULL,
                indexes = FALSE) {
    if(!any(which(utils::installed.packages()[,1] %in% "RANN")))
      stop("please install RANN package before running this function")				
  if(!is.null(ids)) { 
    if(!ids %in% names(x)) 
      stop("ids do not exist in data")
    nidx <- which( names(x) %in% ids )	  
  }
  classes <- c("SpatialPointsDataFrame", 
               "SpatialPolygonsDataFrame",
			   "SpatialPoints", "SpatialPolygons")
  if(any(class(x) %in% classes)) {
    xmat <- sp::coordinates(x)
  } else {
    if(!is.matrix(x)) 
	  stop("x needs to be an sp or matrix object")
	if(ncol(x) > 2)
      stop("coordinate matrix has too many columns")
    xmat <- x	  
  }
  if(any(class(y) %in% classes)) {
    ymat <- sp::coordinates(y)
  } else {
    if(!is.matrix(y)) 
	  stop("y needs to be an sp or matrix object")
	if(ncol(y) > 2)
      stop("coordinate matrix has too many columns")
    ymat <- y	  
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
                   k = k				   
            )				   
    if(!is.null(d)) {
      if(!is.numeric(d)) stop("Distance must be numeric")
      fun.args[["radius"]] <-  d
      fun.args[["searchtype"]] <-  "radius"     
    }    
  nn <- do.call(RANN::nn2, fun.args)
    xrows <- list() 
      for(i in 1:ncol(nn$nn.idx)) {
	    if(!is.null(ids)) {
	      xrows[[i]] <- as.data.frame(x@data[nn$nn.idx[,i],])[,nidx]
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
