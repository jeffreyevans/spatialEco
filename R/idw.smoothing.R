#' @title Inverse Distance Weighted smoothing
#' @description 
#' Distance weighted smoothing of a variable in a 
#'          spatial point object
#'
#' @param x   An sf POINT class object
#' @param y   Numeric data column in x to be smoothed
#' @param d   Distance constraint       
#' @param k   Maximum number of k-nearest neighbors within d    
#' 
#' @details
#' Smoothing is conducted with a weighted-mean where; weights represent inverse 
#' standardized distance lags Distance-based or neighbour-based smoothing can be 
#' specified by setting the desired neighbour smoothing method to a specified value 
#' then the other parameter to the potential maximum. For example; a constraint 
#' distance, including all neighbors within 1000 (d=1000) would require k to equal 
#' all of the potential neighbors (n-1 or k=nrow(x)-1).  
#'    
#' @return A vector, same length as nrow(x), of smoothed y values 
#'                                                                 
#' @examples 
#' 
#' library(sf)
#' if(require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#'       
#'  # Calculate distance weighted mean on cadmium variable in meuse data   
#'    cadmium.idw <- idw.smoothing(meuse, 'cadmium', k=nrow(meuse), d = 1000)                
#'    meuse$cadmium.wm <- cadmium.idw
#'  
#'    opar <- par(no.readonly=TRUE)
#'      par(mfrow=c(2,1)) 
#'        plot(density(meuse$cadmium), main='Cadmium')
#'        plot(density(meuse$cadmium.wm), main='IDW Cadmium')
#'    par(opar)
#' 
#' plot(meuse[c("cadmium","cadmium.wm")], pch=20)   
#'
#' } else { 
#'   cat("Please install sp package to run example", "\n")
#' }
#'
#' @export
idw.smoothing <- function(x, y, d, k) {
    if(length(find.package("RANN", quiet = TRUE)) == 0)
      stop("please install RANN package before running this function")
    if (!inherits(x, "sf")) 
      stop(deparse(substitute(x)), " Must be an sf object")
    if (is.null(y)) 
      stop("y (value) column must be specified")
    if (!y %in% names(x)) 
      stop(deparse(substitute(y)), "does not exists")
    if (!inherits(sf::st_drop_geometry(x[, y])[,1], "numeric")) 
        stop("y must be numeric")
	ddata <- data.frame(ID = row.names(x), sf::st_coordinates(x)[,1:2])
      nearest <- RANN::nn2(ddata, query = ddata, k = k, treetype = c("bd"), 
	                       searchtype = c("radius"), radius = d)
        knn.id <- as.data.frame(nearest$nn.idx)
          names(knn.id) <- paste("NN", seq(1, ncol(knn.id), 1), sep = "")
        knn.id <- data.frame(ID = row.names(x), knn.id)
        knn.dist <- as.data.frame(nearest$nn.dists)
          names(knn.dist) <- paste("NND", seq(1, ncol(knn.dist), 1), sep = "")
      knn.dist <- data.frame(ID = row.names(x), knn.dist)
    wts <- rev(seq(1, d, 1))/d
	v <- as.numeric()
    for (i in 1:dim(knn.id)[1]) {
      knn.sub <- as.numeric(knn.id[i, ][2:k])
      lsub <- x[knn.sub, ]
      if (nrow(lsub) < 2) { wt <- 1 } else {
        dsub <- as.numeric(knn.dist[i, ][2:k])
          dsub[which(knn.sub == 0)] <- 0
            dsub <- dsub[dsub > 0]
          wt <- round(append(dsub, 1, after = 0), digits = 0)
        wt <- wts[wt]
      }
	  ysub <- sf::st_drop_geometry(lsub[,y])[,1]
        wm <- stats::weighted.mean(ysub, wt, na.rm = TRUE)
        wm[is.nan(wm)] <- NA
      v <- append(v, wm, after = length(v))
    }
    return(v)
} 
