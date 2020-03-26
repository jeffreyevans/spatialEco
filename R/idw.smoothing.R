#' @title Inverse Distance Weighted smoothing
#' @description 
#' Distance weighted smoothing of a variable in a 
#'          spatial point object
#'
#' @param x Object of class SpatialPointsDataFrame
#' @param y Numeric data in x@@data
#' @param d Distance constraint       
#' @param k Maximum number of k-nearest neighbors within d    
#' 
#' @return A vector, same length as nrow(x), of adjusted y values 
#'
#' @note
#' Smoothing is conducted with a weighted-mean where; weights represent inverse 
#' standardized distance lags Distance-based or neighbour-based smoothing can be 
#' specified by setting the desired neighbour smoothing method to a specified value 
#' then the other parameter to the potential maximum. For example; a constraint 
#' distance, including all neighbors within 1000 (d=1000) would require k to equal 
#' all of the potential neighbors (n-1 or k=nrow(x)-1).  
#'                                                                     
#' @examples 
#'  library(sp)
#'   data(meuse)                                                   
#'   coordinates(meuse) <- ~x+y             
#' 
#' # Calculate distance weighted mean on cadmium variable in meuse data   
#'   cadmium.idw <- idw.smoothing(meuse, 'cadmium', k=nrow(meuse), d = 1000)                
#'   meuse@@data$cadmium.wm <- cadmium.idw
#'   opar <- par 
#'   par(mfrow=c(2,1)) 
#'   plot(density(meuse@@data$cadmium), main='Cadmium')
#'   plot(density(meuse@@data$cadmium.wm), main='IDW Cadmium')
#'   par <- opar
#'
#' @export
idw.smoothing <- function(x, y, d, k) {
    # if(class(x) == "sf") { x <- as(x, "Spatial") }
    if (!inherits(x, "SpatialPointsDataFrame")) 
        stop(deparse(substitute(x)), " MUST BE A sp SpatialPointsDataFrame OBJECT")
    if (is.null(y)) 
        stop("y (value) column must be specified")
    if (!y %in% names(x@data)) 
        stop(deparse(substitute(y)), "does not exists")
    if (!class(x@data[, y]) == "numeric") 
        stop("y must be numeric")
    ddata <- data.frame(ID = row.names(x), X = sp::coordinates(x)[, 1], Y = sp::coordinates(x)[, 2])
    nearest <- RANN::nn2(ddata, query = ddata, k = k, treetype = c("bd"), searchtype = c("radius"), radius = d)
    knn.id <- as.data.frame(nearest$nn.idx)
    names(knn.id) <- paste("NN", seq(1, ncol(knn.id), 1), sep = "")
    knn.id <- data.frame(ID = row.names(x@data), knn.id)
    knn.dist <- as.data.frame(nearest$nn.dists)
    names(knn.dist) <- paste("NND", seq(1, ncol(knn.dist), 1), sep = "")
    knn.dist <- data.frame(ID = row.names(x@data), knn.dist)
    wts <- rev(seq(1, d, 1))/d
    v <- as.numeric()
    for (i in 1:dim(knn.id)[1]) {
        knn.sub <- as.numeric(knn.id[i, ][2:k])
        lsub <- x[knn.sub, ]
        if (dim(lsub)[1] < 2) {
            wt <- 1
        } else {
            dsub <- as.numeric(knn.dist[i, ][2:k])
            dsub[which(knn.sub == 0)] <- 0
            dsub <- dsub[dsub > 0]
            wt <- round(append(dsub, 1, after = 0), digits = 0)
            wt <- wts[wt]
        }
        wm <- stats::weighted.mean(lsub@data[, y], wt, na.rm = TRUE)
        wm[is.nan(wm)] <- NA
        v <- append(v, wm, after = length(v))
    }
    return(v)
} 
