#' @title Correlogram
#' @description Calculates and plots a correlogram
#' 
#' @param x         SpatialPointsDataFrame object
#' @param v         Test variable in x@@data
#' @param dist      Distance of correlation lags, if latlong=TRUE units are 
#'                  in kilometers
#' @param latlong   Coordinates are in latlong (TRUE/FALSE)
#' @param dmatrix   Should the distance matrix be include in output (TRUE/FALSE)
#' @param ns        Number of simulations to derive simulation envelope 
#' @param ...       Arguments passed to cor ('pearson', 'kendall' or 'spearman')
#' 
#' @return A list object containing: 
#' * autocorrelation is a data.frame object with the following components
#' * autocorrelation - Autocorrelation value for each distance lag 
#' * dist - Value of distance lag
#' * lci - Lower confidence interval (p=0.025)                
#' * uci - Upper confidence interval (p=0.975)
#' * CorrPlot recordedplot object to recall plot
#' * dmatrix Distance matrix (if dmatrix=TRUE)
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                   
#' @examples 
#' library(sp)
#'   data(meuse)
#' coordinates(meuse) = ~x+y
#' zinc.cg <- correlogram(x = meuse, v = meuse@@data[,'zinc'], dist = 250, ns = 9)
#' 
#' @export  
correlogram <- function(x, v, dist = 5000, dmatrix = FALSE, ns = 99, latlong = FALSE, ...) {
    # if(class(x) == "sf") { x <- as(x, "Spatial") }
    if ((inherits(x, "SpatialPointsDataFrame")) == FALSE) 
        stop("x MUST BE SP SpatialPointsDataFrame OBJECT")
    options(warn = -1)
    options(scipen = 999)
    w <- sp::spDists(x, x, longlat = latlong)
    aa <- ceiling(max(w)/dist)
    bw <- seq(0, aa * dist, dist)
    cors <- NULL
    for (i in 1:aa) {
        w1 <- ifelse(w > bw[i] & w <= bw[i + 1], 1, 0)
        w2 <- w1
        for (j in 1:dim(w1)[1]) {
            nu <- sum(w1[j, ])
            if (nu > 0) {
                w2[j, ] <- w1[j, ]/nu
            }
        }
        lag <- w2 %*% v
        cors <- c(cors, stats::cor(v, lag, ...))
    }
    if (length(which(is.na(cors))) > 0) {
      message(paste(length(which(is.na(cors))), "Spatial lag empty and dropped", sep = " "))
      bw <- bw[-which(is.na(cors))]
     cors <- cors[-which(is.na(cors))]
    }
    mc <- matrix(NA, nrow = ns, ncol = length(cors))
    for (s in 1:ns) {
        x@data$rand <- sample(v, dim(x)[1], replace = FALSE)
        rcors <- NULL
        for (i in 1:aa) {
            w1 <- ifelse(w > bw[i] & w <= bw[i + 1], 1, 0)
            w2 <- w1
            for (j in 1:dim(w1)[1]) {
                nu <- sum(w1[j, ])
                if (nu > 0) {
                  w2[j, ] <- w1[j, ]/nu
                }
            }
            lag <- w2 %*% x@data$rand
            rcors <- c(rcors, stats::cor(x@data$rand, lag))
        }
        mc[s, ] <- rcors
    }
    bw <- bw[-1]
    cg <- data.frame(cbind(cors, bw))
    cg <- cbind(cg, t(apply(mc, 2, stats::quantile, probs = c(0.025, 0.975))))
    names(cg) <- c("autocorrelation", "dist", "lci", "uci")
    graphics::plot(cg$dist, cg$autocorrelation, type = "n", ylim = c(-1, 1), main = "Correlogram", xlab = "distance", ylab = "autocorrelation")
      graphics::polygon(c(rev(cg$dist), cg$dist), c(cg$uci, rev(cg$lci)), col = "blue")
        graphics::lines(cg$dist, cg$autocorrelation, type = "b", pch = 20)
    if (dmatrix == TRUE) {
        return(list(autocorrelation = cg, CorrPlot = grDevices::recordPlot(), dmatrix = w))
    } else {
        return(list(autocorrelation = cg, CorrPlot = grDevices::recordPlot()))
    }
} 
