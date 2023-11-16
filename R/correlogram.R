#' @title Correlogram
#' @description Calculates and plots a correlogram
#' 
#' @param x         A sf POINT object
#' @param v         Test variable in x
#' @param dist      Distance of correlation lags, if latlong=TRUE units are 
#'                  great circle in kilometers
#' @param ns        Number of simulations to derive simulation envelope 
#' @param ...       Arguments passed to cor ('pearson', 'kendall' or 'spearman')
#' 
#' @return 
#' Plot of correlogram and a list object containing: 
#' * autocorrelation is a data.frame object with the following components
#' *   autocorrelation - Autocorrelation value for each distance lag 
#' *   dist - Value of distance lag
#' *   lci - Lower confidence interval (p=0.025)                
#' *   uci - Upper confidence interval (p=0.975)
#' * CorrPlot recordedplot object to recall plot
#' @md
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                   
#' @examples 
#' library(sf)
#' if(require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' }
#' 
#' zinc.cg <- correlogram(x = meuse, v = meuse$zinc, dist = 250, ns = 9)
#' 
#' @export correlogram  
correlogram <- function(x, v, dist = 5000, ns = 99, ...) {
  if (!inherits(x, c("SpatialPointsDataFrame", "sf")))		
    stop(deparse(substitute(x)), " x must be a sf or sp point object")
  if(inherits(x, c("SpatialPointsDataFrame", "SpatialPoints"))) {
     x <- sf::st_as_sf(x)
  }
	oops <- options() 
      on.exit(options(oops)) 
        options(scipen = 999)		
    w <- units::drop_units(sf::st_distance(x))
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
      warning(paste(length(which(is.na(cors))), "Spatial lag empty and dropped", sep = " "))
      bw <- bw[-which(is.na(cors))]
     cors <- cors[-which(is.na(cors))]
    }
    mc <- matrix(NA, nrow = ns, ncol = length(cors))
    for (s in 1:ns) {
        x$rand <- sample(v, nrow(x), replace = FALSE)
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
            lag <- w2 %*% x$rand
            rcors <- c(rcors, stats::cor(x$rand, lag))
        }
        mc[s, ] <- rcors
    }
    bw <- bw[-1]
    cg <- data.frame(cbind(cors, bw))
    cg <- cbind(cg, t(apply(mc, 2, stats::quantile, probs = c(0.025, 0.975))))
    names(cg) <- c("autocorrelation", "dist", "lci", "uci")
    graphics::plot(cg$dist, cg$autocorrelation, type = "n", ylim = c(-1, 1), 
	               main = "Correlogram", xlab = "distance", 
				   ylab = "autocorrelation")
      graphics::polygon(c(rev(cg$dist), cg$dist), c(cg$uci, rev(cg$lci)), col = "blue")
        graphics::lines(cg$dist, cg$autocorrelation, type = "b", pch = 20)
  return(list(autocorrelation = cg, CorrPlot = grDevices::recordPlot()))
} 
