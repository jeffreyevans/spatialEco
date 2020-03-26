#' @title optimalK
#' @description Find optimal k of k-Medoid partitions using 
#'              silhouette widths
#'
#' @param x Numeric dataframe, matrix or vector  
#' @param nk Number of clusters to test (2:nk)
#' @param plot Plot cluster silhouettes(TRUE/FALSE)
#' @param cluster Create cluster object with optimal k
#' @param clara Use clara model for large data
#' @param ... Additional arguments passed to clara
#'
#' @return Object of class clust "pam" or "clara" 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'
#' @references 
#' Theodoridis, S. & K. Koutroumbas(2006) Pattern Recognition 3rd ed.   
#'
#' @examples 
#' require(cluster)
#'   x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
#'              cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
#'
#'   clust <- optimal.k(x, 20, plot=TRUE, cluster=TRUE)
#'     plot(silhouette(clust), col = c('red', 'green'))
#'       plot(clust, which.plots=1, main='K-Medoid fit')
#' 
#' # Extract multivariate and univariate mediods (class centres)
#'   clust$medoids
#'     pam(x[,1], 1)$medoids  
#'
#' # join clusters to data
#'   x <- data.frame(x, k=clust$clustering) 
#'
#' @seealso \code{\link[cluster]{pam}} for details on Partitioning Around Medoids (PAM)  
#' @seealso \code{\link[cluster]{clara}} for details on Clustering Large Applications (clara) 
#'
#' @export  
optimal.k <- function(x, nk = 10, plot = TRUE, cluster = TRUE, clara = FALSE, ...) {
    asw <- numeric(nk)
    for (k in 2:nk) {
        if (clara == TRUE) {
            asw[k] <- cluster::clara(x, k, ...)$silinfo$avg.width
        } else {
            asw[k] <- cluster::pam(x, k, ...)$silinfo$avg.width
        }
        k.best <- which.max(asw)
    }
    message(paste("Optimal-K", k.best, sep = ": "))
    if (plot == TRUE) {
        graphics::plot(1:nk, asw, type = "s", main = "Clustering Optimization using K-Mediods",  
                       xlab = "K (number of clusters)", ylab = "mean silhouette width")
        graphics::axis(1, k.best, paste("best", k.best, sep = "\n"), col = "red", col.axis = "red")
    }
    if (cluster == TRUE) {
        if (clara == TRUE) {
            return(cluster::clara(x, k.best, ...))
        } else {
            return(cluster::pam(x, k.best, ...))
        }
    }
} 
