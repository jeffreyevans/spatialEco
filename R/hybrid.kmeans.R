#' @title Hybrid K-means
#' @description 
#' Hybrid K-means clustering using hierarchical clustering to define 
#' cluster-centers 
#'
#' @param x        A data.frame or matrix with data to be clustered
#' @param k        Number of clusters
#' @param hmethod  The agglomeration method used in hclust
#' @param stat     The statistic to aggregate class centers (mean or median) 
#' @param ...      Additional arguments passed to \code{\link[stats]{kmeans}}
#'
#' @return 
#' returns an object of class "kmeans" which has a print and a fitted method
#'
#' @details 
#' This method uses hierarchical clustering to define the cluster-centers in the K-means 
#' clustering algorithm. This mitigates some of the know convergence issues in K-means.   
#' 
#'@note 
#' options for hmethod are: "ward.D", "ward.D2", "single", 
#' "complete", "average", mcquitty", "median", "centroid"
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Singh, H., & K. Kaur (2013) New Method for Finding Initial Cluster Centroids in 
#'   K-means Algorithm. International Journal of Computer Application. 74(6):27-30
#' @references
#' Ward, J.H., (1963) Hierarchical grouping to optimize an objective function. Journal 
#'   of the American Statistical Association. 58:236-24
#'
#' @examples
#' x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
#'            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
#' 
#' # Compare k-means to hybrid k-means with k=4		   
#' km <- kmeans(x, 4)		   
#' hkm <- hybrid.kmeans(x,k=4)		   
#' 
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2))
#'   plot(x[,1],x[,2], col=km$cluster,pch=19, main="K-means")
#'   plot(x[,1],x[,2], col=hkm$cluster,pch=19, main="Hybrid K-means")
#' par(opar)
#'
#' @seealso \code{\link[stats]{kmeans}} for available ... arguments and function details  
#' @seealso \code{\link[stats]{hclust}} for details on hierarchical clustering 
#'  
#' @export hybrid.kmeans
hybrid.kmeans <- function(x, k = 2, hmethod = "ward.D", stat = mean, ...) {
 h.clust <- stats::hclust(stats::dist(x), method=hmethod)
   cut.clust <- stats::cutree(h.clust, k = k) 
   cluster.centers <- stats::aggregate(x, list(cut.clust), stat)[, -1]
 return( stats::kmeans(x, centers = cluster.centers, ...) )
}
 