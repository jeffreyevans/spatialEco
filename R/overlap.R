#' @title Niche overlap (Warren's-I)
#' @description Similarity Statistic for Quantifying Niche Overlap using Warren's-I
#'
#' @param x    A matrix, rasterLayer or sp raster class object
#' @param y    A matrix, rasterLayer or sp raster class object
#'             with the same dimensions of x
#'
#' @return A value representing the I similarity statistic
#'
#' @description
#' The overlap function computes the I similarity statistic (Warren et al. 2008)  
#' of two overlaping niche estimates. Similarity is based on the Hellenger distance. 
#' It is assumed that the input data share the same extent and cellsize and all values 
#' are positive.
#' 
#' The I similarity statistic sums the pair-wise differences between two
#' predictions to create a single value representing the similarity of the two
#' distributions. The I similarity statistic ranges from a value of 0, where
#' two distributions have no overlap, to 1 where two distributions are
#' identical (Warren et al., 2008). The function is based on code from 
#' Jeremy VanDerWal
#'
#' @author Jeffrey Evans <jeffrey_evans@@tnc.org> 
#'
#' @references 
#' Warren, D. L., R. E. Glor, M. Turelli, and D. Funk. (2008).
#'   Environmental Niche Equivalency versus Conservatism: Quantitative 
#'   Approaches to Niche Evolution. Evolution 62:2868-2883.
#'
#' @examples   
#' # add degree of separation in two matrices 
#' p1 <- abs(matrix(1:50,nr=50,nc=50) + 
#'          runif(n = 2500, min = -1, max = 1))
#' p2 <- abs(matrix(1:50,nr=50,nc=50) + 
#'          rnorm(n = 2500, mean = 1, sd = 1))
#'  
#' # High overlap/similarity 
#' ( I <- overlap(p1,p2) ) 
#' 
#' @import raster
#' @export 
overlap <- function(x, y){
  classes = c("SpatialGridDataFrame", "SpatialPixelsDataFrame","RasterLayer", "matrix") 
  if(!any(class(x) %in% classes))
    stop("x must be sp raster, rasterLayer or matrix object")
  if(!any(class(y) %in% classes))
    stop("y must be sp raster, rasterLayer or matrix object")	
  if (any(class(x) %in% "RasterLayer")) x <- as.matrix(x)
    if (any(class(y) %in% "RasterLayer")) y <- as.matrix(y) 
      if (any(class(x) %in% classes[1:2])) x <- as.matrix(raster::raster(x))
    if (any(class(y) %in% classes[1:2])) y <- as.matrix(raster::raster(y)) 
  if(length(which(dim(x) == dim(y))) != 2) 
    stop('matrix / raster objects must be of the same extent')
  if (min(c(x, y), na.rm = TRUE) < 0) 
    stop('all values must be positive') 	
  pos <- which(is.finite(x) & is.finite(y))
    H <- sqrt(sum((sqrt((x[pos] / sum(x[pos])))-sqrt((y[pos]/sum(y[pos]))))^2)) 
  # calculate original and corrected Warren-I statistic 	
  return ( list( original.I = (1 - 0.5 * H), 
           corrected.I = (1 - (H^2)/2) ) )  
}
