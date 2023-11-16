#' @title Niche overlap (Warren's-I)
#' @description Similarity Statistic for Quantifying Niche Overlap using Warren's-I
#'
#' @param x    A matrix or SpatRaster raster class object
#' @param y    A matrix or SpatRaster raster class object
#'             with the same dimensions of x
#'
#' @details 
#' The overlap function computes the I similarity statistic (Warren et al. 2008)  
#' of two overlapping niche estimates. Similarity is based on the Hellenger distance. 
#' It is assumed that the input data share the same extent and cellsize and all values 
#' are positive.
#' 
#' The I similarity statistic sums the pair-wise differences between two
#' predictions to create a single value representing the similarity of the two
#' distributions. The I similarity statistic ranges from a value of 0, where
#' two distributions have no overlap, to 1 where two distributions are
#' identical (Warren et al., 2008). The function is based on code  
#' from Jeremy VanDerWal
#'
#' @return A vector (single value) representing the I similarity statistic
#'
#' @author Jeffrey Evans <jeffrey_evans@@tnc.org> and Jeremy VanDerWal  
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
#' @export overlap 
overlap <- function(x, y){
  if(!inherits(x, c("SpatRaster", "matrix")))
    stop(deparse(substitute(x)), " ust be a SpatRaster or matrix object")
  if(!inherits(y, c("SpatRaster", "matrix")))
   stop(deparse(substitute(y)), " ust be a SpatRaster or matrix object")
  if (any(class(x) %in% "SpatRaster")) x <- terra::as.matrix(x)
  if (any(class(y) %in% "SpatRaster")) y <- terra::as.matrix(y)     
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
