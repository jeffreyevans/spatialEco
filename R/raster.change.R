#' @title Raster change between two nominal rasters
#' @description Compares two categorical rasters with a variety of 
#'              statistical options
#'       
#' @param x            A terra SpatRaster 
#' @param y            A terra SpatRaster for comparison to x   
#' @param s            Integer or matrix for defining Kernel, 
#'                     must be odd but not necessarily square
#' @param stat         Statistic to use in comparison, please see details for 
#'                     options. 
#' @param ...          Additional arguments passed to terra::focalPairs
#'
#' @return A terra SpatRaster layer containing one of the following layers:
#' \itemize{ 
#' \item   kappa         Kappa or Weighted Kappa statistic (if stat = "kappa")
#' \item   correlation   Paired t.test statistic  (if stat = "cor")
#' \item   entropy       Local entropy  (if stat = "entropy")
#' \item   divergence    Kullback-Leibler divergence (if stat = "divergence")
#' \item   cross.entropy Local Cross-entropy (if stat = "cross.entropy")
#' \item   t.test        Paired t.test statistic  (if stat = "t.test")
#' \item   p.value       p-value of the paired t.test statistic (if stat = "t.test")
#'  } 
#'
#' @description
#' This function provides a various statistics for comparing two classified maps. 
#' Valid options are:
#' \itemize{ 
#' \item   kappa - Cohen's Kappa 
#' \item   t.test - Two-tailed paired t-test 
#' \item   cor - Persons Correlation 
#' \item   entropy - Delta entropy 
#' \item   cross-entropy - Cross-entropy loss function 
#' \item   divergence - Kullback-Leibler divergence (relative entropy) 
#' }
#'
#' @note
#' Kappa and t-test values < 0 are reported as 0. For a weighted kappa, a matrix must 
#' be provided that correspond to the pairwise weights for all values in both rasters. 
#' Delta entropy is derived by calculating Shannon's on each focal window then 
#' differencing  them (e(x) - e(y)). The s argument can be a single scalar, defining
#' a symmetrical kernel, two scalers defining the dimensions of the kernel eg., c(3,5)
#' or a matrix defining the kernel say, resulting from terra::focalMat   
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational  
#'   and Psychological Measurement, 20:37-46 
#' @references
#' McHugh M.L. (2012) Interrater reliability: the kappa statistic. 
#'   Biochemia medica, 22(3):276–282. 
#' @references
#'  Kullback, S., R.A. Leibler (1951). On information and sufficiency. Annals of 
#'    Mathematical Statistics. 22(1):79–86
#' 
#' @examples
#' \donttest{
#'  library(sf) 
#'  library(terra) 
#'   
#'  e <- ext(179407.8, 181087.9, 331134.4, 332332.1)
#'  r1 <- rast(e, resolution=20)
#'    r1[] <- sample(1:5, ncell(r1), replace=TRUE)
#'  r2 <- rast(e, resolution=20)
#'    r2[] <- sample(1:5, ncell(r2), replace=TRUE)
#'  	  
#'  d = 5 # kernel    
#'  ( r.kappa <- raster.change(r1, r2, s = d) )   
#'  ( r.ttest <- raster.change(r1, r2, s = d, stat="t.test") )
#'  ( r.ent <- raster.change(r1, r2, s = d, stat="entropy") )   
#'  ( r.cor <- raster.change(r1, r2, s = d, stat="cor") )
#'  ( r.ce <- raster.change(r1, r2, s = d, stat = "cross-entropy") )
#'  ( r.kl <- raster.change(r1, r2, s = d, stat = "divergence") )	
#'        
#'    opar <- par(no.readonly=TRUE)
#'    par(mfrow=c(3,2))
#'      plot(r.kappa, main="Kappa")
#'      plot(r.ttest[[1]], main="Paired t-test")
#'      plot(r.ent, main="Delta Entropy")
#'      plot(r.cor, main="Rank Correlation")
#'      plot(r.kl, main="Kullback-Leibler")
#'      plot(r.ce, main="cross-entropy")
#'    par(opar) 
#' }
#' 
#' @export raster.change
raster.change <- function(x, y, s = 3, stat = c("kappa", "t.test",    
                          "cor", "entropy", "cross-entropy", 
					      "divergence"), ...) {
  stat = stat[1]
  if (!inherits(x, "SpatRaster"))
    stop(deparse(substitute(x)), " Must be a terra SpatRaster object")
  if (!inherits(y, "SpatRaster"))
    stop(deparse(substitute(y)), " Must be a terra SpatRaster object")
  if(!any((dim(x)[1:2] == dim(y)[1:2]))) 
    stop("Rasters dimensions do not match") 
  if(!terra::ext(x) ==  terra::ext(y)) 
    stop("Rasters extents do not match") 
  if(stat == "wkappa") 
    stop("Sorry; weighted kappa is not yet implemented")  
  if(!any( (stat %in% c("kappa", "wkappa", "t.test", "cor",  
            "entropy", "cross-entropy", "divergence"))) )
    stop("Not a valid option for evaluation statistic")
  if(!inherits(s, "matrix")) {
    if(any(round(s) %% 2 == 0))
      stop("Dimensions for kernel must be an odd number")
    if(length(s) < 2) s = c(s,s)
    s <- matrix(1, s[1], s[2])
  } else {
    if(any(dim(s) %% 2 == 0))
      stop("Dimensions for kernel must be an odd number")
  }	  
  cohens <- function(x, y) {
    x <- factor(x)
    y <- factor(y)
      lvl <- unique(c(levels(x), levels(y)))
        x <- factor(x, levels = lvl)
          y <- factor(y, levels = lvl)
            x <- table(x, y)
            d <- diag(x)
          n <- sum(x)
        nc <- ncol(x)
      colFreqs <- colSums(x) / n
    rowFreqs <- rowSums(x) / n
    return( (sum(d)/n - crossprod(colFreqs, rowFreqs)[1])/ 
            (1 - crossprod(colFreqs, rowFreqs)[1]) )
  }

  divergence <- function(x, y, type = c("Kullback-Leibler", "cross-entropy")) {
    type = type[1]
    if(!is.vector(x) | !is.vector(y))
      stop("x and y must be numeric of character vectors")
    if(any(type %in% c("Kullback-Leibler", "cross-entropy")==FALSE))
      stop("Not a valid option for statistic type")
    q <- table(x) / sum(table(x))  # observed or approximated
    p <- table(y) / sum(table(y))  # estimated or probability 
    classes <- intersect(names(q), names(p))
      p <- p[which(names(p) %in% classes)]
      q <- q[which(names(q) %in% classes)]
    if(type == "cross-entropy") {
      return( -sum( q, log(p) ) ) 
    } else if(type == "Kullback-Leibler") {
      return( sum( p * log(p / q) ) )
    }
  }  
	
  ent <- function(x,y) {
    return((-sum(prop.table(x) * log(prop.table(x)))) -  
   (-sum(prop.table(y) * log(prop.table(y)))))
  }   
  
  ttest <- function(x,y) {
    tt <- tryCatch(stats::t.test(x,y), 
	     error = function(e) {})
	if(is.null(tt)) {
      tt <- c(NA,NA) 
	} else {	
      tt <- c(tt$statistic, tt$p.value)
    }    
    return(tt)
  }
  
  if(stat == "kappa") {	   
    r <- terra::focalPairs(c(x,y), w=s, function(x, y) { cohens(x, y) }, ...)
  }						 
  if(stat == "divergence") {
    r <- terra::focalPairs(c(x,y), w=s, function(x, y) { 
                         divergence(x, y, type = "Kullback-Leibler") }, ...)  
   }
  if(stat == "cross-entropy") {
    r <- terra::focalPairs(c(x,y), w=s, function(x, y) { divergence(x, y,
  				         type = "cross-entropy") }, ...)  
   }   
  if(stat == "cor") {
    r <- terra::focalPairs(c(x,y), w=s, function(x, y){ stats::cor(x, y, 
  					     method = "spearman") }, ...) 
  }				    
  if(stat == "entropy") {
    r <- terra::focalPairs(c(x,y), w=s, function(x, y) { ent(x, y) }, ...)
  }  
  if(stat == "t.test") {
    r <- terra::focalPairs(c(x,y), w=s, function(x, y){ ttest(x,y) }, ...)
      names(r) <- c("t", "p.value")					
  }
return(r)  
}
  