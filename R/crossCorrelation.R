#' @title Spatial cross correlation
#' @description Calculates univariate or bivariate spatial cross-correlation using  
#'              local Moran's-I (LISA), following Chen (2015)
#'
#' @param x               Vector of x response variables
#' @param y               Vector of y response variables, if not specified the 
#'                        univariate  statistic is returned
#' @param coords          A matrix of coordinates corresponding to (x,y), only 
#'                        used if w = NULL. Can also be an sp object with relevant 
#'                        x,y coordinate slot (ie., points or polygons)
#' @param w               Spatial neighbors/weights in matrix format. Dimensions 
#'                        must match (n(x),n(y)) and be symmetrical. If w is not defined 
#'                        then a default method is used.
#' @param type            c("LSCI","GSCI") Return Local Spatial Cross-correlation Index (LSCI) 
#'                        or Global Spatial cross-correlation Index (GSCI)
#' @param k               Number of simulations for calculating permutation distribution  
#'                        under the null hypothesis of no spatial autocorrelation
#' @param dist.function   ("inv.power", "neg.exponent", "none") If w = NULL, the default method 
#'                        for deriving spatial weights matrix, options are: inverse power 
#'                        or negative exponent, none is for use with a provided matrix    
#' @param scale.xy        (TRUE/FALSE) scale the x,y vectors, if FALSE it is assumed that  
#'                        they are already scaled following Chen (2015) 
#' @param scale.partial   (FALSE/TRUE) rescale partial spatial autocorrelation statistics
#' @param scale.matrix    (FALSE/TRUE) If a neighbor/distance matrix is passed, should it 
#'                         be scaled using (w/sum(w))
#' @param alpha = 0.05     confidence interval (default is 95 pct)
#' @param clust           (FALSE/TRUE) Return approximated lisa clusters
#' @param return.sims     (FALSE/TRUE) Return randomizations vector n = k
#'
#'
#' @details
#' In specifying a distance matrix, you can pass a coordinates matrix or spatial
#' object to coords or alternately, pass a distance or spatial weights matrix to
#' the w argument. If the w matrix represents spatial weights dist.function="none"
#' should be specified. Otherwise, w is assumed to represent distance and will be
#' converted to spatial weights using inv.power or neg.exponent. The w distances
#' can represent an alternate distance hypothesis (eg., road, stream, network distance)
#' Here are example argument usages for defining a matrix.      
#' * IF coords=x, w=NULL, dist.function= c("inv.power", "neg.exponent")
#'    A distance matrix is derived using the data passed to coords then 
#'    spatial weights derived using one of the dist.function options
#' * IF cords=NULL, w=x, dist.function= c("inv.power", "neg.exponent")
#'     It is expected that the distance matrix specified with w represent 
#'     some form of distance then the spatial weights are derived using 
#'     one of the dist.function options 
#' * IF cords=NULL, w=x, dist.function="none" 
#'     It is assumed that the matrix passed to w already represents 
#'     the spatial weights 
#'
#' @return 
#' When not simulated k=0, a list containing:
#' * I - Global autocorrelation statistic
#' * SCI - - A data.frame with two columns representing the xy and yx autocorrelation
#' * nsim - value of NULL to represent p values were derived from observed data (k=0)
#' * p - Probability based observations above/below confidence interval
#' * t.test - Probability based on t-test
#'   \item clusters - If "clust" argument TRUE, vector representing LISA clusters
#'
#' When simulated (k>0), a list containing: 
#'  * I - Global autocorrelation statistic
#'  * SCI - A data.frame with two columns representing the xy and yx autocorrelation
#'  * nsim - value representing number of simulations
#'  * global.p - p-value of global autocorrelation statistic
#'  * local.p - Probability based simulated data using successful rejection of t-test
#'  * range.p - Probability based on range of probabilities resulting from paired t-test
#'  * clusters - If "clust" argument TRUE, vector representing lisa clusters
#'
#' @references
#' Chen, Y.G. (2012) On the four types of weight functions for spatial contiguity 
#'    matrix. Letters in Spatial and Resource Sciences 5(2):65-72 
#' @references
#' Chen, Y.G. (2013) New approaches for calculating Moran’s index of spatial 
#'   autocorrelation. PLoS ONE 8(7):e68336
#' @references 
#' Chen, Y.G. (2015) A New Methodology of Spatial Cross-Correlation Analysis. 
#'   PLoS One 10(5):e0126158. doi:10.1371/journal.pone.0126158
#'
#' @examples
#' # replicate Chen (2015)
#'  data(chen)
#' ( r <- crossCorrelation(x=chen[["X"]], y=chen[["Y"]], w = chen[["M"]],  
#'                         clust=TRUE, type = "LSCI", k=0, 
#'                         dist.function = "inv.power") ) 
#'
#' \donttest{
#' library(sf)
#' library(spdep)
#'  
#'   if (require(sp, quietly = TRUE)) {
#'    data(meuse, package = "sp")
#'    meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
#'   } 
#' 
#' #### Using a default spatial weights matrix method (inverse power function)
#' ( I <- crossCorrelation(meuse$zinc, meuse$copper, 
#'              coords = st_coordinates(meuse)[,1:2], k=99) )
#'   meuse$lisa <- I$SCI[,"lsci.xy"]
#'     plot(meuse["lisa"], pch=20)
#' 
#' #### Providing a distance matrix
#' if (require(units, quietly = TRUE)) {
#'   Wij <- units::drop_units(st_distance(meuse))
#'  ( I <- crossCorrelation(meuse$zinc, meuse$copper, w = Wij, k=99) )
#' 
#' #### Providing an inverse power function weights matrix
#'   Wij <- 1 / Wij
#'     diag(Wij) <- 0 
#'       Wij <- Wij / sum(Wij) 
#'         diag(Wij) <- 0
#'  ( I <- crossCorrelation(meuse$zinc, meuse$copper, w = Wij, 
#'                          dist.function = "none", k=99) )
#' }
#' } 
#' 
#' @md 
#' @export crossCorrelation
crossCorrelation <- function(x, y = NULL, coords = NULL, w = NULL, type = c("LSCI", "GSCI"), 
                             k = 999, dist.function = c("inv.power", "neg.exponent", "none"), 
							 scale.xy = TRUE, scale.partial = FALSE, scale.matrix = FALSE, 
							 alpha = 0.05, clust = TRUE, return.sims = FALSE) {
	if(missing(x)) stop("x must be specified")
    if(is.null(y)) y = x						 
      if(length(y) != length(x)) stop("(X,Y) are not equal")
        if( length(which(is.na(x))) != 0 | length(which(is.na(y))) != 0) 
	      stop("NA's not permitted in (X,Y)")
            if( k == 0) message("Permutation is not being run, estimated p will be based on observed")
	          if(scale.xy == FALSE) warning("It is assumed that x,v vectors are already scaled") 		
    type = type[1]; dist.function = dist.function[1]		  
	  n <- length(x)
  if( is.null(w) ) {
    if( is.null(coords) ) stop("If no Wij matrix is provided, a coordinates matrix is required")
      w <- sp::spDists( coords ) 
  } else {
    if(!class(w)[1] == "matrix") stop("Spatial weights must be in matrix form")					   
      if(ncol(w) != length(x) | nrow(w) != length(x)) stop("Spatial weights matrix must be symmetrical and match x")
        w[which(is.na(w))] <- 0
  }  
    if( dist.function == "inv.power" ) {
      message("Calculating spatial weights matrix using inverse power function")
	    w <- 1 / w
          diag(w) <- 0 
        w <- w / sum(w) 
    } else if (dist.function == "neg.exponent") { 
      message("Calculating spatial weights matrix using negative exponent")    
	  diag(w) <- NA 
	  mu <- mean(w, na.rm=TRUE)
      for(i in 1:nrow(w)) {
        for(j in 1:nrow(w)) {
		 w[i,j] <- round(exp( (-2 * w[i,j]) / mu ),6)
        }
      }  
      diag(w) <- 0
    } else if (dist.function == "none") { 
      message("Wij matrix is being left raw")	
    } else {
      stop("Not a valid matrix option")
    }  
    if(scale.matrix) {
      if(sum(w) > 0) { w <- as.matrix(w / sum(w))}
    }   
  if( scale.xy ){  
    x <- ( x - mean(x) ) / ( stats::sd(x) * sqrt((length(x)-1)/length(x)) )
    y <- ( y - mean(y) ) / ( stats::sd(y) * sqrt((length(y)-1)/length(y)) )
  } 
  
  #### Local and global cross-correlation statistics function ####
  SCI <- function(x, y, W, type.cc, scale.cc) {
    if( type.cc == "LSCI" ) {
	# local spatial crosscorrelation index (chen's LSCIs)
	# The lsci.xy is empirically the same as Anselin's LISA
    lsci.xy = as.numeric( x*y%*%W )
    lsci.yx = as.numeric( y*x%*%W )
	  if(scale.cc) {
        lsci.xy <- (lsci.xy - min(lsci.xy)) * (1 - -1) / (max(lsci.xy) - min(lsci.xy)) + -1
        lsci.yx <- (lsci.yx - min(lsci.yx)) * (1 - -1) / (max(lsci.yx) - min(lsci.yx)) + -1
      }	
	sci =  data.frame(lsci.xy = lsci.xy, lsci.yx = lsci.yx)  
    } 
    if( type.cc == "GSCI" ) {
	# The global spatial crosscorrelation index (chen's SCI)
      gsci.xy = as.numeric( x*W%*%y ) #/ ( length(x) - 1 )
      gsci.yx = as.numeric( y*W%*%x ) #/ ( length(x) - 1 )
	    if(scale.cc) {
         gsci.xy <- (gsci.xy - min(gsci.xy)) * (1 - -1) / (max(gsci.xy) - min(gsci.xy)) + -1
  	     gsci.yx <- (gsci.yx - min(gsci.yx)) * (1 - -1) / (max(gsci.yx) - min(gsci.yx)) + -1
        }
      sci = data.frame(gsci.xy = gsci.xy, gsci.yx = gsci.yx)	  
    }
    return(sci)	
  } 
  global.i <- as.numeric(x%*%w%*%y) #/ (length(x) - 1) 
    if(type == "LSCI") { tstat = "lsci.xy" } else { tstat = "gsci.xy" }  
      sci.results <- SCI(x = x,  y = y, W = w, type.cc = type, scale.cc = scale.partial)   
      if(clust) {     
	    lisa.clust <- as.character( interaction(x > 0, w %*% y > 0) ) 
          lisa.clust <- gsub("TRUE", "High", lisa.clust)
            lisa.clust <- gsub("FALSE", "Low", lisa.clust)
	  }  
	  probs <- c(alpha / 2, 1 - alpha / 2)   
    if(k > 0) {
     message("\n Computing Permutation Distribution \n")
	 # Global Moran's-I p-value
       y.sim <- matrix(y[sample(1:n, size = n * k, replace = TRUE)], 
       		        nrow = n, ncol = k) 	
       isim <- apply(y.sim, MARGIN=2, function(j) t(x[sample(1:length(x))]) %*% w %*% j )
         ( global.p <- sum( abs(isim) > abs( global.i ) ) ) 	 

       # Local Moran's-I p-value	   
        y.sim <- matrix(y[sample(1:n, size = n * k, replace = TRUE)], 
                        nrow = n, ncol = k) 	
        isim <- apply(y.sim, MARGIN=2, function(j) {
             SCI(as.numeric(t(x[sample(1:length(x))])), j, W=w,type.cc=type,scale.cc=scale.partial)[,tstat] } )
		ttest.p <- round(apply(isim, 2, function(j) stats::t.test(sci.results[,tstat], y = j,
                          alternative = "two.sided", paired = TRUE, 
		 				  conf.level = 1-alpha)$p.value),6)
		p <- length( ttest.p[ttest.p > alpha] ) / length(ttest.p)							  
        p1 <- 2 * min(length(ttest.p[ttest.p > alpha]) / k, 
	                 length(ttest.p[ttest.p < alpha]) / k )	
	  results <- list(I=global.i, SCI=sci.results, nsim = k,  
                      global.p=global.p, local.p=p, range.p=p1)
                        if(clust) { results$clusters <- lisa.clust }					  
	                      if(return.sims) { results$simulated.I <- isim }
	    class(results) <- c("cross.cor","list") 
    } else {
     ttest.p <- round(stats::t.test(sci.results[,tstat], conf.level = 1-alpha)$p.value,6)
     #ci <- t.test(sci.results[,tstat], conf.level = 1-alpha)$conf.int
     ci <- stats::quantile(sci.results[,tstat], probs=probs)
     tstat <- sci.results[,tstat]
       p <- 2 * min(length(tstat[tstat >= ci[2]]) / length(tstat), 
	                length(tstat[tstat <= ci[1]]) / length(tstat) )
	  results <- list(I=global.i, SCI=sci.results, nsim = NULL,
                      p=p, t.test=ttest.p)
	    if(clust) { results$clusters <- lisa.clust }				  			  
	class(results) <- c("cross.cor","list")
    }
  return( invisible(results) )  
}
