#' @title Spatial cross correlation
#' @description Calculates spatial partial cross-correlation using local Moran's-I (LISA)
#'
#' @parm x               Vector of x response variables
#' @parm y               Vector of y response variables
#' @parm w               Spatial neighbors/weights in matrix format. Dimensions must match [n(x),n(y)]
#' @parm k               Number of simulations for calculating permutation distribution under the null hypothesis of no spatial autocorrelation (k=1000, default)
#' @parm scale.matrix    (TRUE/FALSE) rescale spatial weights matrix so rows sum to 1  
#' @parm scale.partial   (TRUE/FALSE) rescale partial spatial autocorrelation statistics [-1 - 1]
#' @parm alpha = 0.05    confidence interval (default is 95%)
#' @parm return.sims     (FALSE/TRUE) Return randomizations vector n = k
#'
#' @return vectors (length of x) containing:
#' \itemize{  
#'   \item {lisa} {local partial spatial correlation statistic}
#'   \item {cluster} {Cluster values indicating juxtaposition: "low/low", "low/high", "high/high", "high/low"}
#' }
#'
#' @return \strong{when simulated, a data.frame containing}
#' \itemize{  
#'   \item {I} {Global partial correlation}
#'   \item {meanI} {Mean I}
#'   \item {stdI} {Standard Deviation of I}
#'   \item {zI} {Z-score of I}
#'   \item {p1} {Probability based on observed}
#'   \item {p} {Probability based on simulated}
#' }
#'
#'@examples
#'  library(sp)
#'  library(spdep)
#'    
#'  data(meuse)
#'    coordinates(meuse) <- ~x+y  
#'  
#'  all.linked <- max(unlist(nbdists(knn2nb(knearneigh(coordinates(meuse))), 
#'                    coordinates(meuse))))  
#'  nb <- nb2listw(dnearneigh(meuse, 0, all.linked), style = "B", zero.policy = T)  
#'    Wij <- as.matrix( as(nb, "symmetricMatrix") ) 
#'  
#'  ( I <- crossCorrelation(meuse$elev, meuse$copper, w=Wij, k=500) )
#'    meuse$lisa <- I$lisa
#'    meuse$lisa.clust <- as.factor(I$cluster)
#'      spplot(meuse, "lisa")
#'      spplot(meuse, "lisa.clust") 
#'  	
#' \dontrun{	
#'  #### Simulate autocorrelated random normal variates using the eigen-decomposition
#'  ####   requires ncf package
#'  x=expand.grid(1:20, 1:20)[,1]
#'  y=expand.grid(1:20, 1:20)[,2]
#'  sdat <- data.frame(x =x,y=y,
#'                    z1=ncf::rmvn.spa(x=x, y=y, p=2, method="exp"),
#'                    z2=ncf::rmvn.spa(x=x, y=y, p=2, method="exp"))  
#'  coordinates(sdat) <- ~x+y
#'  all.linked <- max(unlist(nbdists(knn2nb(knearneigh(coordinates(sdat))), 
#'                    coordinates(sdat))))
#'  nb <- nb2listw(dnearneigh(sdat, 0, all.linked), style = "B", zero.policy = T)  
#'    Wij <- as.matrix( as(nb, "symmetricMatrix") ) 
#'  
#'  ( I <- crossCorrelation(sdat$z1, sdat$z2, w=Wij, k=1000) )
#'  dat$lisa <- I$lisa
#'  dat$lisa.clust <- as.factor(I$cluster)
#'    spplot(dat, "lisa")
#'    spplot(dat, "lisa.clust")    
#'    
#'  #### 1st order polygon contingency example 
#'  ####   requires UScensus2000tract package
#'  library(sp)
#'  library(spdep)
#'  library(UScensus2000tract)
#'  
#'  data(oregon.tract)
#'  nb <- nb2listw(poly2nb(oregon.tract), style = "B", zero.policy = T)
#'    Wij <- as.matrix( as(nb, "symmetricMatrix") )
#'  
#'  X = oregon.tract$white
#'  Y = oregon.tract$black  
#'    
#'  # Simulated bivariate lisa
#'  I <- crossCorrelation(X, Y, w=Wij, k=1000)
#'  oregon.tract$lisa <- I$lisa
#'  oregon.tract$lisa.clust <- as.factor(I$cluster)
#'    spplot(oregon.tract, "lisa")
#'    spplot(oregon.tract, "lisa.clust")
#'  
#' } 
#' 
#' @exportClass cross.cor
#' @export
crossCorrelation <- function(x, y, w, k = 1000, scale.matrix = TRUE, scale.partial = TRUE,  
                           alpha = 0.05, return.sims = FALSE) {
  if(!class(w) == "matrix") stop("Spatial weights must be in matrix form")					   
    if(ncol(w) != length(x) | nrow(w) != length(x)) stop("Spatial weights matrix must be symmetrical")		   
      if(length(y) != length(x)) stop("[X,Y] are not equal")
	    if( length(which(is.na(x))) != 0 | length(which(is.na(y))) != 0) 
		  stop("NA's not permitted in [X,Y]")	
	n <- length(x)
  w[which(is.na(w))] <- 0
   if(scale.matrix) {
     if(sum(w) > 0) { w <- as.matrix(w / rowSums(w))}
   }
   newx <- ( x - mean(x) )
   newy <- ( y - mean(y) )
   sumw <- sum(w)
   s3 <- sum(w * t(w))
     s4 <- sum(w * w)
         s5 <- sum(w %*% w)
       s6 <- sum(t(w) %*% w + w %*% t(w))
     s1 <- s4 + s3
   s2 <- 2 * s5 + s6
   morani <- t(newx) %*% w %*% newy
     pm <- (w %*% newy) * newx
       varx <- sum(newx * newx)/n
       vary <- sum(newy * newy)/n
     denom <- sumw * sqrt(varx * vary)
   morani <- morani/denom
     pm <- pm/denom
     corxy <- cor(newx, newy)
     mxy <- sum(newx * newy)/n
     mxy2 <- sum(newx^2 * newy^2)/n
   Emorani <-  - corxy/(n - 1)
     term1 <- 2 * (sumw^2 - s2 + s1)
     term1 <- term1 + (2 * s3 - 2 * s5) * (n - 3) + s3 * (n - 2) * (n - 3)
     term1 <- (term1 * mxy^2 * n)
     term2 <- 6 * (sumw^2 - s2 + s1) + (4 * s1 - 2 * s2) * (n - 3)
     term2 <- term2 + s1 * (n - 2) * (n - 3)
     term2 <-  - term2 * mxy2
     term3 <- n * (sumw^2 - s2 + s1 + (2 * s4 - s6) * (n - 3) + s4 * (n - 2) * (n - 3))
     term3 <- term3 * varx * vary
   Vmorani <- term1 + term2 + term3
     Vmorani <- Vmorani/((n - 1) * (n - 2) * (n - 3) * sumw^2 * varx * vary)
       Vmorani <- Vmorani - (mxy^2/(varx * vary))/(n - 1)^2
   sdr <- sqrt(Vmorani)
   z2 <- (morani - Emorani) / sdr
     if(scale.partial) {
     	dp <- (max(pm) + min(pm)) / 2
     	pm <- (pm - dp) / (max(pm) - dp)
     }
   if(k > 0) {
   	cat("\n ( Computing Permutation Distribution )\n")
    newy.sim <- matrix(newy[sample(1:n, size = n * k, replace = TRUE)], 
		               nrow = n, ncol = k) 					   
	isim <- apply(newy.sim, MARGIN=2, function(j) t(newx[sample(1:length(newx))]) %*% w %*% j / denom )
   	    prob1 <- length( isim[isim > as.numeric(morani)]) / k
   	  prob2 <- 1 - prob1
   	prob <- 2 * min(prob1, prob2)
	prob.inv <- c(alpha / 2, 1 - alpha / 2)
      clust <- as.character( interaction(newx > 0, w %*% newy > 0) ) 
        clust <- gsub("TRUE", "High", clust)
        clust <- gsub("FALSE", "Low", clust)
    p.inv <- quantile(isim, prob = prob.inv)	
      sig <-  pm < p.inv[1]  |  pm > p.inv[2] 	    
        clust[sig == 0] <- "Not significant"
	results <- list(lisa = as.numeric(pm), cluster = clust,  
	                summary = data.frame(I=round(morani, 6), 
	                meanI=round(Emorani, 6), stdI=round(sdr, 6), zI=round(z2, 6),
	                p1=round(2 * (1 - pnorm(abs(z2))), 6), p=round(prob, 6)))
	  if(return.sims) { results$simulated.I <-  msim }			
	class(results) <- c("cross.cor","list") 
   } else {
      clust <- as.character( interaction(newx > 0, w %*% newy > 0) ) 
        clust <- gsub("TRUE", "High", clust)
        clust <- gsub("FALSE", "Low", clust)   
    results <- list(lisa = as.numeric(pm), I = round(morani, 6), cluster = clust)
	class(results) <- c("cross.cor","list")
   }
  return( invisible(results) )  
}

#' @title Print spatial cross correlation
#' @description print method for class "cross.cor"
#' @param x    Object of class cross.cor
#' @param ...  Ignored
#'
#' @method print cross.cor
#'
#' @export
print.cross.cor <- function(x, ...) {
  if(any(names(x)=="summary")) {
  cat("Moran's-I under randomization assumptions...", "\n")
    cat("    First-order Moran's-I: ", x$summary$I, "\n")
	cat("    Mean(I): ", x$summary$meanI, "\n")
	cat("    STDV(I): ", x$summary$stdI, "\n")
  	cat("    p-value (2-sided): ", x$summary$p, "\n")
	cat("\n", "Summary statistics of local partial cross-correlation", "\n")
      print(summary(x$lisa))
	cat("\n", "Counts of cluster types")
	  print(table(x$cluster))
  } else {
    cat("First-order Moran's-I: ", x$I, "\n")
    cat("\n", "Summary statistics of local partial cross-correlation", "\n")
      print(summary(x$lisa))
  	cat("\n", "Counts of cluster types")
  	  print(table(x$cluster))
  }
}

#' @title Summary of spatial cross correlation
#' @description summary method for class "cross.cor"
#' @param x    Object of class cross.cor
#' @param ...  Ignored
#'
#' @method summary cross.cor
#'
#' @export
summary.cross.cor <- function(x, ...) {
  if(any(names(x)=="summary")) {
  cat("Moran's-I under randomization assumptions...", "\n")
    cat("    First-order Moran's-I: ", x$summary$I, "\n")
	cat("    Mean(I): ", x$summary$meanI, "\n")
	cat("    STDV(I): ", x$summary$stdI, "\n")
  	cat("    p-value (2-sided): ", x$summary$p, "\n")
	cat("\n", "Summary statistics of local partial cross-correlation", "\n")
      print(summary(x$lisa))
	cat("\n", "Counts of cluster types")
	  print(table(x$cluster))
  } else {
    cat("First-order Moran's-I: ", x$I, "\n")
    cat("\n", "Summary statistics of local partial cross-correlation", "\n")
      print(summary(x$lisa))
  	cat("\n", "Counts of cluster types")
  	  print(table(x$cluster))
  }
}
