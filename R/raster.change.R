#' @title Raster change between two nominal rasters
#' @description Compares two categorical rasters with a variety of statistical options
#'       
#' @param x              First raster for comparison, rasterLayer class object    
#' @param y              Second raster for comparison, rasterLayer class object   
#' @param d              Rectangular window size, must be odd but not necessarily square
#' @param stat           Statistic to use in comparison, please see details for options. 
#' @param w              Weights if stat="kappa", must represent same classes as input rasters
#' @param out.raster     Optional output raster
#' @param mask           (FALSE/TRUE) mask output to original rasters 
#' @param force.memory   (FALSE/TRUE) Force in memory processing, may fail with insufficient RAM
#'
#' @return A raster layer or stack object one of the following layers:
#' \itemize{ 
#' \item   kappa             Kappa or Weighted Kappa statistic (if stat = "kappa")
#' \item   correlation       Paired t.test statistic  (if stat = "cor")
#' \item   entropy           Delta entropy  (if stat = "entropy")
#' \item   divergence        Kullback-Leibler divergence (if stat = "divergence")
#' \item   cross.entropy     Cross-entropy (if stat = "cross.entropy")
#' \item   t.test            Paired t.test statistic  (if stat = "t.test")
#' \item   p.value           p-value of the paired t.test statistic (if stat = "t.test")
#'  } 
#'
#' @note This function provides a various statistics for comparing two classified maps. Valid options 
#'       are: "kappa", "wkappa", "t.test", "cor", "entropy", "cross-entropy", "divergence". 
#'       Kappa and ttest values < 0 are reported as 0. For a weighted kappa, a matrix must be 
#'       provided that correspond to the pairwise weights for all values in both rasters. 
#'       Delta entropy is derived by calculating Shannon's on each focal window then differencing them (e(x) - e(y))
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and Psychological 
#'   Measurement, 20:37-46 
#' @references
#' McHugh M.L. (2012) Interrater reliability: the kappa statistic. Biochemia medica, 22(3):276–282. 
#' 
#' @examples
#' \dontrun{
#'  library(sp)                                            
#'  library(raster)                                                                                                
#'  data(meuse.grid)
#'  r1 <- sp::SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)
#'	r1 <- raster(r1)
#'	  na.idx <- which(!is.na(r1[]))
#'	  r1[na.idx] <- round(runif(length(na.idx), 1,5),0)
#'  r2 <- sp::SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid) 
#'	r2 <- raster(r2)
#'	  r2[na.idx] <- round(runif(length(na.idx), 1,5),0)
#'	  
#'	s = 11    
#'  ( r.kappa <- raster.change(r1, r2, d = s, mask = TRUE) )   
#'  ( r.ttest <- raster.change(r1, r2, d = s, stat="t.test", mask = TRUE) )
#'  ( r.ent <- raster.change(r1, r2, d = s, stat="entropy", mask = TRUE) )   
#'  ( r.cor <- raster.change(r1, r2, d = s, stat="cor", mask = TRUE) )
#'  ( r.ce <- raster.change(r1, r2, d = s, stat = "cross-entropy", mask = TRUE) )	  
#'  ( r.kl <- raster.change(r1, r2, d = s, stat = "divergence", mask = TRUE) )	
#'      
#'  par(mfrow=c(3,2))
#'    plot(r.kappa, main="Kappa")
#'    plot(r.ttest[[1]], main="Paired t-test")
#'    plot(r.ent, main="Delta Entropy")
#'    plot(r.cor, main="Rank Correlation")
#'    plot(r.kl, main="Kullback-Leibler")
#'    plot(r.ce, main="cross-entropy")
#'
#' }
#' 
#' @export raster.change
raster.change <- function(x, y, d = c(3,3), stat = c("kappa", "wkappa", "t.test",  
                          "cor", "entropy", "cross-entropy", "divergence"), w = NULL,  
                          out.raster = NULL, mask = FALSE, force.memory = FALSE) {
	stat = stat[1]
      options(warn=-1)
    if(stat == "wkappa") stop("Sorry, weighted kappa is not yet implemented")	  
	if (class(x) != "RasterLayer")
	  stop(deparse(substitute(x)), " Must be a raster object")
	if (class(y) != "RasterLayer") 
      stop(deparse(substitute(y)), " Must be a raster object")
    if( any( (dim(x)[1:2] == dim(y)[1:2]) == FALSE) ) 
	  stop("Rasters dimensions do not match") 
	if(any(round(d) %% 2 == 0))
      stop("Dimensions for a rectangular window must be an odd number")
	if(length(d) < 2) d = c(d,d)
	if( any( (stat %in% c("kappa", "wkappa", "t.test", "cor", "entropy", 
	                      "cross-entropy", "divergence")) == FALSE) )
      stop("Not a valid option for evaluation statistic")
	  
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
    
	# Add weights matrix matching code   
	#  match.weights <- function(wts, classes) {
    #  # ...
    #  }	
	   
  r <- stack(x,y)
  if (!raster::canProcessInMemory(x) | !is.null(out.raster)) {
    if(force.memory) { pm = TRUE } else { pm = FALSE }
	if(is.null(out.raster)) out.raster = "xxxx01.tif"
	  if(stat == "t.test") {
        out <- raster::writeStart(r, out.raster, overwrite=TRUE)
	  } else {
        out <- raster::writeStart(r[[1]], out.raster, overwrite=TRUE)
      }	  
  } else {
    pm = TRUE
	  if(stat == "t.test") {
        out <- stack(x,y)
	  } else {
        out <- raster(x)
      }	  
  }
  # process in memory	
  if(pm == TRUE) {
    v <- raster::getValuesFocal(r[[1:2]], 1, nrow(r[[1]]), ngb = d, 
	                            array = FALSE)
    mi <- rep(NA,nrow(v[[1]]))
	  if(stat == "t.test") mp <- rep(NA,nrow(v[[1]])) 
        for(i in 1:nrow(v[[1]]) ) {
  	      xy <- na.omit( data.frame(x=v[[1]][i,],y=v[[2]][i,]) )
          if( nrow(xy) > 3 ) {
            x.val <- as.vector(xy$x)
  	        y.val <- as.vector(xy$y) 
  	      } else {  
  	        x.val <- NA
            y.val <- NA
          }
          if(length(x.val) > 2) {
		    if(stat == "kappa") {
		      k <- cohens(x.val, y.val)
		  	    k[k < 0] <- 0
  	          mi[i] <- k 
		    } else if(stat == "cor") {		
			  mi[i] <- cor(x.val, y.val, method = "spearman")
		    } else if(stat == "entropy") {			  
			  mi[i] <- (-sum(prop.table(x.val) * log(prop.table(x.val)))) -  
			           (-sum(prop.table(y.val) * log(prop.table(y.val))))
		    } else if(stat == "t.test") {
              tt <- as.numeric(stats::t.test(x.val,y.val)[1])
                tt[tt < 0] <- 0 			  
  	              mi[i] <- tt
		  	  mp[i] <- as.numeric(stats::t.test(x.val,y.val)[3])
		    } else if(stat == "wkappa") {
		      # Need to add weight matrix function
		      # mi[i] <- spatialEco::weighted.kappa(x,y)
		    } else if(stat == "cross-entropy") {
			  mi[i] <- divergence(x.val, y.val, type = "cross-entropy")
		    } else if(stat == "divergence") {
			  mi[i] <- divergence(x.val, y.val, type = "Kullback-Leibler")
			}			
          } else {
            mi[i] <- NA
		    if(stat == "t.test") mp[i] <- NA
          }
        }
      options(warn=0)
    if(stat == "t.test") {
      out <- setValues(out, mi, layer=1)
      out <- setValues(out, mp, layer=2)
        if(mask) out <- raster::mask(out, x)	  
          names(out) <- c("ttest", "pvalue")      
	} else {
	  out <- setValues(out, mi)
	    if(mask) out <- raster::mask(out, x)
	      names(out) <- stat  
    }
	
  # process out of memory	
  } else {
    for( rl in 1:nrow(r) ) { 
      v <- getValuesFocal(r[[1:2]], row = rl, nrows = 1, ngb = d, 
	                      array = FALSE)
        mi <- rep(NA,nrow(v[[1]]))
          if(stat == "t.test") mp <- rep(NA,nrow(v[[1]])) 
          for(i in 1:nrow(v[[1]]) ) {
  		    xy <- na.omit( data.frame(x=v[[1]][i,],y=v[[2]][i,]))
          if(length(x.val) > 2) {
		    if(stat == "kappa") {
		      k <- cohens(x.val, y.val)
		  	    k[k < 0] <- 0
  	          mi[i] <- k 
		    } else if(stat == "cor") {		
			  mi[i] <- cor(x.val, y.val, method = "spearman")
		    } else if(stat == "entropy") {			  
			  mi[i] <- (-sum(prop.table(x.val) * log(prop.table(x.val)))) -  
			           (-sum(prop.table(y.val) * log(prop.table(y.val))))
		    } else if(stat == "t.test") {
              tt <- as.numeric(stats::t.test(x.val,y.val)[1])
                tt[tt < 0] <- 0 			  
  	              mi[i] <- tt
		  	  mp[i] <- as.numeric(stats::t.test(x.val,y.val)[3])
		    } else if(stat == "wkappa") {
		      # Need to add weight matrix function
		      # mi[i] <- spatialEco::weighted.kappa(x,y)
		    } else if(stat == "cross-entropy") {
			  mi[i] <- divergence(x.val, y.val, type = "cross-entropy")
		    } else if(stat == "divergence") {
			  mi[i] <- divergence(x.val, y.val, type = "Kullback-Leibler")
			}
          } else {
            mi[i] <- NA
		    if(stat == "t.test") mp[i] <- NA
          }
        }
      if(stat != "t.test") {	  
        out <- writeValues(out, mi, start = rl)
	  } else {
        out[[1]] <- writeValues(out[[1]], mi, start = rl)
	    out[[2]] <- writeValues(out[[2]], mi, start = rl)
      }  
    }
	  options(warn=0)
    writeStop(out)
  }
return(out)  
} #end of function bracket
  