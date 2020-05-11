#' @title Class comparison between two nominal rasters
#' @description Compares two categorical rasters using Cohen's Kappa (d) 
#'              or paired t-test statistic(s)
#'       
#' @param x          First raster for comparison, SpatialPixelsDataFrame or 
#'                   SpatialGridDataFrame object    
#' @param y          Second raster for comparison, SpatialPixelsDataFrame or 
#'                   SpatialGridDataFrame object 
#' @param x.idx      Index for the column in the x raster object  
#' @param y.idx      Index for the column in the y raster object  
#' @param d          Distance for finding neighbors, the default "AUTO" will derive 
#'                   a distance
#' @param stat       Statistic to use in comparison ("kappa", "t.test", "both")
#' @param sub.sample Should a subsampling approach be employed (FALSE/TRUE)  
#' @param type       If sub.sample = TRUE, what type of sample ("random"  
#'                   or "hexagon")
#' @param p          If sub.sample = TRUE, what proportion of population 
#'                   should be sampled
#' @param size       If sub.sample = TRUE, alternate to proportion of population (p), 
#'                   using fixed sample size               
#'
#' @return 
#' A SpatialPixelsDataFrame or SpatialPointsDataFrame with the 
#' following attributes:
#' \itemize{ 
#' \item   x        x variable used to derive Kappa (d)
#' \item   y        y variable used to derive Kappa (d)
#' \item   kappa    Kappa (d) statistic
#' \item   t.test   Paired t.test statistic  (if stat = "t.test" or "both")
#' \item   p.value  p-value of the paired t.test statistic (if stat = "t.test" 
#'                  or "both")
#'  } 
#'
#' @note 
#' This function provides a Cohen's Kappa or paired t-test to compare two 
#' classified maps. Point based subsampling is provided for computation 
#' tractability.  The hexagon sampling is recommended as it it good at 
#' capturing spatial process that includes nonstationarity and anisotropy.    
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational 
#'   and Psychological Measurement, 20:37-46 
#' 
#' @examples
#' \donttest{
#'  library(sp)                                            
#'  library(raster)
#'            
#'  data(meuse.grid)
#'  r1 <- sp::SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], 
#'                                   data = meuse.grid)
#'    r1@data$class1 <- round(runif(nrow(r1), 1,5),0)
#'  r2 <- sp::SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], 
#'                                   data = meuse.grid) 
#'	r2@data$class2 <- round(runif(nrow(r2), 1,5),0)
#'
#'  d <- class.comparison(r1, r2, x.idx = 8, y.idx = 8, stat="both")
#'  opar <- par(no.readonly=TRUE)
#'    par(mfrow=c(2,2))
#'      plot(raster(d, layer=3), main="Kappa")
#'	    plot(raster(d, layer=4), main="t.test")
#'	    plot(raster(d, layer=5), main="t.test p-value")
#'  par(opar)
#'  # Hexagonal sampling	  
#'  d.hex <- class.comparison(r1, r2, x.idx = 8, y.idx = 8, stat = "both",
#'                            sub.sample = TRUE, d = 500, size = 1000)
#'    sp::bubble(d.hex, "kappa")
#'	    d.hex <- sp.na.omit(d.hex, col.name = "t.test")
#'	  sp::bubble(d.hex, "t.test")
#' }
#' 
#' @export
class.comparison <- function(x, y, x.idx = 1, y.idx = 1, d = "AUTO", stat = "kappa",  
                             sub.sample = FALSE, type = "hexagon", p = 0.10, size = NULL) {
	if (!sp::gridded(x))
	  stop(deparse(substitute(x)), " Must be an sp raster object")
    if (!sp::gridded(y)) 
      stop(deparse(substitute(y)), " Must be an sp raster object")
    if( dim(x)[1] != dim(y)[1]) 
	  stop("Rasters dimensions do not match")	  
   if ( (d == "AUTO") == TRUE){
     cs <- x@grid@cellsize[1]
      d = sqrt(2*((cs*3)^2))
    } else {
      if (!is.numeric (d)) stop("Distance (d) must be numeric")
    }
    cohen.d <- function (pred, obs) {
     t.xy <- table(pred, obs)
       tc <- match(colnames(t.xy), rownames(t.xy))
         mtc <- matrix(ncol = ncol(t.xy), nrow = length(tc[tc == "NA"]), 0)
           nrn <- colnames(t.xy)[is.na(tc) == TRUE]
             rownames(mtc) <- nrn
               t1 <- rbind(t.xy, mtc)
                 tr <- match(rownames(t1), colnames(t1))
                   mtr <- matrix(nrow = nrow(t1), ncol = length(tr[tr == "NA"]), 0)
                     ncn <- rownames(t1)[is.na(tr) == TRUE]
                   colnames(mtr) <- ncn
                 t2 <- cbind(t1, mtr)
               sr <- sort(rownames(t2))
             mr <- match(sr, rownames(t2))
           t3 <- t(t2[mr, ])
         sc <- sort(rownames(t3))
       mc <- match(sc, rownames(t3))
       t4 <- t(t3[mc, ])
         agree <- diag(t4)
           prod1 <- apply(t4, 1, sum)
             prod2 <- agree/prod1
                user1 <- apply(t4, 2, sum)
               user2 <- agree/user1
             N <- sum(t4)
           k1 <- sum(agree)
         k2 <- sum(prod1 * user1)
       khat <- abs(((N * k1) - k2)/(N^2 - k2))
     return( round(khat, 4) )
    }

    if(stat == "both") { results <- list("kappa"=vector(), "t.test" = vector(), p.value=vector())
       } else if(stat == "t.test") { results <- list("t.test" = vector(), p.value=vector())
         } else if(stat == "kappa") { results <- list("kappa" = vector()) 
           } else { stop("Not a valid option") }	  	 

  if(sub.sample == FALSE) { 
    nb <- spdep::dnearneigh(sp::coordinates(x),0, d)  
      k <- vector()
      for(i in 1:length(nb)) {
        if( ncol(x) > 1) {
           x.var <- x@data[nb[[i]],][x.idx][,1]
	     } else {
	       x.var <- x@data[nb[[i]],]   
	    }
      if( ncol(y) > 1) {
         y.var <- y@data[nb[[i]],][y.idx][,1]
	   } else {
	     y.var <- y@data[nb[[i]],]   
	    }

	    if(stat == "kappa") {   
          results[["kappa"]] <- append(results[["kappa"]], round(cohen.d(x.var, y.var),4))
        } else if( stat == "t.test") {
          if(length(x.var) > 2) {		
            results[["t.test"]] <- append(results[["t.test"]], round(stats::t.test(x.var, y.var, 
			                              paired = TRUE)$statistic, 4))	  
	        results[["p.value"]] <- append(results[["p.value"]], round(stats::t.test(x.var, y.var, 
			                               paired = TRUE)$p.value,4))
          } else { 
            results[["t.test"]] <- append(results[["t.test"]], NA)	  
	        results[["p.value"]] <- append(results[["p.value"]], NA)
          }		  
	    } else if(stat == "both") { 
          results[["kappa"]] <- append(results[["kappa"]], round(cohen.d(x.var, y.var),4))
          if(length(x.var) > 2) {		
            results[["t.test"]] <- append(results[["t.test"]], round(stats::t.test(x.var, y.var, 
			                              paired = TRUE)$statistic, 4))	  
	        results[["p.value"]] <- append(results[["p.value"]], round(stats::t.test(x.var, y.var, 
			                               paired = TRUE)$p.value,4))							                   
          } else { 
            results[["t.test"]] <- append(results[["t.test"]], NA)	  
	        results[["p.value"]] <- append(results[["p.value"]], NA)
          }		
        } else { stop("Not a valid option") }		
    }
	  if (stat == "t.test" | stat == "both") names(results[["t.test"]]) <- NULL
      s <- x    
      s@data <- data.frame(x=x@data[,x.idx], y=y@data[,y.idx], 
	                       as.data.frame(do.call("cbind", results))) 
    } else {
	
	# Change to raster class coercion and raster::extract 
	#   nb indexes are too slow
	
    if(!is.null(size)) { n = size } else { n = round(nrow(x) * p, 0) } 
    
	if(type == "random") {
	  rs <- sample(1:length(nb), n)		  
	  for(i in 1:length(rs)) {
        if( ncol(x) > 1) {
           x.var <- x@data[nb[[rs[i]]],][x.idx][,1]
	     } else {
	       x.var <- x@data[nb[[rs[i]]],]   
	     }
      if( ncol(y) > 1) {
         y.var <- y@data[nb[[rs[i]]],][x.idx][,1]
	   } else {
	     y.var <- y@data[nb[[rs[i]]],]   
	    }
	    if(stat == "kappa") {   
          results[["kappa"]] <- append(results[["kappa"]], round(cohen.d(x.var, y.var),4))
        } else if( stat == "t.test") {
          if(length(x.var) > 2) {		
            results[["t.test"]] <- append(results[["t.test"]], round(stats::t.test(x.var, y.var, 
			                              paired = TRUE)$statistic, 4))	  
	        results[["p.value"]] <- append(results[["p.value"]], round(stats::t.test(x.var, y.var, 
			                               paired = TRUE)$p.value,4))
          } else { 
            results[["t.test"]] <- append(results[["t.test"]], NA)	  
	        results[["p.value"]] <- append(results[["p.value"]], NA)
          }		  
	    } else if(stat == "both") { 
          results[["kappa"]] <- append(results[["kappa"]], round(cohen.d(x.var, y.var),4))
          if(length(x.var) > 2) {		
            results[["t.test"]] <- append(results[["t.test"]], round(stats::t.test(x.var, y.var, 
			                              paired = TRUE)$statistic, 4))	  
	        results[["p.value"]] <- append(results[["p.value"]], round(stats::t.test(x.var, y.var, 
			                               paired = TRUE)$p.value,4))							                   
          } else { 
            results[["t.test"]] <- append(results[["t.test"]], NA)	  
	        results[["p.value"]] <- append(results[["p.value"]], NA)
          }		
        } else { stop("Not a valid option") }
	  
	}
	  if (stat == "t.test" | stat == "both") names(results[["t.test"]]) <- NULL
	  s <- x[rs,]
      s@data <- data.frame(x=x@data[rs,][x.idx], y=y@data[rs,][y.idx],
      	                   as.data.frame(do.call("cbind", results)))
    } else if(type == "hexagon") {	
      e <- as(raster::extent(x), "SpatialPolygons")  
	  s <- sp::spsample(e, n = n,  type = "hexagonal")
	    s <- sp::SpatialPointsDataFrame(s, data.frame(ID=1:length(s)))
	  r.ids <- raster::extract(raster::raster(x), s, method='simple', buffer=d,  
	                           small=TRUE, cellnumbers=TRUE) 
		names(r.ids) <- 1:length(r.ids) 			 
	  rm.na <- function(x) {
        x <- x[stats::complete.cases(x),]
	  	if( length(x) < 4 ) {
	  	  x <- NA
	  	} else {
	      x <- as.numeric( x[,1] ) 
	  	}
	    return(x)
	  }	
	r.ids <- lapply(r.ids, FUN = rm.na) 
    r.ids <- r.ids[sapply(r.ids, function(z) !all(is.na(z)))]
	x.r <- raster::raster(x, layer=x.idx)
	y.r <- raster::raster(y, layer=x.idx)
	  for(i in 1:length(r.ids)) {
	    x.var <- x.r[r.ids[[i]]]
		  x.var <- x.var[!is.na(x.var)]
		y.var <- y.r[r.ids[[i]]]
		  y.var <- y.var[!is.na(y.var)]
		  
	    if(stat == "kappa") {   
          results[["kappa"]] <- append(results[["kappa"]], round(cohen.d(x.var, y.var),4))
        } else if( stat == "t.test") {
          if(length(x.var) > 2) {		
            results[["t.test"]] <- append(results[["t.test"]], round(stats::t.test(x.var, y.var, 
			                              paired = TRUE)$statistic, 4))	  
	        results[["p.value"]] <- append(results[["p.value"]], round(stats::t.test(x.var, y.var, 
			                               paired = TRUE)$p.value,4))
          } else { 
            results[["t.test"]] <- append(results[["t.test"]], NA)	  
	        results[["p.value"]] <- append(results[["p.value"]], NA)
          }		  
	    } else if(stat == "both") { 
          results[["kappa"]] <- append(results[["kappa"]], round(cohen.d(x.var, y.var),4))
          if(length(x.var) > 2) {		
            results[["t.test"]] <- append(results[["t.test"]], round(stats::t.test(x.var, y.var, 
			                              paired = TRUE)$statistic, 4))	  
	        results[["p.value"]] <- append(results[["p.value"]], round(stats::t.test(x.var, y.var, 
			                               paired = TRUE)$p.value,4))							                   
          } else { 
            results[["t.test"]] <- append(results[["t.test"]], NA)	  
	        results[["p.value"]] <- append(results[["p.value"]], NA)
          }		
        } else { stop("Not a valid option") }

	  }
	  s <- s[as.numeric(names(r.ids)),]
	  if (stat == "t.test" | stat == "both") names(results[["t.test"]]) <- NULL
      s@data <- data.frame(x=x@data[as.numeric(names(r.ids)),][x.idx], 
	                       y=y@data[as.numeric(names(r.ids)),][y.idx],
      	                   as.data.frame(do.call("cbind", results)))
	}
  }
return( s )  
}
