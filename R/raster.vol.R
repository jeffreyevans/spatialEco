#' @title Raster Percent Volume
#' @description Calculates a percent volume on a raster or based on a 
#'              systematic sample
#' 
#' @param x        A terra SpatRaster class object
#' @param p        percent raster-value volume
#' @param sample   (FALSE/TRUE) base volume on systematic point sample 
#' @param spct     sample percent, if sample (TRUE)
#' @param type     If sample=TRUE type of sample, options are "random" or "regular"
#'
#' @note 
#' Since this model needs to operate on all of the raster values, it is not memory safe 
#'
#' @return 
#' if sample (FALSE) binary raster object with 1 representing designated 
#' percent volume else, if sample (TRUE) n sf POINT object with points 
#' that represent the percent volume of the sub-sample
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \donttest{
#' library(terra)
#'   r <- rast(ncols=100, nrows=100)
#'     r[] <- runif(ncell(r), 0, 1)
#'     r <- focal(r, w=focalMat(r, 6, "Gauss"))
#'   #r[sample(1:ncell(r)),10] <- NA
#'   
#'   # full raster percent volume 
#'   p30 <- raster.vol(r, p=0.30)
#'   p50 <- raster.vol(r, p=0.50)
#'   p80 <- raster.vol(r, p=0.80)
#'
#' opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(2,2))
#'     plot(r, col=cm.colors(10), main="original raster")
#'     plot(p30, breaks=c(0,0.1,1), col=c("cyan","red"), legend=FALSE,
#'	      main="30% volume")
#'     plot(p50, breaks=c(0,0.1,1), col=c("cyan","red"), legend=FALSE,
#'	      main="50% volume")
#'     plot(p80, breaks=c(0,0.1,1), col=c("cyan","red"), legend=FALSE,
#'	      main="80% volume")
#' par(opar)  
#' }
#'
#' @export raster.vol    	
raster.vol <- function(x, p = 0.75, sample = FALSE, spct = 0.05,
                       type=c("random","regular")) {
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")					   
  if( sample == FALSE ) { 
	den <- x[][,1]
      z <- sort(den[!is.na(den)], decreasing=TRUE)
        y <- cumsum(as.numeric(z))
          i <- sum(y <= p * y[length(y)]) 
	        vol <- x
			  vol[] <- as.integer(den >= z[i])
    return( vol ) 
  } else {
    ss = round(terra::ncell(x) * spct, digits = 0)
    den <- sf::st_as_sf(terra::spatSample(x, size=ss, method=type[1],  
                        na.rm=TRUE, as.points=TRUE, values=TRUE))
      names(den)[1] <- "den" 						
      den$idx <- 1:nrow(den) 
	  den$cls = 0  
    #den <- data.frame(idx = 1:nrow(den), den = den[,1], cls = 0)
	  sum.p <- sum(den$den, na.rm = TRUE) * p
        den <- den[order(-den$den),] 
    
	i=0; j=0
      while(i <= sum.p) { 
        j=j+1
        if(!is.na(den$den[j])) {
          i = i + den$den[j]
          den$cls[j] <- 1
        } else {
          den$cls[j] <- NA
      }	
    }
    den <- den[order(den$idx),]
      return( den[den$cls == 1 ,] )	
  }
}

