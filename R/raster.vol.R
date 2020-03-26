#' @title Raster Percent Volume
#' @description Calculates a percent volume on a raster or based on a 
#'              systematic sample
#' 
#' @param x        raster class object
#' @param p        percent raster-value volume
#' @param sample   base volume on systematic point sample (TRUE/FALSE)
#' @param spct     sample percent, if sample (TRUE)
#'
#' @return 
#' if sample (FALSE) binary raster object with 1 representing designated 
#' percent volume else, if sample (TRUE) n sp SpatialPointsDataFrame object 
#' with points that represent the percent volume of the sub-sample
#'
#' @note 
#' Since this model needs to operate on all of the raster values, 
#' it is not memory safe 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \donttest{
#' require(raster)
#'   r <- raster(ncols=100, nrows=100)
#'     r[] <- runif(ncell(r), 0, 1)
#'     r <- focal(r, w=focalWeight(r, 6, "Gauss"))
#'     r[sample(1000, 1:ncell(r))] <- NA
#'   
#'   # full raster percent volume 
#'   p30 <- raster.vol(r, p=0.30)
#'   p50 <- raster.vol(r, p=0.50)
#'   p80 <- raster.vol(r, p=0.80)
#'     par(mfrow=c(2,2))
#'     plot(r, col=cm.colors(10), main="original raster")
#'     plot(p30, breaks=c(0,0.1,1), col=c("cyan","red"), legend=FALSE,
#'	      main="30% volume")
#'     plot(p50, breaks=c(0,0.1,1), col=c("cyan","red"), legend=FALSE,
#'	      main="50% volume")
#'     plot(p80, breaks=c(0,0.1,1), col=c("cyan","red"), legend=FALSE,
#'	      main="80% volume")
#'   
#'   # point sample percent volume
#'   #  p30 <- raster.vol(r, p = 0.30, sample = TRUE, spct = 0.20)
#'   #    plot(r, main="30% volume point sample")
#'   #    plot(p30, pch=20, cex=0.70, add=TRUE)
#' }
#'
#' @export    	
raster.vol <- function(x, p = 0.95, sample = FALSE, spct = 0.05) {
  if( sample == FALSE ) {
    den <- raster::getValues(x)
    z <- sort(den[!is.na(den)], decreasing=TRUE)
    y <- cumsum(as.numeric(z))
    i <- sum(y <= p * y[length(y)])
  return(raster::setValues(x, den >= z[i])) 
  } else {
  den <- raster::sampleRegular(x, round(raster::ncell(x) * spct, digits = 0), sp = TRUE) 
    den@data <- data.frame(idx = 1:nrow(den), den = den@data[,1], cls = 0)
	sum.p <- sum(den@data[,"den"], na.rm = TRUE) * p
    den@data <- den@data[order(-den@data$den),] 
    i=0; j=0
      while(i <= sum.p) { 
        j=j+1
        if( !is.na(den@data[,"den"][j])) {
        i = i + den@data[,"den"][j]
        den@data[,"cls"][j] <- 1
        } else {
        den@data[,"cls"][j] <- NA
      }	
    }						   
    den@data <- den@data[order(den@data[,"idx"]),]
      return( den[den$cls == 1 ,] )	
  }
}
