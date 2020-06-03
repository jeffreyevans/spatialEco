#' @title Raster Downscale
#' @description Downscales a raster to a higher resolution raster using 
#'              a robust regression
#' 
#' @param x          Raster class object representing independent variable(s) 
#' @param y          Raster class object representing dependent variable
#' @param p          Percent sample size      
#' @param n          Fixed sample size      
#' @param filename   Name of output raster      
#' @param scatter    (FALSE/TRUE) Optional scatter plot   
#' @param ...        Additional arguments passed to predict    
#'
#' @return A list object containing:
#' \itemize{ 
#' \item  downscale downscaled raster (omitted if filename is defined)
#' \item  model     rlm model object 
#' \item  MSE       Mean Square Error
#' \item  AIC       Akaike information criterion
#' }
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \dontrun{
#'  library(raster)
#'  elev <- raster::getData('alt', country='SWZ', mask=TRUE)
#'  tmax <- raster::getData('worldclim', var='tmax', res=10, 
#'                          lon=8.25, lat=46.8)
#'  tmax <- crop(tmax[[1]], extent(elev))
#'  
#'  # Downscale temperature
#'  tmax.ds <- raster.downscale(elev, tmax, scatter=TRUE)
#'    opar <- par(no.readonly=TRUE)
#'      par(mfrow=c(2,2))
#'      plot(tmax, main="Temp max")
#'      plot(elev, main="elevation")
#'        plot(tmax.ds$downscale, main="Downscaled Temp max")
#'    par(opar)
#' }
#' 
#' @export raster.downscale
raster.downscale <- function(x, y, p = NULL, n = NULL, filename = FALSE, 
                             scatter = FALSE, ...) {
	if(!class(y) == "RasterLayer") stop( "y is not a raster object")
	  if(!class(x) == "RasterLayer" & !class(x) == "RasterStack" & !class(x) == "RasterBrick")
	        stop( "x is not a raster object")  		
	    x <- raster::stack(x)
    if(is.null(p) & is.null(n)) {
	  warning("Population is being used and may cause memory issues")
	    sub.samp <- raster::rasterToPoints(y, spatial=TRUE)
	} else {  
	  if(!is.null(n)) { sampSize = n }
	  if(!is.null(p)) { sampSize = round(((raster::nrow(y)*raster::ncol(y))*p),0) }
	  sub.samp <- raster::sampleRandom(y, sampSize, sp=TRUE)
	}	  
	sub.samp@data <- data.frame(sub.samp@data, raster::extract(x, sub.samp) )
	  names(sub.samp@data) <- c("y", names(x))
	    sub.samp <- stats::na.omit(sub.samp@data)	  
    rrr <- MASS::rlm(stats::as.formula(paste(names(sub.samp)[1], ".", sep=" ~ ")), 
                     data=sub.samp, scale.est="Huber", psi=MASS::psi.hampel, init="lts")
    if(scatter == TRUE) { graphics::plot(sub.samp[,2], sub.samp[,1], pch=20, cex=0.50,
		                                 xlab=names(sub.samp)[2], ylab="y") }				
    if (filename != FALSE) {
	  raster::predict(x, rrr, filename=filename, na.rm=TRUE, progress='window', 
	                  overwrite=TRUE, ...)
      message(paste("Raster written to", filename, sep=": "))	
        return(list(model = rrr, MRE = round(mean(rrr$residuals), digits=4), 
               AIC = round(stats::AIC(rrr), digits=4)))			  
	} else {
	  r <- raster::predict(x, rrr, na.rm=TRUE, progress='window')
	    return(list(downscale = r, model = rrr, MSE = round(mean(rrr$residuals), digits=4), 
               AIC = round(stats::AIC(rrr), digits=4)))		
	}
}
