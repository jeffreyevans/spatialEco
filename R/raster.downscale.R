#' @title Raster Downscale
#' @description Downscales a raster to a higher resolution raster using 
#'              a robust regression
#' 
#' @param x          A terra SpatRaster object representing independent variable(s) 
#' @param y          A terra SpatRaster object representing dependent variable
#' @param p          Percent sample size (default NULL)      
#' @param n          Fixed sample size (default NULL)      
#' @param scatter    (FALSE/TRUE) Optional scatter plot   
#' @param ...        Additional arguments passed to predict    
#'
#' @return A list object containing:
#' \itemize{ 
#' \item  downscale downscaled terra SpatRaster object
#' \item  model     rlm model object 
#' \item  MSE       Mean Square Error
#' \item  AIC       Akaike information criterion
#' }
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \dontrun{
#'   library(geodata)
#'   library(terra)
#' 
#'   # Download example data (requires geodata package)
#'     elev <- geodata::elevation_30s(country="SWZ",  path=tempdir())
#' 	  slp <- terrain(elev, v="slope")
#'     tmax <- geodata::worldclim_country(country="SWZ", var="tmax", 
#'                                        path=tempdir())
#'       tmax <- crop(tmax[[1]], ext(elev))
#'   
#'   # Downscale temperature
#'   x=c(elev,slp)
#'     names(x) <- c("elev","slope")
#'   y=tmax
#'     names(y) <- c("tmax")
#'   
#'   tmax.ds <- raster.downscale(x, y, scatter=TRUE)
#'     opar <- par(no.readonly=TRUE)
#'       par(mfrow=c(2,2))
#'         plot(tmax, main="Temp max")
#'         plot(x[[1]], main="elevation")
#' 	    plot(x[[2]], main="slope")
#'         plot(tmax.ds$downscale, main="Downscaled Temp max")
#'     par(opar)
#'    
#' }
#' @export raster.downscale
raster.downscale <- function(x, y, p = NULL, n = NULL, scatter = FALSE, 
                             samp.type=c("random", "regular"), ...) {
	
    if (!inherits(x, "SpatRaster")) 
	  stop(deparse(substitute(x)), " must be a terra SpatRaster object")	
    if (!inherits(y, "SpatRaster")) 
	  stop(deparse(substitute(y)), " must be a terra SpatRaster object")	
    if(is.null(p) & is.null(n)) {
	  message("Population is being used and may cause memory issues")
	    sub.samp <- terra::as.points(y)
	} else {  
	  if(!is.null(n)) { sampSize = n }
	  if(!is.null(p)) { sampSize = as.numeric(round(((terra::nrow(y)*terra::ncol(y))*p),0)) }
	  sub.samp <- terra::spatSample(y, size=sampSize, as.points=TRUE, 
	                                na.rm=TRUE, method=samp.type[1])
	}
	sub.samp <- data.frame(sub.samp, terra::extract(x, sub.samp) )
	  sub.samp <- sub.samp[,-which(names(sub.samp) %in% "ID")]
	    names(sub.samp) <- c("y", names(x))
	      sub.samp <- stats::na.omit(sub.samp)	
	if(length(names(sub.samp)[-1]) > 1){ 
      xnames <- paste(names(sub.samp)[-1], collapse = "+") 
	} else {
	  xnames <- names(sub.samp)[2]
	}
    rrr <- MASS::rlm(stats::as.formula(paste(names(sub.samp)[1], xnames, sep=" ~ ")), 
                     data=sub.samp, scale.est="Huber", psi=MASS::psi.hampel, init="lts")
    if(scatter == TRUE) { graphics::plot(sub.samp[,2], sub.samp[,1], pch=20, cex=0.50,
		                                 xlab=names(sub.samp)[2], ylab="y") }				
  r <- terra::predict(x, rrr, na.rm=TRUE, ...)
  return(list(downscale = r, model = rrr, MSE = round(mean(rrr$residuals), digits=4), 
         AIC = round(stats::AIC(rrr), digits=4)))		
}
