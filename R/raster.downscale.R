#' @title Raster Downscale
#' @description Downscales a raster to a higher resolution raster using 
#'              a robust regression
#' 
#' @param x            A terra SpatRaster object representing independent variable(s) 
#' @param y            A terra SpatRaster object representing dependent variable
#' @param full.res     (FALSE/TRUE) Use full resolution of x (see notes)  
#' @param scatter      (FALSE/TRUE) Optional scatter plot  
#' @param residuals    (FALSE/TRUE) Output raster residual error raster, 
#'                     at same resolution as y 
#' @param se           (FALSE/TRUE) Output standard error raster, using prediction or 
#'                     confidence interval  
#' @param p            The confidence/prediction interval (default is 95%)  
#' @param uncertainty  Output uncertainty raster(s) of confidence or prediction interval, 
#'                     at same resolution as y. Options are c("none", "prediction", "confidence")  
#'
#' @return A list object containing:  
#' * downscale      downscaled terra SpatRaster object
#' * model          MASS rlm model object 
#' * MSE            Mean Square Error
#' * AIC            Akaike information criterion
#' * parm.ci        Parameter confidence intervals
#' * residuals      If residuals = TRUE, a SpatRaster of the residual error
#' * uncertainty    If pred.int = TRUE, SpatRaster's of the 
#'                  lower/upper prediction intervals
#' * std.error      If se = TRUE, SpatRaster's of the standard error 
#' @md
#'
#' @note
#' This function uses a robust regression, fit using an M-estimation with Tukey's biweight 
#' initialized by a specific S-estimator, to downscale a raster based on higher-resolution
#' or more detailed raster data specified as covariate(s). You can optionally output residual 
#' error, standard error and/or uncertainty rasters. However, please note that when choosing 
#' the type of uncertainty, using a confidence interval (uncertainty around the mean predictions) 
#' when you should be using the prediction interval (uncertainty around a single values) will 
#' greatly underestimate the uncertainty in a given predicted value (Bruce & Bruce 2017). 
#' The full.res = TRUE option uses the x data to sample y rather than y to sample x. THis makes 
#' the problem much more computationally and memory extensive and should be used with caution. 
#' There is also the question of pseudo-replication (sample redundancy) in the dependent variable.
#' Statistically speaking one would expect to capture the sample variation of x by sampling at the
#' frequency of y thus supporting the downscaling estimate. Note that if uncertainty is not defined
#' the prediction interval for standard error defaults to "confidence" else is the same output as
#' uncertainty (eg., prediction or confidence).    
#'
#' @references
#' Bruce, P., & A. Bruce. (2017). Practical Statistics for Data Scientists. O’Reilly Media.
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \dontrun{
#' library(geodata)
#' library(terra)
#' 
#' # Download example data (requires geodata package)
#'   elev <- geodata::elevation_30s(country="SWZ",  path=tempdir())
#'  slp <- terrain(elev, v="slope")
#'   tmax <- geodata::worldclim_country(country="SWZ", var="tmax", 
#'                                      path=tempdir())
#'     tmax <- crop(tmax[[1]], ext(elev))
#' 
#' # Downscale temperature
#' x=c(elev,slp)
#'   names(x) <- c("elev","slope")
#' y=tmax
#'   names(y) <- c("tmax")
#' 
#' tmax.ds <- raster.downscale(x, y, scatter=TRUE, residuals = TRUE,
#'                             uncertainty = "prediction", se = TRUE)
#' 	
#'   # plot prediction and parameters	
#'   opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(2,2))
#'       plot(tmax, main="Temp max")
#'       plot(x[[1]], main="elevation")
#'       plot(x[[2]], main="slope")
#'       plot(tmax.ds$downscale, main="Downscaled Temp max")
#'   par(opar)
#' 
#'   # Plot residual error and raw prediction +/- intervals
#'   opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(2,2))
#'       plot(tmax.ds$std.error, main="Standard Error")
#'       plot(tmax.ds$residuals, main="residuals")
#'       plot(tmax.ds$uncertainty[[1]], 
#' 	       main="lower prediction interval")
#'       plot(tmax.ds$uncertainty[[2]], 
#' 	       main="upper prediction interval")
#'   par(opar)
#'   
#'   # plot prediction uncertainty
#'   opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(2,1))
#'       plot(tmax.ds$downscale - tmax.ds$uncertainty[[1]], 
#' 	       main="lower prediction interval")
#'       plot(tmax.ds$downscale - tmax.ds$uncertainty[[2]], 
#' 	       main="upper prediction interval")  
#'   par(opar)  
#'  
#' }
#' @export raster.downscale
raster.downscale <- function(x, y, scatter = FALSE, full.res = FALSE, 
                             residuals = FALSE, se = FALSE, p = 0.95,  
							 uncertainty = c("none", "prediction", "confidence")) {
	uncertainty = uncertainty[1]
    if (!inherits(x, "SpatRaster")) 
	  stop(deparse(substitute(x)), " must be a terra SpatRaster object")	
    if (!inherits(y, "SpatRaster")) 
	  stop(deparse(substitute(y)), " must be a terra SpatRaster object")
    if (terra::nlyr(y) > 1)  {
      warning("Dependent variable must be a single response, 
	    only the first raster will be used")	
          y <- y[[1]]
    }	  
	if(full.res == FALSE) {
	  sub.samp.sp <- terra::as.points(y, na.rm=TRUE)
	    sub.samp <- data.frame(sub.samp.sp, terra::extract(x, sub.samp.sp) )
	      sub.samp <- sub.samp[,-which(names(sub.samp) %in% "ID")]
	        names(sub.samp) <- c("y", names(x))
			  na.idx <- unique(which(is.na(sub.samp), arr.ind = TRUE)[,1])
			  if(length(na.idx) > 0)
	            sub.samp <- sub.samp[-na.idx,]
	} else {
	  message("Population is being used and may cause memory issues")
	  sub.samp.sp <- terra::as.points(x, na.rm=TRUE)
	    sub.samp <- data.frame(terra::extract(y, sub.samp.sp), sub.samp.sp)
	      sub.samp <- sub.samp[,-which(names(sub.samp) %in% "ID")]
	        names(sub.samp) <- c("y", names(x))
			  na.idx <- unique(which(is.na(sub.samp), arr.ind = TRUE)[,1])
			  if(length(na.idx) > 0)
	            sub.samp <- sub.samp[-na.idx,]
    }		
	if(length(names(sub.samp)[-1]) > 1){ 
      xnames <- paste(names(sub.samp)[-1], collapse = "+") 
	} else {
	  xnames <- names(sub.samp)[2]
	}
    rrr <- MASS::rlm(stats::as.formula(paste(names(sub.samp)[1], xnames, sep=" ~ ")), 
                     data=sub.samp, scale.est="Huber", psi=MASS::psi.hampel, init="lts")
    if(scatter == TRUE) {
      n = terra::nlyr(x)
	  if(n > 1) {
	    graphics::par(mfrow=c(n,n/2))   
	    for(i in 2:(n+1)) {
	      graphics::plot(sub.samp[,i], sub.samp[,1], pch=20, cex=0.50,
		                 col="grey", xlab=names(sub.samp)[i], ylab="y") 
		  graphics::abline(stats::coefficients(rrr)[c(1,i)], col="red") 
		}					 
	  } else {
	    graphics::plot(sub.samp[,2], sub.samp[,1], pch=20, cex=0.50,
		               col="grey", xlab=names(sub.samp)[2], ylab="y") 
		  graphics::abline(stats::coefficients(rrr)[c(1,2)], col="red") 	  
	  }
	}  
     r <- terra::predict(x, rrr, na.rm=TRUE)
       results <- list(downscale = r, model = rrr, 
                    MSE = round(mean(rrr$residuals), digits=4), 
                    AIC = round(stats::AIC(rrr), digits=4),
                    parm.ci=stats::confint.default(object = rrr, level = 0.95))
  if(residuals == TRUE) {
    sub.samp.sp <- sub.samp.sp[-na.idx,]
    sub.samp.sp$resd <- rrr$residuals 
      if(full.res) {
	    results$residuals <- terra::rasterize(sub.samp.sp, x[[1]], field="resd")
	  } else {
	    results$residuals <- terra::rasterize(sub.samp.sp, y, field="resd")
	  }
  }
  if(uncertainty != "none"){
    if(!uncertainty %in% c("prediction", "confidence")) {
     warning(paste0(uncertainty, " is not a valid option, uncertainty will not be calculated"))
	} else {	
	  pred.uncert <- function(model, data) {
        v <- suppressWarnings(stats::predict(model, data, se.fit=FALSE,  
                              interval=uncertainty,level=p))
	      cbind(lci=as.vector(v[,"lwr"]), uci=as.vector(v[,"upr"]) )
      }
    results$uncertainty <- terra::predict(x, rrr, fun=pred.uncert) 
    }	
  }
  if(se == TRUE) {
    if(uncertainty == "none")
	  uncertainty = "confidence"
	  message("Calculating standard error using ", uncertainty, " interval")
    pred.se <- function(model, data) {
      suppressWarnings(stats::predict(model, data, se.fit=TRUE,  
                      interval=uncertainty,level=p)$se.fit)
    }
  results$std.error <- terra::predict(x, rrr, fun=pred.se) 
  }    
  return( results )		
}

