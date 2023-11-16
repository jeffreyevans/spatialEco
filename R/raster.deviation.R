#' @title Raster local deviation from the global trend 
#' @description 
#' Calculates the local deviation from the raster, a specified global statistic 
#' or a polynomial trend of the raster.  
#' 
#' @param x         A terra SpatRaster object
#' @param type      The global statistic to represent the local deviation  
#'                  options are: "trend", "min", "max", "mean", "median"
#' @param degree    The polynomial degree if type is trend, default is 1st order. 
#' @param s         Size of matrix (focal window), not used with type="trend"
#' @param global    Use single global value for deviation or cell-level values 
#'                 (FALSE/TRUE). Argument is ignored for type="trend"
#' 
#' @details
#' The deviation from the trend is derived as [y-hat - y] where; y-hat is the 
#' Nth-order polynomial. Whereas the deviation from a global statistic is [y - y-hat] 
#' where; y-hat is the local (focal) statistic. The global = TRUE argument allows 
#' one to evaluate the local deviation from the global statistic [stat(x) - y-hat] 
#' where; stat(x) is the global value of the specified statistic and y-hat is the 
#' specified focal statistic.  
#'
#' @return 
#' A SpatRaster class object representing local deviation from the raster or the 
#' specified global statistic
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#'  Magee, Lonnie (1998). Nonlocal Behavior in Polynomial Regressions. The American 
#'   Statistician. American Statistical Association. 52(1):20-22
#' 
#'  Fan, J. (1996). Local Polynomial Modelling and Its Applications: From linear  
#'    regression to nonlinear regression. Monographs on Statistics and Applied 
#'    Probability. Chapman and  Hall/CRC. ISBN 0-412-98321-4
#' 
#' @examples 
#' library(terra)
#' elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#' 
#' # local deviation from first-order trend, global mean and raw value
#' r.dev.trend <- raster.deviation(elev, type="trend", degree=1) 
#' r.dev.mean <- raster.deviation(elev, type="mean", s=5)
#' r.gdev.mean <- raster.deviation(elev, type="mean", s=5, global=TRUE)
#' 
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(2,2))
#'   plot(elev, main="original")
#'   plot(r.dev.trend, main="dev from trend")
#'   plot(r.dev.mean, main="dev of mean from raw values")
#'   plot(r.gdev.mean, main="local dev from global mean")
#' par(opar)
#'
#' @export raster.deviation
raster.deviation <- function(x, type = c("trend", "min", "max", "mean", "median"), 
                             s = 3, degree = 1, global = FALSE) {  
  if (!inherits(x, "SpatRaster")) 
	stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if( type[1] != "trend") {
     if( length(s) == 1) s = c(s[1],s[1])
       m <- matrix(1, nrow=s, ncol=s)
	   if( global == FALSE) {   
	     return( x - terra::focal(x, w = m, fun =   match.fun(type[1]), 
		                          na.rm = TRUE) )
	   } else {
	     return( terra::global(x, eval(parse(text=type[1])), na.rm = TRUE)[,1] - 
	             terra::focal(x, w = m, fun = match.fun(type[1]), 
			                  na.rm = TRUE) )
	  }  
    }	
  if( type[1] == "trend" ) {
    r <- data.frame(y=stats::na.omit(as.vector(x)),
	                terra::crds(x, df=TRUE, 
					na.rm=TRUE))
      names(r) <- c("y", "xcoord", "ycoord")	
     # 1st and 2nd order polynomial equations
      if( degree == 1 ) {   
        p.1 <- stats::as.formula(y ~ xcoord + ycoord) 
          poly.mdl <- stats::lm(p.1, data=r)
      } else if(degree > 1 ) {	
        #f.2 <- stats::as.formula(y ~ xcoord+ycoord+I(xcoord*xcoord)+I(ycoord*ycoord)+I(xcoord*ycoord))
        #poly.mdl <- stats::lm(f.2, data=r)
	    poly.mdl <- stats::lm(y ~ stats::poly(xcoord, degree) + 
		                      poly(ycoord, degree), data=r)
	  }	
      r <- data.frame(r, trend = stats::predict(poly.mdl, newdata=r)) 
      rsf <- sf::st_as_sf(r, coords = c("xcoord", "ycoord"), 
	                      agr = "constant")
      rstat <- terra::rasterize(terra::vect(rsf), x, field="trend")
      message("polynomial confidence intervals")	  
	    stats::confint(poly.mdl, level=0.95)
     return( rstat - x ) 
  } 
}  
