#' @title Raster local deviation from the global trend 
#' @description Calculates the local deviation from the raster, a specified global statistic or a polynomial trend of the raster.  
#' 
#' @param x         raster object
#' @param type      The global statistic to represent the local deviation  options are: "trend", "min", "max", "mean", "median"
#' @param degree    The polynomial degree if type is trend, options are 1 and 2. 
#' @param s         Size of matrix (focal window), not used with type="trend"
#' @param global    Use single global value for deviation or cell-level values (FALSE/TRUE). Argument is ignored for type="trend"
#' 
#' @return raster class object of the local deviation from the raster or specified global statistic
#'
#' @note
#'  The deviation from the trend is derived as [y-hat - y] where; y-hat is the Nth-order polynomial.  Whereas the deviation from a global statistic is [y - y-hat] where; y-hat is the local (focal) statistic. 
#'  The global = TRUE argument allows one to evalute the local deviation from the global statistic [stat(x) - y-hat] where; stat(x) is the global value of the specified staistic and y-hat is the specified focal statistic.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#'  Magee, Lonnie (1998). Nonlocal Behavior in Polynomial Regressions. The American Statistician. American Statistical Association. 52(1):20-22
#'  Fan, J. (1996). Local Polynomial Modelling and Its Applications: From linear regression to nonlinear regression. Monographs on Statistics and Applied Probability. Chapman and Hall/CRC. ISBN 0-412-98321-4
#' 
#' @examples 
#'   library(raster)
#'   data(elev)
#' 
#' # local deviation from first-order trend, global mean and raw value
#' r.dev.trend <- raster.deviation(elev, type="trend", degree=1) 
#' r.dev.mean <- raster.deviation(elev, type="mean", s=5)
#' r.gdev.mean <- raster.deviation(elev, type="mean", s=5, global=TRUE)
#' 
#' par(mfrow=c(2,2))
#'   plot(elev, main="original")
#'   plot(r.dev.trend, main="dev from trend")
#'   plot(r.dev.mean, main="dev of mean from raw values")
#'   plot(r.gdev.mean, main="local dev from global mean")
#'
#' @export
raster.deviation <- function(x, type = "trend", s = 3, degree = 1, global = FALSE) {  
  if (!inherits(x, "RasterLayer")) stop("MUST BE RasterLayer OBJECT")  
  if( type != "trend") {
     if( length(s) == 1) s = c(s[1],s[1])
       m <- matrix(1, nrow=s, ncol=s)
	   if( global == FALSE) {   
	     return( x - raster::focal(x, w = m, fun =  match.fun(type), na.rm = TRUE) )
	   } else {
	   return( raster::cellStats(x, stat = type, na.rm = TRUE) - raster::focal(x, w = m, fun = match.fun(type), na.rm = TRUE) )
	  }  
    }	
  if( type == "trend" ) {
    r <- methods::as(x, "SpatialGridDataFrame")
      names(r)[1] <- "y"
      r@data$XCOORD <- sp::coordinates(r)[,1]
      r@data$YCOORD <- sp::coordinates(r)[,2]    
      # 1st and 2nd order polynomial equations
      if( degree == 1 ) {   
        p.1 <- stats::as.formula(y ~ XCOORD + YCOORD) 
          poly.mdl <- stats::lm(p.1, data=r@data)
      } else {	
        f.2 <- stats::as.formula(y ~ XCOORD + YCOORD + I(XCOORD*XCOORD)+I(YCOORD*YCOORD) + I(XCOORD*YCOORD))
          poly.mdl <- stats::lm( f.2, data=r@data)
      }
      r@data <- data.frame( r@data, trend = stats::predict(poly.mdl, newdata=r@data))  
        return( raster::raster(r, layer=4, values=TRUE) - x ) 
  } 
}  
