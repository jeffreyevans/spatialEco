#' @title Alpha Convex Hull
#'
#' @description Calculates a convex hull on a point feature class using the Pateiro-Lopez (2009) Alphahull model 
#' 
#' @param x         SpatialPoints or SpatialPointsDataFrame object
#' @param alpha     Alpha parameter for adjusting boundary tension
#'
#' @return SpatialPolygons object  
#'
#' @note This method provides flexibility over traditional convex functions in that the the alpha parameter 
#'       can be adjusted to relax or increase tension between boundary-edge points
#'       Due to licensing constraints associated with the alphahull package, this function is not available 
#'       in the CRAN release. The function must be called from the package NAMESPACE using: 
#'       spatialEco:::convexHull  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references Pateiro-Lopez & Rodriguez-Casal (2009) Generalizing the Convex Hull of a Sample: The R Package alphahull. Journal of Statistical Software 34(5):1-28 http://www.jstatsoft.org/v34/i05/paper
#'                                                                    
#' @examples 
#'  library(sp)
#'  data(meuse)
#'   coordinates(meuse) = ~x+y
#'  a <- convexHull(meuse, alpha=100000)
#'    plot(a)
#'      points(meuse, pch=19)
#'  
#'  # Convert SpatialLinesDataFrame to SpatialPolygonsDataFrame
#'  library(sf)
#'  a <- sf::st_as_sf(a) 
#'  a <- sf::st_polygonize(a)
#'  class( a <- as(a, "Spatial") )
#'    plot(a)
#'  
#'  # Test multiple alpha values
#'   par(mfcol=c(2,2))
#'     for (a in c(500, 1500, 5000, 100000)) {
#'     ch <- convexHull(meuse, alpha = a)
#'       plot(ch)
#'        points(meuse, pch=19)
#'          title( paste0("alpha=", a))		 
#'     }
#'
#' @export convexHull
convexHull <- function(x, alpha = 250000)	{
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
  if (!inherits(x, "SpatialPointsDataFrame") |  !inherits(x, "SpatialPoints") ) 
      stop(deparse(substitute(x)), " x must be a sp Points object")
    a <- alphahull::ashape(sp::coordinates(x), alpha = alpha)
    l <- list()
      for (i in 1:nrow(a$edges)) { l[[i]] <-  sp::Line(rbind(a$edges[i, 3:4], a$edges[i, 5:6])) }
        a <- sp::SpatialLinesDataFrame(sp::SpatialLines(list(sp::Lines(l, as.character("1")))),
                                       data.frame(name ="ashape"), match.ID = FALSE)				 
  return( a )	
}
