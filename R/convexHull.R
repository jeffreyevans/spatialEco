#' @title Alpha Convex Hull
#'
#' @description 
#' Calculates a convex hull on a point feature class using the Pateiro-Lopez (2009) 
#' Alphahull model 
#' 
#' @param x         sf, sfc, SpatialPoints, SpatialPointsDataFrame or matrix object 
#'                    representing [x,y] coordinates
#' @param alpha     Alpha parameter for adjusting boundary tension
#'
#' @return sf POLYGIN object  
#'
#' @note 
#' This method provides flexibility over traditional convex functions in that the the alpha 
#' parameter can be adjusted to relax or increase tension between boundary-edge points
#' Due to licensing constraints associated with the alphahull package, this function is not 
#' available in the CRAN release. The function must be called from the package NAMESPACE 
#' using:  spatialEco:::convexHull. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references 
#' Pateiro-Lopez & Rodriguez-Casal (2009) Generalizing the Convex Hull of a Sample: 
#'   The R Package alphahull. Journal of Statistical Software 34(5):1-28 
#'                                                                    
#' @examples 
#'  library(sf)
#'  
#'  #### points example
#' if(require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' }
#'   
#'   a <- convexHull(meuse, alpha=100000)
#'     plot(a)
#'       plot(st_geometry(meuse), pch=19, add=TRUE)
#' 
#'   # Test multiple alpha values
#'    par(mfcol=c(2,2))
#'      for (a in c(500, 1500, 5000, 100000)) {
#'      ch <- convexHull(meuse, alpha = a)
#'        plot(ch, key.pos = NULL, reset = FALSE)
#'         plot(st_geometry(meuse), pch=19, add=TRUE)
#'           title( paste0("alpha=", a))		 
#'      }
#' 
#'  \dontrun{
#'   #### Polygon example (note, I draw a sample to make more tractable)
#'   data(meuse, package = "sp")
#'     meuse <- st_as_sf(meuse, coords = c("x", "y"), 
#'                       crs = 28992, agr = "constant")
#'        meuse_poly <- st_buffer(meuse[sample(1:nrow(meuse),10),], 
#' 	                           dist = meuse$elev*15)
#'  
#'   # Create [x,y] points representing polygon boundaries 
#'   poly_points <- sf::st_segmentize(meuse_poly, dfMaxLength = 5) |>
#'     sf::st_coordinates() |> 
#'       as.data.frame() |> 
#'         subset(select = c(X, Y)) |> 
#'           sf::st_as_sf(coords = c("X", "Y")) 
#' 
#'   # Draw random sample to make tractable
#'   poly_points <- poly_points[sample(1:nrow(poly_points), 500),]
#' 
#'   # calculate convex hull		  
#'   a <- convexHull(poly_points, alpha = 10000)
#'     plot(sf::st_geometry(a), cex=1.5, col="red") 
#'       plot(sf::st_geometry(poly_points), col="black", add=TRUE)
#' }
#'
#' @export convexHull
convexHull <- function(x, alpha = 250000)	{
  if(!any(which(utils::installed.packages()[,1] %in% "alphahull")))
     stop("please install alphahull package before running this function")
  if(!inherits(x, c("SpatialPointsDataFrame", "SpatialPoints", "sf", "sfc", "matrix")))		
    stop(deparse(substitute(x)), " must be a spatial (sf, sp) or matrix object")
  if(inherits(x, c("sf", "sfc"))) { 
     xy <- as.data.frame(sf::st_coordinates(x)) 
  } else if(inherits(x, c("SpatialPointsDataFrame", "SpatialPoints"))) {
     xy <- as.data.frame(sp::coordinates(x))
  } else if(inherits(x, "matrix")) {
     xy <- as.data.frame(x)
  } else {
     stop("Not a valid object")
  }
  xy <- xy[!duplicated(xy[c(1,2)]),]
    a <- alphahull::ashape(as.matrix(xy), alpha = alpha)$edges[,3:6]
      a <- apply(a, 1, function(x)  {
        v <- as.numeric(x[c(1,3,2,4)])
          m <- matrix(v, nrow = 2)
        return(sf::st_sfc(sf::st_linestring(m)))
      })
	  a <- Reduce(c, a) |>
        sf::st_combine() |>		
		  sf::st_as_sf() |>
		    sf::st_polygonize() |>	
              sf::st_collection_extract(type = "POLYGON")
			    st_geometry(a) <- "geometry"
			      a$ID <- 1
	if(!is.na(st_crs(x))) {
	  sf::st_crs(a) <- sf::st_crs(x) 
	}	  
  return( a )	
}
