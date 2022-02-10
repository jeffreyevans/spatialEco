#' @title Background sample
#'
#' @description Creates a point sample that can be used as 
#'              a NULL for SDM's and other modeling approaches. 
#'
#' @param x      A polygon defining sample region    
#' @param ext    Vector of extent coordinates (xmin, xmax, ymin, ymax) 
#' @param p      Size of sample
#' @param known  SpatialPoints of known locations (same CSR as x)
#' @param d      Threshold distance for known proximity 
#' @param type   Type of sample c("systematic", "random", "hexagon", "nonaligned")
#'
#' @return A SpatialPointsDataFrame or data.frame with x,y coordinates
#'
#' @note 
#' This function creates a background point sample based on an extent 
#' or polygon sampling region. The known argument can be used with d 
#' to remove sample points based on distance-based proximity to existing  
#' locations (eg., known species locations). The size (p) of the resulting 
#' sample will be dependent on the known locations and the influence of 
#' the distance threshold (d). As such, if the know and d arguments are
#' provided the exact value provided in p will not be returned. 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(rgeos)
#'   data(meuse)
#'   coordinates(meuse) <- ~x+y
#' 
#' # create "known" locations  
#' locs <- meuse[sample(1:nrow(meuse), 5),]
#' 
#' # systematic sample using extent polygon
#' e <- as(extent(meuse), "SpatialPolygons")
#' s <- background(e, p=1000, known=locs, d=300)
#'   plot(s,pch=20)
#'     points(locs, pch=20, col="red")
#' 
#' # systematic sample using irregular polygon
#' data(meuse.grid)
#'   coordinates(meuse.grid) = c("x", "y")
#'   gridded(meuse.grid) = TRUE
#' meuse.poly = gUnaryUnion(as(meuse.grid, "SpatialPolygons"))
#' 
#' s <- background(meuse.poly, p=1000, known=locs, d=200)
#'   plot(s,pch=20)
#'     plot(meuse.poly, add=TRUE)
#'     points(locs, pch=20, col="red")
#' 
#' # random sample using irregular polygon
#' s <- background(meuse.poly, p=500, known=locs, 
#'                 d=200, type="random")
#'   plot(s,pch=20)
#'     plot(meuse.poly, add=TRUE)
#'     points(locs, pch=20, col="red")
#' 
#' # systematic sample using defined extent
#' extent(meuse)
#' s <- background(ext=c(178605, 181390, 329714, 333611), 
#'                 p=1000, known=locs, d=300)
#'   plot(s,pch=20)
#'     points(locs, pch=20, col="red")
#' 
#' @export
background <- function(x, ext=NULL, p=1000, known=NULL, d=NULL, 
                type=c("regular", "random", "hexagon", "nonaligned")) {
  if(missing(x) & is.null(ext))
    stop("extent argument (x or ext) must be defined")	
  if(!missing(x)){
    if(class(x)[1] == "sf") x <- as(x, "sf")
    if(!any(class(x)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame")))
      stop("known must be sp class polygons object")
  }  
  if(!is.null(ext)){
     if(length(ext) != 4)
	   stop("4 coordinates needed for extent") 
    x <- as(raster::extent(ext), "SpatialPolygons")	   
  }    
  if(!is.null(known)){
    if(!any(class(known)[1] == c("SpatialPoints", "SpatialPointsDataFrame")))
      stop("known must be sp class points object")
    if(is.null(d)) 
      stop("distance (d) must be defined")  
  }
  s <- sp::spsample(x=x, n=p, type=type[1], iter=10)
  if(!is.null(known)) {
    rm.buff <- rgeos::gBuffer(known, byid = FALSE, width = d)
	  idx <- which(rgeos::gIntersects(s, rm.buff, byid = TRUE))
    s <- s[-idx,] 
  }
    slot(s, "proj4string") <- sp::CRS(s) 
  return(s)
}
