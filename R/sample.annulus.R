#' @title Sample annulus
#' @description Creates sample points based on annulus with defined 
#'              inner and outer radius
#' 
#' @param x      An sf POINT class object
#' @param r1     Numeric value defining inner radius of annulus 
#'               (in projection units)
#' @param r2     Numeric value defining outer radius of annulus 
#'               (in projection units)
#' @param size   Number of samples
#' @param ...    Additional arguments passed to sf::st_sample
#'
#' @return sp SpatialPointsataFrame OBJECT
#'
#' @note 
#' Function can be used for distance based sampling. This is a sampling method 
#' that can be used to capture spatially lagged variation.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#'  library(sf)
#'  if(require(sp, quietly = TRUE)) {
#'    data(meuse, package = "sp")
#'    meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                      agr = "constant")
#'  }
#' 
#'  xy <- meuse[2,]
#'  rs100 <- sample.annulus(xy, r1=50, r2=100, size = 50)
#'  rs200 <- sample.annulus(xy, r1=100, r2=200, size = 50)
#'  
#'  plot(st_geometry(rs200), pch=20, col="red")
#'    plot(st_geometry(rs100), pch=20, col="blue", add=TRUE)
#'    plot(st_geometry(xy), pch=20, cex=2, col="black", add=TRUE)
#'  legend("topright", legend=c("50-100m", "100-200m", "source"), 
#'         pch=c(20,20,20), col=c("blue","red","black"))
#' 
#'  \dontrun{
#' # Run on multiple points
#' rs100 <- sample.annulus(meuse[1:3,], r1=50, r2=100, 
#'                         size = 50)
#' rs200 <- sample.annulus(meuse[1:3,], r1=50, r2=200, 
#'                         size = 50)
#' plot(st_geometry(rs200), pch=20, col="red")
#'   plot(st_geometry(rs100), pch=20, col="blue", add=TRUE)
#'     plot(st_geometry(meuse[1:3,]), pch=20, cex=2, col="black", add=TRUE)
#'  legend("topright", legend=c("50-100m", "100-200m", "source"), 
#'         pch=c(20,20,20), col=c("blue","red","black"))
#' }

#' @export sample.annulus
sample.annulus <- function(x, r1, r2, size = 10, ...) {
  if(!inherits(x, c("sf", "sfc")))		
    stop(deparse(substitute(x)), " must be an sf or sfc object")
 if(!unique(as.character(st_geometry_type(x))) %in% c("POINT", "MULTIPOINT"))
    stop(deparse(substitute(x)), " must be an sf POLYGON object")		
  if(unique(as.character(st_geometry_type(x))) %in% "MULTIPOINT")
    stop("Function does not support multi-part MULTIPOINT objects")		
  if(r1 >= r2) stop("inner radius (r1) must be smaller than outer (r2)")
    dots <- as.list(match.call(expand.dots = TRUE)[-1])
  if (is.null(dots[["type"]]) & "type" %in% names(dots) == FALSE) dots[["type"]] <-  "random"
    if (is.null(dots[["size"]]) & "size" %in% names(dots) == FALSE) dots[["size"]] <- 10
  c1 <- sf::st_buffer(x, dist = r1)
  c2 <- sf::st_buffer(x, dist = r2)
  annulus <- sf::st_difference(c2, c1)
	dots[["x"]] <- annulus
	s <- sf::st_as_sf(do.call(sf::st_sample, dots))
	  sf::st_geometry(s) <- "geometry"
	  s$SID = 1:length(s)
	  s$PID = rownames(x[1,])
  return( s )
}
