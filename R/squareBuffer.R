#' @title Square buffer
#' @description Creates a square buffer of a feature class
#' 
#' @param x    An sf object
#' @param a    Numeric single or vector indicating buffer distance(s)
#' @param ...  Additional arguments passed to st_buffer
#'
#' @details  Function creates a square buffer of feature class.
#'
#' @return A single feature sf class polygon object
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(sf)
#' xy <- st_as_sf(data.frame(x = c(1,3,6,7),
#'                y = c(3,2,7,8), z = c(38,23,12,12),
#'                area = c(32,23,45,67)),
#' 			      coords = c("x", "y"), 
#' 			      agr = "constant") 
#' 
#' # With fixed buffer
#' sb <- squareBuffer(xy, 32)
#'   plot(st_geometry(sb))
#'     plot(st_geometry(xy), pch=20, add=TRUE)
#'   
#' # With variable buffer
#' sb.var <- squareBuffer(xy, xy$area)
#'   plot(st_geometry(sb.var))
#'     plot(st_geometry(xy), pch=20, add=TRUE)
#'   
#' @export				 					   
squareBuffer <- function(x, a, ...) {
  if(!inherits(x, c("sf", "sfc")))		
    stop(deparse(substitute(x)), " must be an sf class object")	
  if(missing(a))
    stop("buffer distance argument is missing")  
  a <- sqrt(a)/2
    dots <- as.list(match.call(expand.dots = TRUE)[-1])
	if(any(names(dots) == "a"))
	  dots <- dots[-which(names(dots) %in% "a")]
      dots$x <- x
      dots$dist <- a
      dots$nQuadSegs <- 1
      dots$endCapStyle <- "SQUARE"   
  return(do.call(sf::st_buffer, dots)) 
}
