#' @title Average Nearest Neighbor Index (NNI)
#' @description Calculates the NNI as a measure of clustering or dispersal 
#'
#' @param x An sf point object
#' @param win Type of window 'hull' or 'extent'
#'
#' @details
#' The nearest neighbor index is expressed as the ratio of the observed distance 
#' divided by the expected distance. The expected distance is the average distance 
#' between neighbors in a hypothetical random distribution. If the index is less than 1, 
#' the pattern exhibits clustering; if the index is greater than 1, the trend is toward 
#' dispersion or competition. The Nearest Neighbor Index is calculated as:
#'   * Mean Nearest Neighbor Distance (observed) D(nn) = sum(min(Dij)/N) 
#'   * Mean Random Distance (expected) D(e) = 0.5 SQRT(A/N)
#'   * Nearest Neighbor Index NNI = D(nn)/D(e)
#'       Where; D=neighbor distance, A=Area
#' @md
#'
#' @return 
#' list object containing NNI = nearest neighbor index, z.score = Z 
#' Score value, p = p value, expected.mean.distance = Expected mean 
#' distance, observed.mean.distance = Observed meand distance.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Clark, P.J., and F.C. Evans (1954) Distance to nearest neighbour as a measure 
#'   of spatial relationships in populations. Ecology 35:445-453
#'  
#' Cressie, N (1991) Statistics for spatial data. Wiley & Sons, New York.
#'
#' @examples 
#'
#' p = c("sf", "sp")
#'   if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
#'     m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
#'     message("Can't run examples, please install ", paste(p[m], collapse = " "))
#'   } else {
#'   invisible(lapply(p, require, character.only=TRUE))
#' 
#'   data(meuse, package = "sp")
#'   meuse <- sf::st_as_sf(meuse, coords = c("x", "y"),  
#'                         crs = 28992, agr = "constant")
#'   nni(meuse)
#'   }
#'
#' @export
nni <- function(x, win = c("hull", "extent")) {
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf POINT object")	
  if(unique(as.character(sf::st_geometry_type(x))) != "POINT")
    stop(deparse(substitute(x)), " must be an sf POINT object")		
    if (win[1] == "hull") {
      w <- spatstat.geom::convexhull.xy( sf::st_coordinates(x)[,1:2] )
    }
    if (win[1] == "extent") {
      e <- as.vector(sf::st_bbox(x))
      w <- spatstat.geom::as.owin(c(e[1], e[3], e[2], e[4]))
    }
    x <- spatstat.geom::as.ppp(sf::st_coordinates(x)[,1:2], w)
    A <- spatstat.geom::area.owin(w)
      obsMeanDist <- sum(spatstat.geom::nndist(x))/x$n
      expMeanDist <- 0.5 * sqrt(A / x$n)
      se <- 0.26136 / ((x$n**2.0 / A)**0.5)
      nni <- obsMeanDist / expMeanDist
      z <- (obsMeanDist - expMeanDist) / se
    return(list(NNI = nni, z.score = z, p = 2*stats::pnorm(-abs(z)),  
	       expected.mean.distance = expMeanDist,
		   observed.mean.distance = obsMeanDist))
}
