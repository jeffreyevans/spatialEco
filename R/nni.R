#' @title Average Nearest Neighbour Index (NNI)
#' @description Calculates the NNI as a measure of clustering or dispersal 
#'
#' @param x An sp point object
#' @param win Type of window 'hull' or 'extent'
#'
#' @return list object contanint NNI = nearest neighbor index, z.score = Z Score value, p = p value, expected.mean.distance = Expected meand distance, observed.mean.distance = Observed meand distance.
#'
#' @note
#' The nearest neighbour index is expressed as the ratio of the observed distance divided by the expected distance. The expected distance is the average distance between neighbours in a hypothetical random distribution. If the index is less than 1, the pattern exhibits clustering; if the index is greater than 1, the trend is toward dispersion or competition.
#' The Nearest Neighbour Index is calculated as:
#'   Mean Nearest Neighbour Distance (observed) D(nn) = sum(min(Dij)/N) 
#'   Mean Random Distance (expected) D(e) = 0.5 SQRT(A/N)
#'   Nearest Neighbour Index NNI = D(nn)/D(e)
#'       Where; D=neighbour distance, A=Area
#'
#' @note Depends: sp, spatstat
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Clark, P.J., and F.C. Evans (1954) Distance to nearest neighbour as a measure of spatial relationships in populations. Ecology 35:445-453
#' @references Cressie, N (1991) Statistics for spatial data. Wiley & Sons, New York.
#'
#' @examples 
#' require(sp)
#' data(meuse)
#'   coordinates(meuse) <- ~x+y
#'     nni(meuse)
#'
#' @export
nni <- function(x, win = "hull") {
    if (!class(x) == "SpatialPointsDataFrame" & !class(x) == "SpatialPoints") 
        stop(deparse(substitute(x)), " MUST BE A sp POINTS OBJECT")
    if (win == "hull") {
        w <- spatstat::convexhull.xy(sp::coordinates(x))
    }
    if (win == "extent") {
        e <- as.vector(sp::bbox(x))
        w <- spatstat::as.owin(c(e[1], e[3], e[2], e[4]))
    }
    x <- spatstat::as.ppp(sp::coordinates(x), w)
    A <- spatstat::area.owin(w)
      obsMeanDist <- sum(spatstat::nndist(x))/x$n
      expMeanDist <- 0.5 * sqrt(A / x$n)
      se <- 0.26136 / ((x$n**2.0 / A)**0.5)
      nni <- obsMeanDist / expMeanDist
      z <- (obsMeanDist - expMeanDist) / se
    return(list(NNI = nni, z.score = z, p = 2*stats::pnorm(-abs(z)),  
	       expected.mean.distance = expMeanDist,
		   observed.mean.distance = obsMeanDist))
} 
