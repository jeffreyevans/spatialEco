#' @title Sample annulus
#' @description Creates sample points based on annulus with defined inner and outer radius
#' 
#' @param x              sp SpatialPoints or SpatialPointsDataFrame class object
#' @param r1             Numeric value defining inner radius of annulus (in projection units)
#' @param r2             Numeric value defining outer radius of annulus (in projection units)
#' @param n              Number of samples
#' @param ...            Additional arguments passed to spsample
#'
#' @return sp SpatialPointsataFrame OBJECT
#'
#' @note Function can be used for distance based sampling. This is one sampling method to capture the spatially lagged variation.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' xy <- meuse[2,]
#' 
#' rs100 <- sample.annulus(xy, r1=50, r2=100, n = 50, type = "random")
#' rs200 <- sample.annulus(xy, r1=100, r2=200, n = 50, type = "random")
#' 
#' plot(rs200, pch=20, col="red")
#'   points(rs100, pch=20, col="blue")
#'   points(xy, pch=20, cex=2, col="black")
#'   box()
#'   legend("topright", legend=c("50-100m", "100-200m", "source"), 
#'          pch=c(20,20,20), col=c("blue","red","black"))
#'
#' @export
sample.annulus <- function(x, r1, r2, n = 10, ...) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
  if(r1 >= r2) stop("inner radius (r1) must be smaller than outer (r2)")
    dots <- as.list(match.call(expand.dots = TRUE)[-1])
  if (is.null(dots[["type"]]) & "type" %in% names(dots) == FALSE) dots[["type"]] <-  "random"
  if (is.null(dots[["n"]]) & "n" %in% names(dots) == FALSE) dots[["n"]] <- 10
    c1 <- rgeos::gBuffer(x, byid = TRUE, width = r1, quadsegs = 10)
    c2 <- rgeos::gBuffer(x, byid = TRUE, width = r2, quadsegs = 10)
    annulus <- rgeos::gDifference(c2, c1, byid=TRUE)
	dots[["x"]] <- annulus
	s <- do.call("spsample", dots)
	if(inherits(x, "SpatialPointsDataFrame")) { id = rownames(x@data[1,]) } else { id = "1" } 
  return( sp::SpatialPointsDataFrame(s, data.frame( 
          SID = rep(id,length(s)), PID = 1:length(s) ) ) )
}
