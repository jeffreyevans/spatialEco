#' @title Mean Angle
#' @description Calculates the mean angle of a vector
#' 
#' @param a        vector of angle values
#' @param angle    ("degree", "radians") to define angle in degrees or radians
#'
#' @return A vector of mean angle 
#'
#' @note
#' The arithmetic mean is not correct for calculating the central tendency of
#' angles. This function is intended to return the mean angle for slope or aspect,
#' which could be used in a focal or zonal function.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' mean_angle(c(180, 10))
#'   mean(c(180, 10))
#' mean_angle(c(90, 180, 70, 60))
#'   mean(c(90, 180, 70, 60))
#' mean_angle(c(90, 180, 270, 360))
#'   mean(c(90, 180, 270, 360))
#'
#' library(terra)
#' data(elev)
#'   elev <- rast(elev)
#' asp <- terrain(elev, v="aspect")
#' s <- buffer(spatSample(asp, 20, as.points=TRUE, 
#'             na.rm=TRUE, values=FALSE), 5000)
#' 
#' plot(asp)
#'   plot(s, add=TRUE)
#' 
#' d <- extract(asp, s)
#' cat("Mean angles of aspect", "\n")
#'   tapply(d[,2], d[,1], mean_angle) 
#' cat("arithmetic means of aspect", "\n")
#'   tapply(d[,2], d[,1], mean, na.rm=TRUE) 
#' 
#' @export
mean_angle <- function(a, angle=c("degree", "radians")) {
  angle=angle[1]
  a <- a[!is.na(a)]
  if(length(a) < 1)
    stop("Vector appears to be all NA's")
  deg2rad <- function(x) { x * pi/180} 
  rad2deg <- function(x) { x * 180/pi }
  deg2vec <- function(x, ang = c("degree", "radians")) { 
    if(ang == "degree") {
	  a <- c(sin(deg2rad(x)), cos(deg2rad(x)))
	} else if(ang == "radians") {
	  a <- c(sin(x), cos(x))
	}
	return(a)
  }
  vec2deg <- function(x) {
    res <- rad2deg(atan2(x[1], x[2]))
    if (res < 0) { res <- 360 + res }
	return(res)
  }
  mean_vec <- function(x) {
    y <- lapply(x, deg2vec, ang=angle)
    Reduce(`+`, y)/length(y)
  }
  return( vec2deg(mean_vec(a)) ) 
}
