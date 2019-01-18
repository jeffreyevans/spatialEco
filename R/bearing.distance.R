#' @title Bearing and Distance
#' @description Calculates a new point [X,Y] based on defined bearing and distance
#' 
#' @param x             x coordinate
#' @param y             y coordinate
#' @param distance      Distance to new point (in same units as x,y)
#' @param azimuth       Azimuth to new point
#' @param EastOfNorth   Specified surveying convention
#'
#' @note East of north is a surveying convention and defaults to true. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples 
#'  pt <- cbind( x=480933, y=4479433)
#'  bearing.distance(pt[1], pt[2], 1000, 40)
#'
#' @export
bearing.distance <- function(x, y, distance, azimuth, EastOfNorth = TRUE) { 
  radians <- function(a) { a * (pi / 180) }
   if(EastOfNorth) {
      pt <- cbind( X = (x + distance * sin(radians(azimuth))), 
                   Y = (y + distance * cos(radians(azimuth)))) 
   } else {
      pt <- cbind( X = (x + distance * cos(radians(azimuth))), 
                   Y = (y + distance * sin(radians(azimuth))))       
  }
  return(pt)
}
