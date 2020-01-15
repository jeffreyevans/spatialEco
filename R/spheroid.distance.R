#' @title Vincenty Direct Calculation of Distance and Direction
#' @description Estimates the distance on a sphere using lat and long coordinates
#'  
#' @param lat1     a single value or vector of values representing latitude in
#'                 decimal degrees from -90 to 90 degrees. Alternatively, a 
#'                 data.frame or matrix can be used here with each column 
#'                 representing lat1, lon1, lat2, lon2 (in that order).
#' @param lon1     a single value or vector of values representing longitude in
#'                 decimal degrees from -180 to 180 degrees. If NULL, lat1 is 
#'                 assumed to be a matrix or data.frame.
#' @param lat2     a single value or vector of values representing latitude in
#'                 decimal degrees from -90 to 90 degrees. If NULL, lat1 is 
#'                 assumed to be a matrix or data.frame.
#' @param lon2     a single value or vector of values representing longitude in
#'                 decimal degrees from -180 to 180 degrees. If NULL, lat1 is 
#'                 assumed to be a matrix or data.frame.
#' @param bearing  boolean value as to calculate the direction as well as the distance.
#'
#' @return Returns a data.frame with: 
#' * lon1 - the original longitude
#' * lat1 - the original latitude 
#' * lon2 - the destination longitude
#' * lat2 - the destination latitude 
#' * distance - the distance used
#' * bearing - if requested, the bearing between the two points
#' @md
#'
#' @note
#' The spheroid.distance estimates the distance given a starting and ending latitude
#' and longitude. Vincenty's approach, is described as: Vincenty's formulae
#' are two related iterative methods used in geodesy to calculate the distance
#' between two points on the surface of an spheroid.
#' They are based on the assumption that the figure of the Earth is an oblate spheroid,
#' and hence are more accurate than methods such as great-circle distance which
#' assume a spherical Earth. 
#' Note: this method assumes a locations are lat & lon given in WGS 84.Direction, 
#' if requested, is the the initial bearing (sometimes referred to as forward azimuth)
#' which one would follow as a straight line along a great-circle arc from start 
#' to finish. That this will fail if there are NA's in the data.
#'
#' @author Jeremy VanDerWal (code from depreciated/orphaned SDMTools package)
#'
#' @references Vincenty, T. (1975). Direct and Inverse Solutions of Geodesics on
#' the Ellipsoid with application of Nested Equations. Survey Review, vol XXII
#' no 176.
#' 	
#' @export 
#' @useDynLib spatialEco Dist
spheroid.distance <- function(lat1, lon1 = NULL, lat2 = NULL, lon2 = NULL, bearing = FALSE) {
	if (is.data.frame(lat1) | is.matrix(lat1)) { 
		lat1 = as.matrix(lat1); if (ncol(lat1)!=4) stop('incorrect lat/lon inputs... must be matrix with 4 columns or 4 vectors')
		lon2=lat1[,4]; lat2=lat1[,3]; lon1=lat1[,2]; lat1=lat1[,1] 
	} else if (!is.null(lat2) & !is.null(lon1) & !is.null(lon2)) {
		if (!all(c(length(lat2),length(lon1),length(lon2))==length(lat1))) 
	  stop('inputs must all be of same length')
	} else { 
	  stop('inappropriate inputs') 
	}
	if (any(c(lon1,lon2) < -180) | any(c(lon1,lon2) > 180)) stop('lon must be decimal degrees between -180 & 180')
	if (any(c(lat1,lat2) < -90) | any(c(lat1,lat2) > 90)) stop('lat must be decimal degrees between -90 & 90')
	out = data.frame(lat1=lat1,lon1=lon1,lat2=lat2,lon2=lon2)
	out$distance = round(.Call('Dist',out$lat1,out$lon1,out$lat2,out$lon2,PACKAGE='spatialEco'),2) 
	if (bearing) { 
		lat1=lat1*pi/180;lat2=lat2*pi/180;lon1=lon1*pi/180;lon2=lon2*pi/180 #convert to radians
		brng = atan2(sin(lon2-lon1)*cos(lat2),cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(lon1-lon2)) #estimate bearing
		out$bearing = ((brng*180/pi)+360)%%360 #convert to bearing in degrees
	}
	return(out)
}
