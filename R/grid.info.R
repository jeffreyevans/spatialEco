#' @title Grid Information
#' @description Grid Information from Geographic (lat lon) Projections
#' 
#' @param lats        geographic coords
#' @param cellsize    cellsize
#' @param r           radius of earth
#'
#' @note
#' Since spatial grids in geographic projections do not have equal area or
#' perimeters, grid.info extracts perimeter & area related information
#' for latitudinal bands with differing longitudinal widths. Outputs
#' lengths are in m using Vincenty's equation distance and areas in m2.
#' Surface areas are calculated summing surface areas of spherical polygons as
#' estimated using l'Huiller's formula.
#'
#' @export 
grid.info <- function(lats, cellsize, r = 6378137) {
	r2 = r^2 
	if (length(cellsize)==1) cellsize=rep(cellsize,2) 
	out = data.frame(lat=lats) 
	toplats = lats+(0.5*cellsize[1]); bottomlats = lats-(0.5*cellsize[1]) #define the top and bottom lats
	check = range(c(toplats,bottomlats),na.rm=TRUE); if (-90>check[1] | 90<check[2]) stop('latitudes must be between -90 & 90')
	out$top = spheroid.distance(toplats,rep(0,length(lats)),toplats,rep(cellsize[2],length(lats)))$distance
	out$bottom = spheroid.distance(bottomlats,rep(0,length(lats)),bottomlats,rep(cellsize[2],length(lats)))$distance
	out$side = spheroid.distance(toplats,rep(0,length(lats)),bottomlats,rep(0,length(lats)))$distance
	out$diagnal = spheroid.distance(toplats,rep(0,length(lats)),bottomlats,rep(cellsize[2],length(lats)))$distance
	excess = function(lam1,lam2,beta1,beta2){ 
		haversine = function(y) { (1-cos(y))/2 }
		cosB1 = cos(beta1); cosB2 = cos(beta2)
		hav1 = haversine(beta2-beta1) + cosB1*cosB2*haversine(lam2-lam1)
		aa = 2 * asin(sqrt(hav1)); bb = 0.5*pi - beta2; cc = 0.5*pi - beta1
		ss = 0.5*(aa+bb+cc)
		tt = tan(ss/2)*tan((ss-aa)/2)*tan((ss-bb)/2)*tan((ss-cc)/2)
		return(abs(4*atan(sqrt(abs(tt)))))		
	}
	if (any(bottomlats==-90)) { pos = which(bottomlats==-90); bottomlats[pos] = -bottomlats[pos]; toplats[pos] = -toplats[pos]} #ensure no -90 bottom lats
	out$area = excess(lam1=0,lam2=cellsize[2]*pi/180,toplats*pi/180,toplats*pi/180)
	out$area = abs(out$area-excess(lam1=0,lam2=cellsize[2]*pi/180,bottomlats*pi/180,bottomlats*pi/180))*r2
	return(out)
}

