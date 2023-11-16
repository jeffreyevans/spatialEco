#' @title Spherical Variance or Standard Deviation of Surface 
#' @description Derives the spherical standard deviation of a raster surface  
#'    
#' @param r         A terra SpatRaster class object
#' @param d         Size of focal window or a matrix to use in focal function
#' @param variance  (FALSE|TRUE) Output spherical variance rather than standard deviation
#' @param ...       Additional arguments passed to terra:app (can write raster to disk here)
#'
#' @details
#' Surface variability using spherical variance/standard deviation. 
#' The variation can be assessed using the spherical standard deviation of the normal 
#' direction within a local neighborhood. This is found by expressing the normal 
#' directions on the surfaces cells in terms of their displacements in a Cartesian (x,y,z) 
#' coordinate system. Averaging the x-coordinates, y-coordinates, and z-coordinates 
#' separately gives a vector (xb, yb, zb) pointing in the direction of the average 
#' normal. This vector will be shorter when there is more variation of the normals and 
#' it will be longest--equal to unity--when there is no variation. Its squared length 
#' is (by the Pythagorean theorem) given by: R^2 = xb^2 + yb^2 + zb^2
#'    where; x = cos(aspect) * sin(slope) and xb = nXn focal mean of x 
#'           y = sin(aspect) * sin(slope) and  yb = nXn focal mean of y
#'           z = cos(slope) and zb = nXn focal mean of z 	   
#' 
#' The slope and aspect values are expected to be in radians. 
#' The value of (1 - R^2), which will lie between 0 and 1, is the spherical variance. 
#' and it's square root can be considered the spherical standard deviation.
#'
#' @return A terra SpatRaster class object of the spherical standard deviation   
#'
#' @author Jeffrey S. Evans <jeffrey_evans<at>tnc.org>
#'
#' @examples
#' \donttest{
#'  library(terra)
#'  elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'  
#'  ssd <- spherical.sd(elev, d=5)
#'  
#'  slope <- terrain(elev, v='slope')
#'  aspect <- terrain(elev, v='aspect')
#'  hill <- shade(slope, aspect, 40, 270)
#'  plot(hill, col=grey(0:100/100), legend=FALSE, 
#'       main='terrain spherical standard deviation')
#'    plot(ssd, col=rainbow(25, alpha=0.35), add=TRUE)
#' }
#'  
#' @seealso \code{\link[terra]{app}} for details on ... arguments
#' 
#' @export
spherical.sd <- function(r, d, variance = FALSE, ...) {
  if(!inherits(r, "SpatRaster"))	
    stop("r must be a terra or raster object")	
    if(class(d)[1] != "matrix") { d = matrix(1,d,d) }
  s <- terra::terrain(r, v='slope', unit='radians') 
    a <- terra::terrain(r, v='aspect', unit='radians')
      x <- terra::lapp(c(a,s), fun=function(x,y) {cos(x) * sin(y) } )
	    y <- terra::lapp(c(a,s), fun=function(x,y) {sin(x) * sin(y) } )
          z <- terra::app(s, fun=cos)
        xb = terra::focal(x, d, fun=mean)
      yb = terra::focal(y, d, fun=mean)
    zb = terra::focal(z, d, fun=mean)
  if(variance == TRUE) {
    return(
	  terra::lapp( c(xb, yb, zb), fun=function(x,y,z) { 1 - (x^2 + y^2 + z^2) }, ... )
    )	  
  } else {
    return(
      terra::lapp( c(xb, yb, zb), fun=function(x,y,z) { sqrt( 1 - (x^2 + y^2 + z^2) ) }, ... )
	)  
  }  
}
