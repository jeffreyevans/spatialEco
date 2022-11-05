#' @title Weighted centroid
#' @description Creates centroid of [x,y] coordinates with optional
#'              weights field 
#'
#' @param x         sf POINT class object                     
#' @param p         Weights column in x 
#' @param spatial   (TRUE/FALSE) Output sf POINT object    
#' 
#' @return 
#' An x,y coordinate or sf POINT object representing the weighted or unweighted 
#' coordinate centroid      
#'
#' @note
#' The weighted centroid is calculated as:
#' [Xw]=[X]*[p], [Yw]=[Y]*[p], [sXw]=SUM[Xw], [sYw]=SUM[Yw], [sP]=SUM[p]
#'    wX=[sXw]/[sP], wY=[sYw]/[sP]    
#'    where; X=X coordinate(S), Y=Y coordinate(S), p=WEIGHT 
#'    
#' @note Depends: sp
#'                                                                  
#' @examples 
#' p = c("sf", "sp")
#' if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
#'   m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
#'   message("Can't run examples, please install ", paste(p[m], collapse = " "))
#' } else {
#' invisible(lapply(p, require, character.only=TRUE))
#' 
#' data(meuse, package = "sp")
#' meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                   agr = "constant")
#' 
#' wt.copper <- wt.centroid(meuse, p='copper') 
#' wt.zinc <- wt.centroid(meuse,  p='zinc') 
#' 
#' plot(st_geometry(meuse), pch=20, cex=0.75, 
#'      main='Weighted centroid(s)')
#'   plot(st_geometry(wt.copper), pch=19, col='red', 
#'        cex=1.5, add=TRUE)  
#'   plot(st_geometry(wt.zinc), pch=19, col='blue', 
#'        cex=1.5, add=TRUE)
#' legend('topleft', legend=c('all','copper', 'zinc'), 
#'        pch=c(20,19,19),col=c('black','red','blue'))
#' }
#' @export wt.centroid                                                                                   
wt.centroid <- function(x, p = NULL, spatial = TRUE) {
  gtypes = c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT")
  if (!inherits(x, c("sf", "sfc"))) 
    stop(deparse(substitute(x)), " must be an sf class object or coercible")
  if(!any(unique(as.character(sf::st_geometry_type(x))) != gtypes[c(1,3)]))
    stop(deparse(substitute(x)), " must be POINT or POLYGON geometry ")
  if(any(unique(as.character(sf::st_geometry_type(x))) == gtypes[c(2,4)]))
    stop("Function cannot accept multi-part geometry, please use st_cast 
	      to create single-part geometry")
  if(!is.na(sf::st_crs(x))) {
    if(sf::st_is_longlat(x) )
      stop("Projection must be in distance units, not lat/long") 
  } else {
    warning("Projection is not defined, lat/long not acceptable input")
  }
  p <- sf::st_drop_geometry(x[,p])[,1]
    if(!is.numeric(p))
	  stop(deparse(substitute(p)), " must be numeric")
    Xw <- sum(sf::st_coordinates(x)[,1] * p)
      Yw <- sum(sf::st_coordinates(x)[, 2] * p)
        wX <- Xw/sum(p)
          wY <- Yw/sum(p)
    xy <- data.frame(ID=1, X=wX, Y=wY)
      if (spatial) {
        xy <- sf::st_as_sf(xy, coords = c("X", "Y"), 
		        crs = sf::st_crs(x), agr = "constant")
	  }  
  return(xy)
} 
