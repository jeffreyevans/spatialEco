#' @title Erase points
#' @description Removes points intersecting a polygon feature class
#'    
#' @param y       A sf POINT object   
#' @param x       A sf POLYGON object  
#' @param inside  (TRUE/FALSE) Remove points inside polygon, else outside polygon
#'
#' @return A sf POINT object
#' 
#' @note 
#' Used to erase points that intersect polygon(s). The default of inside=TRUE
#' erases points inside the polygons however, if inside=FALSE then
#' the function results in an intersection where points that
#' intersect the polygon are retained. 
#'
#' @author Jeffrey S. Evans    <jeffrey_evans<at>tnc.org>
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
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' 
#'   s <- st_as_sf(st_sample(st_as_sfc(st_bbox(meuse)), size=1000, 
#'                  type = "regular"))
#'     s$id <- 1:nrow(s)
#'   b <- st_buffer(s[sample(1:nrow(s),5),], dist=300)
#'     b$id <- 1:nrow(b)
#'   
#' # Erase points based on polygons
#' in.erase <- erase.point(s, b)
#' out.erase <- erase.point(s, b, inside = FALSE)
#' 
#'  opar <- par(no.readonly=TRUE)
#'  par(mfrow=c(2,2))
#'    plot(st_geometry(s), pch=20, main="original data")
#'      plot(st_geometry(b),add=TRUE)
#'    plot(st_geometry(in.erase), pch=20, main="erased data")
#'      plot(st_geometry(b),add=TRUE)
#'    plot(st_geometry(out.erase), pch=20,  
#'         main="erased data using inside=FALSE")
#'      plot(st_geometry(b),add=TRUE)
#'  par(opar)
#' 
#' }
#'
#' @export erase.point
erase.point <- function(y, x, inside = TRUE) {
  if(!inherits(y, "sf"))		
    stop(deparse(substitute(y)), " must be an sf POINT object")	
  if(unique(as.character(st_geometry_type(y))) != "POINT")
    stop(deparse(substitute(y)), " must be an sf POINT object")		
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf POLYGON object")
  if(unique(as.character(st_geometry_type(x))) != "POLYGON")
    stop(deparse(substitute(x)), " must be an sf POLYGON object")		
  idx <- sf::st_intersects(y, x, sparse = FALSE)   
  if(inside == TRUE) { 
    y <- y[-which(apply(idx, 1, any)),]
  } else {
    y <- y[which(apply(idx, 1, any)),]  
  }
  return(y)
}
