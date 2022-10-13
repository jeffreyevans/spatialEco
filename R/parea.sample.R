#' @title Percent area sample
#' @description Creates a point sample of polygons where n is based 
#'              on percent area
#'
#' @param x         An sf POLYGON object
#' @param pct       Percent of area sampled
#' @param join      {FALSE/TRUE} Join polygon attributed to point sample
#' @param sf        Scaling factor (default is meters to acres conversion factor)
#' @param stype     Sampling type ('random', 'regular', 'nonaligned', 'hexagonal')
#' @param ...       Additional arguments passed to spsample
#'
#' @note 
#' This function results in an adaptive sample based on the area of 
#' each polygon. The default scaling factor (sf) converts meters to
#' acres. You can set sf=1 to stay in the native projection units
#'
#' @return An sf POINT object
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' if(require(sf, quietly = TRUE)) {
#'   nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'     nc <- suppressWarnings(st_cast(nc[c(10,100),], "POLYGON"))
#'   
#'   ( ars <- parea.sample(nc, pct=0.001, join = TRUE, stype='random') ) 
#'     plot(st_geometry(nc))
#'       plot(st_geometry(ars), pch=19, add=TRUE)  
#' }
#'
#' @export
parea.sample <- function(x, pct = 0.1, join = FALSE, sf = 4046.86,    
                         stype = "random", ...) {
  if (!inherits(x, "sf")) 
    stop(deparse(substitute(x)), " must be an sf POLYGON object")
  if(!unique(as.character(st_geometry_type(x))) %in% c("POLYGON", "MULTIPOLYGON"))
    stop(deparse(substitute(x)), " must be an sf POLYGON object")		
  ns <- unlist(lapply(1:nrow(x), function(i) {
    round( (units::drop_units(sf::st_area(x[i,])) / sf) * pct, 0) 
  })) 
  s <- sf::st_as_sf(sf::st_sample(x, size=ns, type=stype, 
                    by_polygon=TRUE, ...))
    if(join == TRUE) {
      s <- suppressWarnings(sf::st_intersection(s, x))
    }
  return(s)
} 
