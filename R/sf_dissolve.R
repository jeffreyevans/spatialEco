#' @title Dissolve polygons
#' @description Dissolve polygon feature calss
#' 
#' @param x    An sf POLYGON or MULTIPOLYGON object
#' @param y    An attribute in x to dissolve by, default is NULL
#'
#' @return A dissolved POLYGON or MULTIPOLYGON object 
#'
#' @note If a dissolve attribute is defined, the result will be a 
#' MULTIPOLYGON with the grouping attribute column. If y=NULL all polygons
#' will be dissolved into a single attribute, unless there is spatial
#' discontinuity (eg., gaps) in the data.
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'   nc$group <- ifelse(nc$CNTY_ <= 1902, 1, 
#'                 ifelse(nc$CNTY_ > 1902 & nc$CNTY_ <= 1982, 2, 
#' 				     ifelse(nc$CNTY_ > 1982, 3, NA))) 
#' 
#' # Dissolve by group attribute
#' d <- sf_dissolve(nc, "group")
#'   plot(st_geometry(nc), border="grey")
#'     plot(st_geometry(d), border="red", col=NA, 
#'          lwd=2, add=TRUE) 
#' 
#' # Dissolve all polygons
#' d <- sf_dissolve(nc)
#'   plot(st_geometry(nc), border="grey")
#'     plot(st_geometry(d), border="red", col=NA, 
#'          lwd=2, add=TRUE)
#' 
#' @export
sf_dissolve <- function(x, y=NULL) {
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf POLYGON object")	
  if(!any(unique(as.character(sf::st_geometry_type(x))) != c("POLYGON", "MULTIPOLYGON")))
    stop(deparse(substitute(x)), " must be an sf POLYGON object")	
  if(!is.null(y)) {	
    if(!y %in% names(x))
      stop(deparse(substitute(y)), " not in sf object")
    d <- split(x, f=sf::st_drop_geometry(x[,y])[,1]) |> 
      lapply(sf::st_union)
        atts <- names(d)
          d <- do.call(c, d) |> 
            sf::st_as_sf()		  
		      d[,y] <- atts
  } else {
    d <- sf::st_union(x, by_feature = FALSE) |> 
        sf::st_cast(to="POLYGON") |> 
          sf::st_as_sf()
  }
  return(d)
}  
