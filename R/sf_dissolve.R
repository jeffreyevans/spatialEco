#' @title Dissolve polygons
#' @description Dissolve polygon feature calss
#' 
#' @param x         An sf POLYGON or MULTIPOLYGON object
#' @param y         An attribute in x to dissolve by, default is NULL
#' @param overlaps  (FALSE/TRUE) Dissolve overlapping polygons, negates using attribute
#'
#' @return A dissolved POLYGON or MULTIPOLYGON object 
#'
#' @note If a dissolve attribute is defined, the result will be a 
#' MULTIPOLYGON with the grouping attribute column. If y=NULL all polygons
#' will be dissolved into a single attribute, unless there is spatial
#' discontinuity (eg., gaps) in the data. The intent of overlaps=TRUE is to 
#' provide functionality for dissolving overlapping polygons and should only
#' be used in this specialized case.
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
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
#' # Dissolve overlapping polygons
#' sq <- function(pt, sz = 1) st_polygon(list(rbind(c(pt - sz), 
#'      c(pt[1] + sz, pt[2] - sz), c(pt + sz), c(pt[1] - sz, pt[2] + sz), 
#' 	 c(pt - sz))))
#' pol <- st_sf(box = 1:6, st_sfc(sq(c(4.2,4.2)), sq(c(0,0)), sq(c(1, -0.8)), 
#'           sq(c(0.5, 1.7)), sq(c(3,3)), sq(c(-3, -3))))
#'  st_geometry(pol) <- "geometry" 		  
#' 
#' plot(pol)
#' 
#' d <- sf_dissolve(pol, overlaps=TRUE)
#'   plot(d["diss"])
#' 
#' @export
sf_dissolve <- function(x, y=NULL, overlaps=FALSE) {
  if(!inherits(x, "sf"))		
    stop(deparse(substitute(x)), " must be an sf POLYGON object")	
  if(!any(unique(as.character(sf::st_geometry_type(x))) != c("POLYGON", "MULTIPOLYGON")))
    stop(deparse(substitute(x)), " must be an sf POLYGON object")	
  if(!is.null(y)) {	
    if(!y %in% names(x))
      stop(deparse(substitute(y)), " is not in sf objects attributes")
    d <- split(x, f=sf::st_drop_geometry(x[,y])[,1]) |> 
      lapply(sf::st_union)
        atts <- names(d)
          d <- do.call(c, d) |> 
            sf::st_as_sf()		  
		      d[,y] <- atts
  } else if(overlaps == FALSE) {
    d <- sf::st_union(x, by_feature = FALSE) |> 
        sf::st_cast(to="POLYGON") |> 
          sf::st_as_sf()
  } else if(overlaps == TRUE) {
    if(!any(sf::st_overlaps(x,sparse = FALSE) == TRUE))
      stop("There are no overlapping geometries")
    diss <- unlist(sf::st_intersects(x, 
      sf::st_as_sf(sf::st_cast(sf::st_union(x),"POLYGON"))))
        d <- cbind(x, diss) |>
          dplyr::group_by(diss)
            d <- dplyr::summarize(d, paste(d$box, collapse = ", ")) 
			  names(d)[2] <- "box"
        sf::st_geometry(d) <- "geometry"
  }
    sf::st_crs(d) <- sf::st_crs(x) 
  return(d)
}  
