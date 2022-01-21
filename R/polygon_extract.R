#' @title polygon raster extract
#' @description Fast method for extracting raster values to polygons 
#' 
#' @param r           RasterLayer, RasterStack, RasterBrick or SpatRaster object      
#' @param p           sf polygon data  
#' @param ids         A unique id field contained in p, will be assigned to 
#'                    output otherwise will return rownames
#' @param cells       FALSE | TRUE - Return cell index ids
#' @param asList      TRUE | FALSE - Output list object
#' @param use.terra   FALSE | TRUE - Use terra for extracting indices
#'
#' @return A list object containing a data.frame for each polygons
#'         raster values, as columns. Additional columns are "row_names"
#'         or which ever column is passed to the ids argument and "cells"
#'         if cells = TRUE. If asList = FALSE a data.frame will be returned
#'
#' @description
#' This method for raster extraction uses the raster cell indices and
#' is quite a bit faster with polygon data than other methods. This is
#' especially true with large raster stacks (eg., time-series). The cell 
#' indices are returned using \code{\link[tabularaster]{cellnumbers}}.
#' If ids argument is provided a column with values from the associate 
#' column are included otherwise "row_names" is returned which corresponds
#' to the rownames in the source polygon object. Please note that if 
#' use.terra = TRUE it will coerce to a terra class if not already. With 
#' large data the coercion overhead may be worth it, providing speed gains.
#' If the raster is a terra SpatRaster it will operate in its native class.
#' The cells = TRUE argument will return the cell indices which could be 
#' used at a later time.          
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' p = c("sf", "raster", "tabularaster")
#'  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
#'    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
#'    message("Can't run examples, please install ", paste(p[m], collapse = " "))
#'  } else {
#'    invisible(lapply(p, require, character.only=TRUE))
#'  
#'  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'    nc <- sf::st_cast(nc, "POLYGON")
#'  
#'  #### multi-band 
#'  i=100; j=100
#'  r <- do.call(raster::stack, replicate(20, 
#'  	         raster::raster(matrix(runif(i*j), i, j))))
#'        names(r) <- paste0("time", 1:nlayers(r))			 
#'      extent(r) <- extent(nc)
#'  	  proj4string(r) <- st_crs(nc)$proj4string
#'  
#'  plot(r[[1]])
#'    plot(st_geometry(nc), add=TRUE)
#'  
#'  ( e <- polygon_extract(r, nc) )
#'  ( e <- polygon_extract(r, nc, ids="CNTY_ID") )
#'  
#'  # Column means
#'  lapply(e, function(x) apply(x[,2:ncol(x)], MARGIN=1, FUN=mean) )
#'  
#'  #### Single band mean
#'  ( e <- polygon_extract(r[[1]], nc, ids="CNTY_ID") )
#'    unlist(lapply(e, function(x) mean(x[,2], na.rm=TRUE) ))
#'
#' \donttest{ 
#'  # Leveraging cell ids, pulls values, calculates   
#'  # new value, and assigns to source cell using
#'  # index from cells = TRUE 
#'
#'  e <- polygon_extract(r, nc, cells=TRUE)
#'  e <- data.frame(
#'    cells=unlist(lapply(e, function(x) as.numeric(x[,22]))),
#'    means=unlist(lapply(e, function(x) apply(x[,2:22], MARGIN=1, FUN=mean))) 
#'  )
#'  
#'  # copy raster and assign NA's
#'  r2 <- r[[1]]
#'    r2[] <- rep(NA, ncell(r))
#'
#'  # assign using cell indices
#'  r2[e$cells] <- e$means 
#'    plot(r2)
#'
#'  # benchmark against raster extract
#'  system.time({
#'    e <- polygon_extract(r, nc)
#'  })
#'  
#'  system.time({
#'    e <- raster::extract(r, nc)
#'  })
#'
#' }
#'  }
#'
#' @export polygon_extract 
polygon_extract <- function(r, p, ids = NULL, cells = FALSE, asList = TRUE,  
                            use.terra = FALSE) {
  if(!any(which(utils::installed.packages()[,1] %in% "tabularaster")))
    stop("please install tabularaster package before running this function")
  if(!any(class(r)[1] %in% c("RasterLayer", "RasterBrick","RasterStack"))) 
    stop("r is not a raster stack or brick object")
  if(any(methods::is(p, "Spatial"))) {
    type="dsp"
    if(!any(class(p)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) 
      stop("p must be a sp polygon object")
  } else if(methods::is(p, "sf")) {
    type="dsf"
    if(!any(attributes(p$geometry)$class[1] == c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
	  stop("p must be a sf sfc_POLYGON object")
    if(any(attributes(p$geometry)$class[1] == "sfc_MULTIPOLYGON"))
	  warning("p is a MULIPOLYGON object and may produce unexpected results")	  
  } else {
    stop("p needs to be sp or sf polygon objects")
  }	
  index <- as.data.frame(tabularaster::cellnumbers(r[[1]], p) )
    index <- tapply(index$cell_, index$object_, unique)
	  if(cells) { cell.values <- unlist(index) }	
      if(use.terra == TRUE | class(r) == "SpatRaster") {
	    if(!any(which(utils::installed.packages()[,1] %in% "terra")))
          stop("please install terra package before running this function")
	    if(class(r) != "SpatRast") { r <- terra::rast(r) }
	      index <- lapply(index, function(x) terra::extract(r, x) )
	    } else {
	      index <- lapply(index, function(x) raster::extract(r, x) )
	    }
        id <- as.numeric(names(index))
          if(!is.null(ids)) {
	        id <- sf::st_drop_geometry(p[id,][ids])[,1]
	      } else {		  
	        id <- rownames(p)[id]
			  ids = "row_names"
	      }
      names(index) <- id
    index <- lapply(1:length(index), function(i) data.frame(ids=names(index)[i], index[[i]]) ) 
	  
	if(cells) {
	  index <- data.frame(do.call("rbind", index), cells=cell.values)
        names(index) <- c(ids, names(r), "cell") 
	} else {
	  index <- as.data.frame(do.call("rbind", index))
        names(index) <- c(ids, names(r)) 	  
	}
  if(asList) {	
    return( split(index, f=as.factor(index[,ids])) )
  } else {
    return( index )
  }
}
