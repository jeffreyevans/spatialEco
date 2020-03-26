#' @title Sample transect
#' @description Creates random transects from points and generates 
#'              sample points along each transect
#' 
#' @param x           A sp point object
#' @param min.length  Minimum length of transect(s)
#' @param max.length  Maximum length of transect(s)
#' @param id          A unique identification column in x
#' @param ...         Additional arguments passed to sample.line 
#'
#' @note 
#' Function create random direction and length transects and then creates a point 
#' sample along each transect. The characteristic of the sample points are defined 
#' by arguments passed to the sample.line function
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples 
#' library(sp)
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' meuse <- meuse[sample(1:nrow(meuse),10),]
#' 
#' transects <- sampleTransect(meuse, min.length=200, 
#'                     max.length=500, min.samp = 3)
#'   plot(transects$transects)
#'     plot(transects$samples, pch=20, add=TRUE)
#'
#' @export
sampleTransect <- function(x, min.length, max.length, id = NULL, ...) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
  tlines <- list()   
  tpoints <- list()
    for(i in 1:nrow(x) ) {
      az = stats::runif(1, 0, 360)
	  d = stats::runif(1, min.length, max.length)
      p <- x[i,]
	  if(!is.null(id)) { ids <- p@data[,id] } else { ids = i } 
      samp.pt <- spatialEco::bearing.distance(sp::coordinates(p)[,1], sp::coordinates(p)[,2], 
	                                          distance = d, azimuth = az)
	  sp.lines <- sp::SpatialLinesDataFrame(sp::SpatialLines(list(sp::Lines(list(sp::Line(matrix(c(sp::coordinates(p),
	                                        samp.pt),nrow=2, ncol=2, byrow=TRUE))), ID=i))), 
											data.frame(row.names = as.character(i), IDS = ids))	
	  dots <- as.list(match.call(expand.dots = TRUE)[-1])
        dots[["x"]] <- sp.lines
        if (is.null(dots[["min.samp"]]) & "min.samp" %in% names(dots) == FALSE) dots[["min.samp"]] <- 2
        if (is.null(dots[["type"]]) & "type" %in% names(dots) == FALSE) dots[["type"]] <-  "regular"
		if (is.null(dots[["offset"]]) & "offset" %in% names(dots) == FALSE) dots[["offset"]] <-  c(1,1)
	  tpoints[[i]] <- do.call(spatialEco::sample.line, dots)
	  tlines[[i]] <- sp.lines
    }
  return( list( transects = do.call("rbind", tlines), 
          samples = do.call("rbind", tpoints) ) )	
}    
