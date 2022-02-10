#' @title Remove polygon holes
#' @description Removes all holes (null geometry) in polygon sp class objects 
#'
#' @param x SpatialPolygons or SpatialPolygonsDataFrame class object
#'
#' @return SpatialPolygonsDataFrame object with all holes removed
#'
#' @note 
#' A hole is considered a polygon within a polygon representing null geometry 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @examples
#'  library(sp)
#'  Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
#'  Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
#'  Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
#'  Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
#'  polys <- SpatialPolygons(list(Polygons(list(Sr1), "s1"),
#'                  Polygons(list(Sr2), "s2"),
#'                  Polygons(list(Sr3, Sr4), "s3/4")), 1:3)
#'
#' opar <- par(no.readonly=TRUE)
#'    par(mfrow=c(1,2))
#'      plot(polys, col = 1:3, main="with hole")
#'      plot(remove.holes(polys), col = 1:3, main="with hole removed")
#' par(opar)
#'
#' @export remove.holes
remove.holes <- function(x) {
  if(!any(which(utils::installed.packages()[,1] %in% "maptools")))
    stop("please install maptools package before running this function")
  xp <- slot(x, "polygons")
    holes <- lapply(xp, function(x) sapply(methods::slot(x, "Polygons"), methods::slot, "hole"))
    res <- lapply(1:length(xp), function(i) methods::slot(xp[[i]], "Polygons")[!holes[[i]]])
    IDs <- row.names(x)
  x.fill <- sp::SpatialPolygons(lapply(1:length(res), function(i)
                                sp::Polygons(res[[i]], ID=IDs[i])), 
  						        proj4string=sp::CRS(sp::wkt(x)))
  methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, "polygons"), 
                maptools::checkPolygonsHoles)   
  methods::slot(x.fill, "polygons") <- lapply(methods::slot(x.fill, "polygons"), "comment<-", NULL)   
    pids <- sapply(methods::slot(x.fill, "polygons"), function(x) methods::slot(x, "ID"))
    x.fill <- sp::SpatialPolygonsDataFrame(x.fill, data.frame(row.names=pids, ID=1:length(pids)))	   
  return( x.fill )	   
}
