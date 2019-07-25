#' @title DAYMET Tile ID's
#' @description Returns a vector of DAYMET tile id's within a specified extent
#'
#' @param x       A sp, raster or extent object (with same projection as tiles)
#' @param tiles   A SpatialPolygonsDataFrame tile index (see notes)
#' @param ids     A tile id field in the tiles index 
#' @param coords  A vector of xmin, xmax, ymin, ymax coordinates, in same projection as tiles 
#' @param sp      (TRUE/FALSE) Should an sp class SpatialPolygonsDataFrame object of associate tiles 
#'                 be returned
#'
#' @return Vector of DAYMET tile IDS or if sp = TRUE a sp class SpatialPolygonsDataFrame 
#' 
#' @note Function accepts sp, raster or extent class object or bounding coordinates. All input must 
#'         be in the same projection as the tile index SpatialPolygonsDataFrame. The library includes 
#'         the DAYMAT tile index "DAYMET_tiles" which can be add using data(), see examples. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @examples 
#' library(sp)
#' library(raster)
#' data(DAYMET_tiles)
#' e <- extent(-117.2567, -104.7523, 36.62797, 47.68194) 
#'   plot(DAYMET_tiles)
#'     plot(e, col="red", add=TRUE)
#' 
#' # Using extent object	
#' daymet.tiles(x = e, tiles = DAYMET_tiles, ids = "Id")
#' 
#' # Using sp object
#' e <- as(e, "SpatialPolygons")	
#' daymet.tiles(e, tiles = DAYMET_tiles, ids = "Id")
#' 
#' # Using bounding coordinates	
#' daymet.tiles(coords=c(-117.2567, -104.7523, 36.62797, 47.68194),
#'              tiles = DAYMET_tiles, ids = "Id" )
#' 
#' # Return sp polygons object
#' tiles <- daymet.tiles(x = e, tiles = DAYMET_tiles, ids = "Id", sp = TRUE)
#'   plot(DAYMET_tiles)
#'     plot(tiles, col="red", add=TRUE)
#' 			 
#' \dontrun{
#' # batch download of DAYMET tiles using function
#'   tile.ids = daymet.tiles(e)
#'   download.daymet(years=2010, tile=tile.ids, data.type=c('tmin'))
#'  }
#'
#' @importFrom methods as slot
#' @export
daymet.tiles <- function(x, tiles, ids, coords, sp = FALSE) {
    if(missing(tiles)) 
	  stop("Please specify the tile index \n")
	if( !inherits(tiles, "SpatialPolygonsDataFrame") )
	  stop("tiles must be a SpatailPolygonsDataFrame object \n")
	if (missing(ids)) 
	  stop("Tile ids missing \n")
	if(is.na(charmatch(ids, names(tiles@data))))
      stop(ids, "is not present in the tile index")
    if(!missing(coords)) {
	  x <- raster::extent(coords[1],coords[2],coords[3],coords[4])
	} 
  x <- as(x, "SpatialPolygons")
  x <- sp::SpatialPolygonsDataFrame(x, data.frame(row.names=
         sapply(slot(x, "polygons"), function(x) slot(x, "ID")),
 		 ID=1:length(x)) )
    sp::proj4string(x) <- sp::proj4string(tiles)
  idx <- as.numeric(row.names(stats::na.omit(sp::over(tiles,x))))
  tiles.sub <- tiles[idx,]
    tile.ids <- tiles.sub@data[,ids]
      if (length(tile.ids)==0) {
        stop("Defined extent is outside DAYMET coverage \n")
      }	else { 
	if ( sp == TRUE ) { return( tiles.sub ) } else { return (tile.ids) }
  }  
} 
