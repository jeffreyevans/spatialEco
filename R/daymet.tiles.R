#' @title DAYMET Tile ID's
#' @description Returns a vector of DAYMET tile id's within a specified extent
#'
#' @param ... ignored
#'
#' @return 
#' Vector of DAYMET tile IDS or if sp = TRUE a sp class SpatialPolygonsDataFrame 
#' 
#' @note 
#' Function accepts sp, raster or extent class object or bounding coordinates. 
#' All input must be in the same projection as the tile index SpatialPolygonsDataFrame. 
#' The library includes the DAYMAT tile index "DAYMET_tiles" which can be add using 
#' data(), see examples. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @export daymet.tiles
daymet.tiles <- function(...) {
 message("Because THREDDS has moved to a NetCDF Markup Language (NcML) query service, 
          this function is currently depreciated")
 message("You can access the North America NetCDF subset tool here:
         https://thredds.daac.ornl.gov/thredds/ncss/grid/daymet-v3-agg/na.ncml/dataset.html")	
  .Deprecated("daymet.tiles", package="spatialEco", 
    msg="Function is deprecated due to significant changes to the ORNL DAAC THREDDS server")	  
}
