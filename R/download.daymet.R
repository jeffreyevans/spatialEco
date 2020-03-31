#' @title Download DAYMET
#' @description Batch download of daily gridded DAYMET climate data 
#'
#' @param ... ignored
#' 
#' @details 
#' DAYMET website: \url{http://daymet.ornl.gov},
#' path structure: /year/tile_year/file.nc
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references 
#' Thornton P.E., S.W. Running and M.A. White (1997) Generating surfaces of daily 
#'   meteorological variables over large regions of complex terrain. Journal of
#'   Hydrology 190: 214-251. 
#' @references 
#' Thornton, P.E. and S.W. Running (1999) An improved algorithm for estimating 
#'   incident daily solar radiation from measurements of temperature, humidity, 
#'   and precipitation. Agriculture and Forest Meteorology. 93:211-228.
#' @references 
#' Thornton, P.E., H. Hasenauer and M.A. White (2000) Simultaneous estimation 
#'   of daily solar radiation and humidity from observed temperature and 
#'   precipitation: An application over complex terrain in Austria. 
#'   Agricultural and Forest Meteorology 104:255-271.
#'
#' @export
download.daymet <- function(...) {
 message("Because THREDDS has moved to a NetCDF Markup Language (NcML) query service, 
          this function is currently not able to access the server")
 message("You can access the North America NetCDF subset tool here:
         https://thredds.daac.ornl.gov/thredds/ncss/grid/daymet-v3-agg/na.ncml/dataset.html")	
  .Deprecated("download.daymet", package="spatialEco", 
    msg="Function is deprecated due to significant changes to the ORNL DAAC THREDDS server")	  
}
