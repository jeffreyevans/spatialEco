#' @title Download DAYMET
#' @description Batch download of daily gridded DAYMET climate data 
#'
#' @param years Years to download (valid years 1980-2015)
#' @param tiles Tile index value (see url for tile index grid in notes section)
#' @param data.type Type of climate metric: 'all', 'vp', 'tmin', 'tmax', 'swe', 
#'        'srad', 'prcp', 'dayl'. 
#' @param download.folder local download directory, defaults to current working 
#'                        directory
#' @param http option to change URL
#'
#' @return DAYMET netCDF format climate metrics 
#' 
#' @details Available products:
#' * vp - Water Vapor Pressure Daily average partial pressure of water vapor
#' * tmin - Daily minimum (degrees C) 2-meter air temperature
#' * tmax - Daily maximum (degrees C) 2-meter air temperature
#' * swe - Snow water equivalent (kg/m^2). Amount of water contained within 
#'         snowpack.       
#' * srad - Incident shortwave radiation flux density (W/m^2), taken as average 
#'          over daylight period of the day. 
#' * prcp - Daily total precipitation(mm/day), sum of all forms converted to 
#'          water-equivalent.
#' * dayl - Duration of the daylight period for the day (s/day). Calculation is 
#'          based on the period of the day during which the sun is above a hypothetical 
#'         flat horizon. 
#' @md
#' @details 
#' DAYMET main website: \url{http://daymet.ornl.gov}
#' Tile index information \url{http://daymet.ornl.gov/datasupport.html}
#' Data repository url: https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/tiles
#' Path structure: /year/tile_year/file.nc
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
#' @examples 
#' \donttest{
#' # Download 2009-2010 min and max temp for tiles 11737 and 11738  
#'  laramie.plains <- c(11737, 11738)
#'  my.years <- c(seq(2009,2010,1))
#'  download.daymet(years=my.years, tiles=laramie.plains, data.type=c('tmin','tmax')) 
#' }
#'
#' @export
download.daymet <- function(years, tiles, data.type = "all", 
                            download.folder = c("current", "temp"), 
                            http = "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/tiles") {
    owd <- getwd()
      on.exit(setwd(owd))	
	if (!length(years %in% seq(1980, 2020, 1)) == length(years)) stop("Invalid year defined")
    if ((data.type == "all")[1]) {
	  data.type <- c("vp", "tmin", "tmax", "swe", "srad", "prcp", "dayl") 
	}
    if (!(data.type == "ppt" | data.type == "tmin" | data.type == "tmax" | data.type == "tmean")) 
        stop("Not a valid dataset")
	if(download.folder == "current") {
	  download.folder = getwd()
    } else if(download.folder == "temp") {
	  download.folder = tempdir()
    } 
	if(!dir.exists(download.folder))
      stop("directory does not exists")
	tiles <- rep(tiles, length(years))
    TilesYears <- paste(tiles, years, sep = "_")
      for (y in TilesYears) {
        for (i in data.type) {
          file.name <- paste(paste(unlist(strsplit(i, "[.]"))[1], y, sep = "_"), "nc", sep = ".")
          url.string <- paste(paste(http, unlist(strsplit(y, "_"))[2], y, 
	                          sep = "/"), paste0(i,".nc"), sep="/") 
          try(utils::download.file(url=url.string, destfile=paste(download.folder, file.name,sep="/"), 
		      mode = "wb"))
        }
    }	
} 
