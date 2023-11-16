#' @title DAYMET point values
#' @description Downloads DAYMET climate variables for specified point and time-period 
#' 
#' @param lat          latitude of point (decimal degrees WGS84)
#' @param long         longitude pf point (decimal degrees WGS84)
#' @param start.year   First year of data
#' @param end.year     Last year of data
#' @param site         Unique identification value that is appended to data
#' @param files       (TRUE/FALSE) Write file to disk
#' @param echo        (TRUE/FALSE) Echo progress
#'
#' @details 
#' data is available for Long -131.0 W and -53.0 W; lat 52.0 N and 14.5 N
#' Function uses the Single Pixel Extraction tool and returns year, yday, 
#'       dayl(s), prcp (mm/day), srad (W/m^2), swe (kg/m^2), tmax (deg c), 
#'       tmin (deg c), vp (Pa)
#' Metadata for DAYMET single pixel extraction: 
#' \url{ https://daymet.ornl.gov/files/UserGuides/current/readme_singlepointextraction.pdf }
#' 
#' @return A data.frame with geographic coordinate point-level climate results 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'         
#' @examples
#' \donttest{
#' ( d <- daymet.point(lat = 36.0133, long = -84.2625, start.year = 2013, 
#'                     end.year=2014, site = "1", files = FALSE, echo = FALSE) )
#' }
#'
#' @export
daymet.point <- function (lat, long, start.year, end.year, site=NULL, 
                          files = FALSE, echo = FALSE) {
	if(length(find.package("RCurl", quiet = TRUE)) == 0)
      stop("please install RCurl package before running this function")
    if(missing(lat)) stop("Please define lat") 
    if(missing(long)) stop("Please define long") 
	if(missing(start.year)) stop("Please define start year") 
    if(missing(end.year)) stop("Please define end year") 
    if (start.year < 1980) stop("Data not available prior to 1980")
	if (start.year > 2015) stop("Data not available after 2015")
    year.range = paste(seq(start.year, end.year, by = 1), collapse = ",")
    download.url = sprintf("https://daymet.ornl.gov/data/send/saveData?lat=%s&lon=%s&measuredParams=tmax,tmin,dayl,prcp,srad,swe,vp&year=%s", 
                           lat, long, year.range)
	if (echo == TRUE) {
      message(paste("Downloading DAYMET data for: ", site, " at ", 
          lat, "/", long, " latitude/longitude !\n", sep = ""))
    }	
    x <- try( RCurl::getURL(download.url, ssl.verifypeer = FALSE) )
	  if (!inherits(x, "try-error")) {
        dat <- utils::read.csv(textConnection(x), skip = 7)
		  names(dat) <- c("year", "julian", "dayl", "prcp", "srad", "swe", 
		                  "tmax", "tmin", "vp")
		if(!is.null(site)) dat <- data.frame(site = site, dat)
      } else {
	    stop("There was an error downloading this file")
	  }  
	if( files == TRUE ) {
	  if(is.null(site)) site = paste( paste("lat", lat, sep="."), 
	                                  paste("long", long, sep="."), sep=".") 
      utils::write.csv( paste(site, "_", start.year, "_", end.year, ".csv", sep = ""),
	             row.names = FALSE)
    }	
  return(dat)
 }
