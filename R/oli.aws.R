#' @title Query AWS-OLI
#' @description Query of Amazon AWS OLI-Landsat 8 cloud service
#'
#' @param path        landsat path
#' @param row         landsat row
#' @param dates       dates, single or start-stop range in YYYY-MM-DD format 
#' @param cloud.cover percent cloud cover
#' @param processing  processing level ("L1GT" or "L1T")
#'
#' @return
#' data.frame object with:
#' \itemize{ 
#' \item entityId -  Granule ID 
#' \item      L = Landsat
#' \item      X = Sensor
#' \item      SS = Satellite
#' \item      PPP = WRS path
#' \item      RRR = WRS row
#' \item      YYYYMMDD = Acquisition date
#' \item      yyyymmdd = Processing date
#' \item      CC = Collection number
#' \item      TX = Collection category
#' \item acquisitionDate - POSIXct YYYY-MM-DD (eg., 2015-01-02)
#' \item cloudCover -  % cloud cover in whole numbers (0-100)
#' \item processingLevel - USGS processing level
#' \item path - Landsat path
#' \item row - Landsat row
#' }
#'
#' @note 
#' Amazons AWS cloud service is hosting OLI Landsat 8 data granules
#'   \url{https://aws.amazon.com/public-datasets/landsat/}
#'   \url{https://aws.amazon.com/blogs/aws/start-using-landsat-on-aws/}
#'
#' USGS Landsat collections: \url{https://www.usgs.gov/land-resources/nli/landsat}
#' Pre-collection processing levels: "L1T", "L1GT", "L1G"
#' Collection 1 processing levels: "L1TP", "L1GT", "L1GS"
#'     "L1T" and "L1TP" - Radiomertically calibrated and orthorectified (highest level processing) 
#'     "L1GT" and "L1GT" - Radiomertically calibrated and systematic geometric corrections   
#'     "L1G" and "L1GS" - Radiomertically calibrated with systematic ephemeris correction   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \donttest{
#' # Query path 126, row 59, 2013-04-15 to 2017-03-09, <20% cloud cover    
#' ( p126r59.oli <- oli.asw(path=126, row=59, dates = c("2013-04-15", "2017-03-09"), 
#'                           cloud.cover = 20) )
#' }
#'
#' \dontrun{
#' # Download images from query
#'  bands <- c("_B1.TIF", "_B2.TIF", "_B3.TIF", "_B4.TIF", "_B5.TIF", 
#'             "_B6.TIF","_B7.TIF", "_B8.TIF", "_B9.TIF", "_B10.TIF",
#'	         "_B11.TIF", "_BQA.TIF","_MTL.txt") 
#'   for(i in 1:length(p126r59.oli$download_url)) {
#'     oli.url <- gsub("/index.html","",p126r59.oli$download_url[i])
#'	 all.bands <- paste(oli.url, paste0(unlist(strsplit(oli.url, "/"))[8], bands), sep="/")
#'	   for(j in all.bands) {  
#'         try(utils::download.file(url=j, destfile=basename(j), mode = "wb"))
#'        }		 
#'   }
#' }
#'
#' @export
oli.asw <- function(path, row, dates, cloud.cover = 10, 
                    processing) {
    aws.url <- "http://landsat-pds.s3.amazonaws.com/scene_list.gz"
	if( missing(path) ) stop("Must specify landsat path")
      if( missing(row) ) stop("Must specify landsat row")
        if(!missing(processing)) {
	      oli.processing <-  c("L1T", "L1TP", "L1GT", "L1GT", "L1G", "L1GS") 
	         if( !processing %in% oli.processing) stop("Not a valid processing level") 
        }
    oli <- as.data.frame( readr::read_csv(aws.url) )
	  oli <- oli[oli$path == path & oli$row == row,]
      oli <-  oli[oli$cloudCover <= cloud.cover,] 	  
        if(!missing(dates)) {
		  dates <- as.POSIXct(dates) 
		    if(length(dates) < 2) { dates <- c(dates[1], dates[1]) } 
	      oli <- oli[oli$acquisitionDate >= dates[1] & oli$acquisitionDate <= dates[2],]  
	    }
        if(!missing(processing)) {
	      oli.processing <- oli[oli$processingLevel == processing,]
            if( nrow(oli.processing) == 0) { 
			  message("\n", "Processing level ", processing, "not available, returning other results", "\n")  			  
			} else {
			  oli <- oli.processing
            }			
	    }
  if(nrow(oli) < 1) {
    warning("No results were returned")
  } else {
    rm.idx <- which(names(oli) %in% c("min_lat", "min_lon", "max_lat","max_lon"))
      if(length(rm.idx) > 0) oli <- oli[,-rm.idx]
    return(oli)
  }
}
