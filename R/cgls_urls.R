#' @title Provide URL's for Copernicus Global Land Service datasets
#' @description Returns URL's of a product/version/resolution 
#'
#' @param dates        Dates to subset default is NULL, returns all products
#' @param resolution   The product resolution c("1km", "300m"),
#' @param product      Which product to query options are "fapar", 
#                      "fcover", "lai", "ndvi"
#' @param ver          Product version options are "newest", "v1", "v2", "v3"
#'
#' @return A vector of download URL's for the products
#'
#' @details
#' Provides a query of the ESA's Copernicus Global Land Service global 
#  datasets which can then be used to download product(s).
#' The query is performed on the manifest files and return URL's
#' however, to download data you will need login credentials which,
#' can be acquired from: http://land.copernicus.eu   
#'
#' @details
#' If provided, dates need to be in a "YYYY-MM-DD" format. The dates 
#' are an explicit search string and can contain dates that are not in 
#' the imagery. As such, the user should generate a daily date string
#' representing the range of the desired download as not to have to 
#' guess the available dates. Also note that multiple processing versions
#' of a given image are retained in the manifest. This means that if you
#' download a previous processing version, it could be an invalid image.
#' It is highly recommended that you do not change the default  
#' ver="newest" argument unless there is a specific reason to.  
#' 
#' @details Available products
#' * fapar    Fraction of photosynthetically active radiation  
#'            absorbed by the vegetation
#' * fcover   Fraction of green vegetation cover
#' * lai      Leaf Area index
#' * ndvi     Normalized Difference Vegetation Index 
#' @md
#'
#' @details
#' Not yet implemented; Soil Water Index, Surface Soil Moisture,
#  and Land Surface Temperature.
#' Copernicus product details: http://land.copernicus.eu/global/products/
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \donttest{
#' # Create date string for query
#' d <- seq(as.Date("2020/05/01"), as.Date("2020-09-01"), by="day")
#'
#' # Search for 300m (333m) LAI within specified date range 
#' all.urls <- cgls_urls(dates = d, resolution = 300, 
#'                         product = "lai")		
#' # Search for 1000m LAI within specified date range  
#' all.urls <- cgls_urls(dates = d, resolution = 1000, 
#'                         product = "lai")		
#'
#' ## Return all 300m LAI 
#' # lai <- cgls_urls(resolution = 300, product = "lai")
#' #   head( basename(lai) )
#' 
#' ## Example of downloading URL's
#' ## You need to define your login credentials to download data
#' # username = "xxxx"  
#' # password = "xxxx" 
#' # 	  
#' #   for(i in 1:length(all.urls)){
#' #     if(i > 1){ Sys.sleep(3) }
#' #     file.url <- paste0("https://", paste(username, password, sep=":"), "@", 
#' #                        sub(".*//", "", all.urls[i]))  
#' #     download.file(file.url, file.path(getwd(), 
#' # 	              basename(all.urls[i])), mode = 'wb') 
#' #   }
#' }
#' @export cgls_urls 
cgls_urls <- function(dates = NULL, resolution = c(1000, 300),  
                       product = c("fapar", "fcover", "lai", "ndvi"),
                       ver = c("newest", "v1", "v2", "v3")) {
    manifest.url <- "https://land.copernicus.vgt.vito.be/manifest/"
   if(!any(which(utils::installed.packages()[,1] %in% "stringr")))
      stop("please install stringr package before running this function")	
    if(resolution[1] == 300) {
      r1 = "333m" 
    } else if(resolution[1] == 1000) {
      r1 = "1km"  
    }	
	html <- paste(readLines(manifest.url), collapse="\n")
      manifest.dirs <- stringr::str_match_all(html, "<a href=\"(.*?)\"")[[1]][,2][-c(1:5)]   
        manifest.dirs <- manifest.dirs[grep(product, manifest.dirs)]	
          manifest.dirs <- manifest.dirs[grep(r1, manifest.dirs)]  
		    manifest.dirs <- gsub("/", "", manifest.dirs, fixed=T)  
	if(ver[1] != "newest") {
      if(!ver[1] %in% c("v1","v2","v3"))
	    stop("Not a valid product version")
      collection <- manifest.dirs[grep(ver[1], manifest.dirs)]
	  if( length(collection) < 1)
	    stop("Specified version is not available for this collection")	
	  cgls.url <- paste0(manifest.url, collection, "/", "manifest_cgls_", 
	                     collection, "_latest.txt")
	    con <- url(cgls.url)
          file.url <- readLines(con)		  
	        close(con)
	  if(grepl("does not exist", file.url[10])) 
        stop("This product is not available or the product name is misspecified")			
      if(!is.null(dates)){
       file.url <- file.url[grep(paste(gsub("-", "", dates),collapse="|"), file.url)]
	     if(length(file.url) < 1)
           stop("There are no data in this date range")	
	   }
	} else if(ver[1] == "newest") {
	  file.url <- list()
	    for(i in 1:length(manifest.dirs)) {
          cgls.url <- paste0(manifest.url, manifest.dirs[i], "/", "manifest_cgls_", 
		                     manifest.dirs[i], "_latest.txt")
	        con <- url(cgls.url)
          file.url[[i]] <- readLines(con) 
		close(con)
       } 
	   if(length(file.url) > 1) {
	     file.url <- unlist(file.url)
	   } else {
         file.url <- file.url[[1]]
       } 
       if(!is.null(dates)){
         file.url <- file.url[grep(paste(paste0(gsub("-", "", dates),"0000"), 
	                          collapse="|"), file.url)]
	     if(length(file.url) < 1)
           stop("There are no data in this date range")	
	   }
      file.url <- unique(file.url)		 
      bn <- basename(file.url)
	    d <- unlist(lapply(strsplit(bn, "_"), function(x) x[4]))   
          dup.dates <- sort(d[which(duplicated(d))])
	  if(length(dup.dates) > 0) {  
	    didx <- grep(paste(sort(d[which(duplicated(d))]),  collapse="|"), file.url) 
          dup.url <- file.url[didx]
          file.url <- file.url[-didx]
	    keep.url <- vector() 
          for(i in unique(dup.dates)) {
	        dv <- unique(dup.url[grep(i, dup.url)])
            v <- numeric_version(unlist(lapply(strsplit(basename(dv), "_"), 
		                         function(x) substr(x[7],2,6))))
	          keep.url <- append(keep.url, dv[which(v == max(v))]) 
	      }
	  file.url <- append(file.url, keep.url)
	  }  
	}
  return(file.url)
}
