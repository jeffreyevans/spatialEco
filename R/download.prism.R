#' @title Download PRISM 
#' @description Batch download of monthly gridded PRISM climate data 
#'
#' @param data.type Specify climate metric ('ppt','tmin','tmax','tmean')
#' @param date.range A vector with start and end date in y/m/d format 
#' @param time.step Timestep of product ('daily'/'monthly') 
#' @param download.folder Local download directory, defaults to current working directory
#' @param by.year Create a directory for each year (TRUE/FALSE) 
#' @param unzip.file Unzip file on download (TRUE/FALSE)  
#' @param ftp.site PRISM ftp address to use, default: \url{ftp://prism.oregonstate.edu}
#'
#' @return Compressed or uncompressed PRISM monthly gridded data(bil raster format)
#' 
#' @note Monthly data 1895-1980 is available in a single zip file on the ftp site
#'
#' @note PRISM URL: \url{http://prism.nacse.org/}
#' @note FTP download sites for 400m gridded daily/monthly climate data
#' @note     \url{ftp://prism.oregonstate.edu/daily}
#' @note     \url{ftp://prism.oregonstate.edu/monthly}
#'    
#' @note Naming convention: PRISM_<var>_<stability>_<scale&version>_<date>_bil.zip
#' @note     i.e., 'PRISM_ppt_stable_4kmD1_20100208_bil.zip' 
#' @note Data description: 
#' @note   \url{http://prism.nacse.org/documents/PRISM_datasets_aug2013.pdf}   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @seealso
#' \code{\link{download.daymet}}, \code{\link{download.hansen}}
#'
#' @examples 
#' \dontrun{
# Download monthly precipitation data Jan 1st 2000 to Dec 30th 2001 (n=24)
#'    my.dates <- c('2000/1/1', '2001/12/30')
#'    download.prism('ppt', date.range=my.dates, time.step='monthly', by.year=TRUE)
#' 
#' # Download monthly precipitation data Jan 1st 2000 to Feb 10th 2000 (n=41)
#'    my.dates <- c('2000/1/1', '2000/2/10')
#'    download.prism('ppt', date.range=my.dates, time.step='daily', by.year=TRUE)
#'}
#'
#' @export
download.prism <- function(data.type, date.range, time.step = "monthly", download.folder = getwd(), 
                    by.year = FALSE, unzip.file = TRUE, ftp.site = "ftp://prism.oregonstate.edu") {
    if (!(data.type == "ppt" | data.type == "tmin" | data.type == "tmax" | data.type == "tmean")) 
        stop("Not a valid dataset")
    avl.years <- seq(1895, 2014, by = 1)
    startdate <- as.Date(date.range[1])
    enddate <- as.Date(date.range[2])
    if (format(startdate, "%Y") %in% avl.years == FALSE) 
        stop("Start year is not available")
    if (format(enddate, "%Y") %in% avl.years == FALSE) 
        stop("End year is not available")
    getContent <- function(dirs) {
        urls <- paste(dirs, "/", sep = "")
        fls <- strsplit(RCurl::getURL(urls, dirlistonly = TRUE, 
		               userpwd = "anonymous:anonymous"), "\r*\n")
        ok <- sapply(fls, length) > 0
        unlist(mapply(paste, urls[ok], fls[ok], sep = "", SIMPLIFY = FALSE), 
		       use.names = FALSE)
    }
    ftp.site <- paste(ftp.site, time.step, data.type, sep = "/")
    setwd(download.folder)
    if (time.step == "monthly") {
        dates <- seq.Date(startdate, enddate, by = "month")
    }
    if (time.step == "daily") {
        dates <- seq.Date(startdate, enddate, by = "day")
    }
    years.ch <- format(dates, "%Y")
    years <- unique(years.ch)
    months.ch <- format(dates, "%m")
    days.ch <- format(dates, "%d")
    dl.list <- lapply(years, function(x) NULL)
    for (y in 1:length(dl.list)) {
        dl.list[[y]] <- getContent(paste(ftp.site, years[y], sep = "/"))
    }
    date.format <- dates
    dates <- as.character(dates)
    for (i in 1:length(dates)) {
        dates[i] <- paste(unlist(strsplit(as.character(dates[i]), "-")), 
		                  sep = "", collapse = "")
    }
    if (time.step == "monthly") 
        for (i in 1:length(dates)) {
            dates[i] <- substr(dates[i], 1, 6)
        }
    for (i in 1:length(dl.list)) {
        rm.vector <- vector()
        tmp.vector <- dl.list[[i]]
        for (j in 1:length(tmp.vector)) {
            if (as.numeric(unlist(strsplit(tmp.vector[[j]], "_")[1])[5]) > 1980) {
                if (unlist(strsplit(tmp.vector[[j]], "_")[1])[5] %in% dates == FALSE) 
                  rm.vector <- append(rm.vector, j)
            }
        }
        if (length(rm.vector) > 0) {
            tmp.vector <- tmp.vector[-rm.vector]
            dl.list[[i]] <- tmp.vector
        }
    }
    print(paste("Downloading", data.type, "for dates...", sep = " "))
    print(dates)
    for (i in 1:length(dl.list)) {
        if (by.year) {
            dir.create(file.path(download.folder, years[i]), showWarnings = FALSE)
            setwd(file.path(download.folder, years[i]))
        }
        for (f in 1:length(dl.list[[i]])) {
            file.name <- dl.list[[i]][f]
            try(utils::download.file(file.name, destfile = paste(getwd(), 
			    unlist(strsplit(file.name, "/"))[7], sep = "/")))
            if (unzip.file == TRUE) 
                utils::unzip(unlist(strsplit(file.name, "/"))[7])
        }
        setwd(download.folder)
    }
} 
