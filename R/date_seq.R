#' @title date sequence
#' @description creates date sequence given start and stop dates
#'
#' @param start      Start date in "yyyy/mm/dd" character format         
#' @param end        End date in "yyyy/mm/dd" character format 
#' @param step       Time step, options are c("day", "week", "month", "quarter", 
#'                                            "year", "minute") 
#' @param rm.leap    Remove extra days in leap years 
#'
#' @return 
#' A date vector of class POSIXct for minute and Date for other options 
#'     
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' # monthly steps 1990/01/01 - 2019/12/31
#' d <- date_seq("1990/01/01", "2019/12/31", step="month")
#'
#' # daily steps 1990/01/01 - 2019/12/31
#' d <- date_seq("1990/01/01", "2019/12/31", step="day")
#'
#' # daily steps 1990/01/01 - 2019/12/31 with leap days removed
#' d <- date_seq("1990/01/01", "2019/12/31", step="day", rm.leap=TRUE)
#'
#' # daily step 2008/12/29 - 2008/12/31, 2008 is leap year
#' d <- date_seq("2008/12/29", "2008/12/31")
#'
#' # minutes step 2008/12/29 - 2008/12/31, 2008 is leap year
#' d <- date_seq("2008/12/29", "2008/12/31", step="minute")
#'
#' @export date_seq 
date_seq <- function(start, end, step = c("day", "week", "month", "quarter", "year", "minute"),
                     rm.leap = FALSE){
  is.leap <- function(year) {
    return(ifelse((year %%4 == 0 & year %%100 != 0) | year %%400 == 0, TRUE, FALSE))
  }
  if(step[1] == "minute"){
    if(rm.leap) 
	  message("The rm.leap argument is not honored for minute sequences")
    d = seq(as.Date(start), as.Date(end), "days")
	if(length(d) > 10) 
	  message("Using minutes, your vector is n=", length(d)*1440)
    d <- seq.POSIXt(min(as.POSIXct(d)), (min(as.POSIXct(d)) + 
                   (length(d)*1440)*60), by = "1 min")
  } else {
    d = seq(as.Date(start), as.Date(end), step[1])
    if(rm.leap)
      d = d[!(format(d,"%m") == "02" & format(d, "%d") == "29"),drop = FALSE]  
  }  
  y <- unique(as.numeric(format(d,"%Y")))
  leap.idx <- which(sapply(y, is.leap))
  if(length(leap.idx) > 0)
    message("The following are leap years ", y[leap.idx], "\n" ,sep="\t")
  return(d)
}
