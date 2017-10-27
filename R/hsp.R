#' @title Hierarchical Slope Position
#' @description Calculates a hierarchical scale decomposition of topographic position index  
#'                                                                       
#' @param x            Object of class raster (requires integer raster)  
#' @param min.scale    Minimum scale (window size)
#' @param max.scale    Maximum scale (window size)
#' @param inc          Increment to increase scales
#' @param win          Window type, options are "rectangle" or "circle"
#' @param normalize    Normalize results to 0-1 scale (FALSE | TRUE)          
#'  
#' @return raster class object 
#'    
#' @note
#' if win  = "circle" units are distance, if win = "rectangle" untis are number of cells 
#'      
#' @references
#' Murphy M.A., J.S. Evans, and A.S. Storfer (2010) Quantify Bufo boreas connectivity in Yellowstone National Park with landscape genetics. Ecology 91:252-261
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'    
#' @examples
#' \dontrun{ 
#'   library(raster)
#'   data(elev)
#'   hsp27 <- hsp(elev, 3, 27, 4, normalize = TRUE)
#'   plot(hsp27)
#'  }
#'
#' @export  
hsp <- function(x, min.scale = 3, max.scale = 27, inc = 4, win = "rectangle",
                normalize = FALSE) { 
  scales = rev(seq(from=min.scale, to=max.scale, by=inc)) 
    for(s in scales) {
	  if( win == "circle") {
	    if( min.scale < raster::res(x)[1] * 2) 
		  stop( "Minimum resolution is too small for a circular window")
	        m <- raster::focalWeight(x, s, type=c('circle'))
              m[m > 0] <- 1  
          } else { 	  
        m <- matrix(1, nrow=s, ncol=s)
	  }
	cat("Calculating scale:", s, "\n")
        scale.r <- x - raster::focal(x, w=m, fun=mean)
	  if( s == max(scales) ) {
        scale.r.norm <- 100 * ( (scale.r - raster::cellStats(scale.r, stat="mean") / 
                                 raster::cellStats(scale.r, stat="sd") ) )
	    } else {
	    scale.r.norm <-  scale.r.norm + 100 * ( (scale.r - raster::cellStats(scale.r, stat="mean") / 
                                                 raster::cellStats(scale.r, stat="sd") ) )   
	  }   			 
    }
  if(normalize == TRUE) {  
    scale.r.norm <- (scale.r.norm - raster::cellStats(scale.r.norm, stat="min")) /
                    (raster::cellStats(scale.r.norm, stat="max") - 
					 raster::cellStats(scale.r.norm, stat="min"))
  }
  return(scale.r.norm)  
}
