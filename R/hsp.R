#' @title Hierarchical Slope Position
#' @description 
#' Calculates a hierarchical scale decomposition of topographic 
#' position index  
#'                                                                       
#' @param x            A terra SpatRaster class object 
#' @param min.scale    Minimum scale (window size)
#' @param max.scale    Maximum scale (window size)
#' @param inc          Increment to increase scales
#' @param win          Window type, options are "rectangle" or "circle"
#' @param normalize    Normalize results to 0-1 scale (FALSE | TRUE)          
#'  
#' @return  terra SpatRaster class object of slope position 
#'    
#' @note
#' if win  = "circle" units are distance, if win = "rectangle" units 
#' are number of cells 
#'      
#' @references
#' Murphy M.A., J.S. Evans, and A.S. Storfer (2010) Quantify Bufo boreas 
#'   connectivity in Yellowstone National Park with landscape genetics. 
#'   Ecology 91:252-261
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'    
#' @examples
#' \donttest{ 
#'   library(terra)
#'   elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))
#'   hsp27 <- hsp(elev, 3, 27, 4, normalize = TRUE)
#'   plot(hsp27)
#'  }
#'
#' @export  
hsp <- function(x, min.scale = 3, max.scale = 27, inc = 4, win = "rectangle",
                normalize = FALSE) { 
  if (!inherits(x, "SpatRaster")) 
    stop(deparse(substitute(x)), " must be a terra SpatRaster class object") 
  scales = rev(seq(from=min.scale, to=max.scale, by=inc)) 
    for(s in scales) {
	  if( win == "circle") {
	    if( min.scale < terra::res(x)[1] * 2) 
		  stop( "Minimum resolution is too small for a circular window")
			m <- terra::focalMat(x, s, type=c('circle'))
              m[m > 0] <- 1  
          } else { 	  
        m <- matrix(1, nrow=s, ncol=s)
	  }
	message("Calculating scale:", s, "\n")
        scale.r <- x - terra::focal(x, w=m, fun=mean)
	  if( s == max(scales) ) {
        scale.r.norm <- 100 * ( (scale.r - 
		     terra::global(scale.r, "mean", na.rm=TRUE)[,1] / 
             terra::global(scale.r, "sd", na.rm=TRUE)[,1] ) )
	    } else {
	    scale.r.norm <-  scale.r.norm + 100 * ( (scale.r - 
		     terra::global(scale.r, "mean", na.rm=TRUE)[,1] / 
             terra::global(scale.r, "sd", na.rm=TRUE)[,1] ) )   
	  }   			 
    }
  if(normalize == TRUE) {  
 	scale.r.norm <- (scale.r.norm - terra::global(scale.r.norm, "min", na.rm=TRUE)[,1]) /
                    (terra::global(scale.r.norm, "max", na.rm=TRUE)[,1] - 
					 terra::global(scale.r.norm, "min", na.rm=TRUE)[,1])				 			 
  }
  return(scale.r.norm)  
}
