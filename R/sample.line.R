#' @title Systematic or random point sample of line(s)
#' @description Creates a systematic or random point sample of an sp 
#'              SpatialLinesDataFrame object based on distance spacing, 
#'              fixed size or proportional size
#'
#' @param x          sp class SpatialLinesDataFrame object 
#' @param d          Sample distance. For regular sample.   
#' @param n          Fixed sample size. For regular or random
#' @param p          Proportional sample size (length * p), expected value 
#'                   is 0-1. For regular or random. 
#' @param longlat    TRUE/FALSE is data in geographic units, if TRUE distance 
#'                   is in kilometres
#' @param type       Defines sample type. Options are "regular" or "random". A 
#'                   regular sample results in a systematic, evenly spaced sample. 
#' @param min.samp   Minimal number of sample points for a given line 
#'                   (default is 1 point)     
#' @param ...        Additional argument passed to spsample       
#'
#' @return sp SpatialPointsDataFrame object. 
#'
#' @description
#' The sdist argument will produce an evenly spaced sample, whereas n produces 
#' a fixed sized sample. The p (proportional) argument calculates the percent 
#' of the line-length. The LID column in the @@data slot corresponds to the row.names 
#' of the SpatialLinesDataFrame object. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#'  require(sp)
#'  sp.lines <- SpatialLines(list(Lines(list(Line(cbind(c(1,2,3),c(3,2,2)))), 
#"                ID="1"), Lines(list(Line(cbind(c(1,2,3),c(1,1.5,1)))), 
#'                ID="2")))
#'  sp.lines <- SpatialLinesDataFrame( sp.lines, data.frame(ID=1:2, 
#'                                     row.names=c(1,2)) )
#' 
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(2,2))
#'  # Create systematic sample at 20 km spacing 
#'  reg.sample <- sample.line(sp.lines, d = 20, type = "regular", 
#'                            longlat = TRUE)
#'    plot(sp.lines)
#'      plot(reg.sample, pch = 20, add = TRUE)
#'	  box()
#'	  title("systematic d = 20")
#'
#'  # Create fixed size (n = 20) systematic sample
#'  reg.sample <- sample.line(sp.lines, n = 20, type = "regular", 
#'                            longlat = TRUE)  
#'    plot(sp.lines)
#'      plot(reg.sample, pch = 20, add = TRUE)
#'      box()
#'	  title("systematic n = 20")
#'	  
#'  # Create fixed size (n = 20) random sample 
#'  rand.sample <- sample.line(sp.lines, n = 20, type = "random", 
#'                             longlat = TRUE)  
#'    plot(sp.lines)
#'      plot(rand.sample, pch = 20, add = TRUE)
#'      box()	  
#'      title("rand n = 20")
#'	  
#'  # Create proportional (p = 0.10) random sample
#'  rand.sample <- sample.line(sp.lines, p = 0.10, type = "random", 
#'                             longlat = TRUE)  
#'    plot(sp.lines)
#'      plot(rand.sample, pch = 20, add = TRUE)
#'      box()
#'	  title("rand p = 0.10")
#' par(opar)
#'	
#' @export
sample.line <- function(x, d = 100, p = NULL, n = NULL, type = "regular", 
                        longlat = FALSE, min.samp = 1, ...) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
  if (!inherits(x, "SpatialLinesDataFrame")) 
    stop("x is not a SpatialLinesDataFrame object")
  if( (!is.null(p) == TRUE) && (!is.null(n) == TRUE) ) 
    stop( "Cannot have both a fixed and proportional sample") 
    lids <- rownames(x@data)   
      samp.size <- function(l, p.s = p, n.s = n, d.s = d, longlat.s = longlat,  
	                        min.samp.s = min.samp) {
        line.length <- sp::SpatialLinesLengths(l, longlat = longlat.s)
          if( !is.null(n.s) ) { 
	        ns = n.s 	  
	      } else if( !is.null(p.s) ) { 
	        ns <- round( line.length[1] * p.s, digits=0)  
	      } else { 
	        ns <- round( (line.length[1] / d.s), digits=0) 
	      }
	    if (ns < 1) ns = min.samp
	  return( ns )
      }  
    lsub <- x[rownames(x@data) == lids[1] ,]
    ns <- samp.size( lsub )
	lsamp <- sp::spsample(lsub, n = ns, type = type, ...)
    results <- sp::SpatialPointsDataFrame(lsamp, 
	  data=data.frame(LID=rep(as.numeric(lids[1]), ns))) 
      if( length(lids) > 1) { 
	    for (i in 2:length(x) ) 
          {    
		   lsub <- x[rownames(x@data) == lids[i] ,]
           ns <- samp.size( lsub ) 		   
	       lsamp <- sp::spsample(lsub, n = ns, type = type, ...)	 
           lsamp <- sp::SpatialPointsDataFrame(lsamp, 
		      data=data.frame(LID=rep(as.numeric(lids[i]), ns)))
           results <- rbind(results, lsamp)     
         }
	  }
  return( results )
}
