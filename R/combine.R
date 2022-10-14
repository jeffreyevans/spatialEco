#' @title raster combine
#' @description Combines rasters into all unique combinations of inputs
#'
#' @param x         raster stack/brick or SpatialPixelsDataFrame object
#'
#' @return 
#' A  ratified rasterLayer or a list containing a SpatialPixelsDataFrame 
#' and a data.frame of unique combinations.
#'
#' @details
#' Please note that this is not a memory safe function that utilizes
#' rasters out of memory in the manner that the raster package does.
#' @details
#' If sp = TRUE the object will be a list with "combine", containing 
#' the SpatialPixelsDataFrame with the value attribute containing the 
#' unique combinations, and "summary" with the summary table of collapsed
#' combinations and associated attributes. 
#' @details
#' If sp = FALSE the a single ratified rasterLayer class object is returned 
#' with the summary table as the raster attribute table, this is most similar
#' to the ESRI format resulting from their combine function. 
#'     
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' if(require(terra, quietly = TRUE)) { 
#'
#' # Create example data (with a few NA's introduced)
#'  r1 <- rast(nrows=100, ncol=100)
#'    names(r1) <- "LC1"
#'    r1[] <- round(runif(ncell(r1), 1,4),0)
#'      r1[c(8,10,50,100)] <- NA
#'  r2 <- rast(nrows=100, ncol=100)
#'    names(r2) <- "LC2"
#'    r2[] <- round(runif(ncell(r2), 2,6),0)
#'      r2[c(10,50,100)] <- NA   
#'  r3 <- rast(nrows=100, ncol=100)
#'    names(r3) <- "LC3"
#'    r3[] <- round(runif(ncell(r3), 2,6),0)
#'      r3[c(10,50,100)] <- NA   
#'  r <- c(r1,r2,r3)  
#'    names(r) <- c("LC1","LC2","LC3")
#' 
#'  # Combine rasters with a multilayer stack
#'  cr <- combine(r)
#'    head(cr$summary)
#'    plot(cr$combine)
#'
#' # or, from separate layers
#'  cr <- combine(c(r1,r3))
#' 
#' }
#'
#' @export combine
combine <- function(x) {
  if(!inherits(x, "SpatRaster"))
    stop(deparse(substitute(x)), " must be a terra SpatRaster object")
  if(terra::nlyr(x) < 2)
    stop(deparse(substitute(x)), " needs > 1 layers to combine")  
  is.int <- function(x){
    e <- all.equal(x, as.integer(x), 
          check.attributes = FALSE)
    if(e == TRUE){ 
      return(TRUE) 
    } else { 
      return(FALSE) 
    }
  }	
  r <-  terra::as.data.frame(x, cells=TRUE) 
    if(any(apply(r, 2, is.int) == FALSE))
	  stop("All rasters must be integer, 
	        no floating point data is allowed")
    combine <- apply(r[,-1], 1, function(x) paste(x, collapse = "_"))
      r$value=as.numeric(factor(combine))
	    s <- as.data.frame(table(r[,-1]))
          s <- s[s$Freq != 0,]
		    names(s)[which(names(s) == "Freq")] <- "count"
    r.combine <- x[[1]]
      r.combine[as.numeric(r[,"cell"])] <- as.numeric(r[,"value"]) 
      r.combine[which(!1:terra::ncell(x) %in% as.numeric(r[,"cell"]))] <- NA
   return(list(combine=r.combine, summary=s))	
}
