#' @title raster combine
#' @description Combines rasters into all unique combinations of inputs
#'
#' @param x         raster stack/brick or SpatialPixelsDataFrame object
#' @param rnames    Column names to combine in raster stack or sp object
#' @param sp        (FALSE/TRUE) output SpatialPixelsDataFrame 
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
#' library(raster)
#' 
#' r1 <- raster(nrows=100, ncol=100)
#'   r1[] <- round(runif(ncell(r1), 1,4),0)
#' r2 <- raster(nrows=100, ncol=100)
#'   r2[] <- round(runif(ncell(r2), 2,6),0)
#' r3 <- raster(nrows=100, ncol=100)
#'   r3[] <- round(runif(ncell(r3), 2,6),0)
#' r <- stack(r1,r2,r3)  
#'   names(r) <- c("LC1","LC2","LC3")
#' 
#' # Combine rasters in stack
#' ( cr <- combine(r) )
#'   levels(cr)
#'   
#' # Combine rasters in stack, using specific rasters
#' ( cr <- combine(r, rnames=c("LC1","LC3")) )
#' 
#' # Combine rasters in stack, output SpatialPixelsDataFrame
#' cr.sp <- combine(r, sp = TRUE)
#'   head(cr.sp$summary)
#'   class(cr.sp$combine)
#' 
#' # Input SpatialPixelsDataFrame 
#' r.sp <- as(r, "SpatialPixelsDataFrame")
#' cr.sp <- combine(r.sp, sp = TRUE)
#'
#' @export combine
combine <- function(x, rnames = NULL, sp = FALSE) {
  if(!any(class(x)[1] %in% c("RasterStack", "RasterBrick",
     "SpatialPixelsDataFrame")))
    stop("x is not a raster stack or brick object")
  is.int <- function(x){
    e <- all.equal(x, as.integer(x), 
           check.attributes = FALSE)
    if(e == TRUE){ 
      return(TRUE) 
    } else { 
      return(FALSE) 
    }
  }	
  if(!is.null(rnames)){
    nidx <- which(names(x) %in% rnames)
    if(length(nidx) < 2)
	  stop("Rasters do not exist in data")
  } else {
    nidx <- 1:length(names(x))
  }  
  if(any(class(x)[1] %in% c("RasterStack", "RasterBrick"))) {
    r <- methods::as(x[[nidx]], "SpatialPixelsDataFrame")
  } else if(any(class(x)[1] %in% "SpatialPixelsDataFrame")){
    r <- x
      x@data <- x@data[,nidx]
  }  
    if(any(apply(r@data, 2, is.int) == FALSE))
	  stop("All rasters must be integer, 
	        no floating point data is allowed")
    combine <- apply(r@data, 1, function(x) paste(x, collapse = "_"))
      r@data <- data.frame(value=as.numeric(factor(combine)), r@data)
	    s <- as.data.frame(table(r@data))
          s <- s[s$Freq != 0,]
    if(!sp) { 
      r <- raster::raster(r, layer = 1)
        r <- raster::ratify(r)
          rat <- s
          names(rat)[1] <- "ID"
        levels(r) <- rat
    } else {
      r <- list(combine = r, summary = s)
    }
  return(r)	
}
