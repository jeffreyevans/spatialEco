#' @title Class comparison between two nominal rasters
#' @description Creates a labeled cross tabulation between two nominal rasters 
#'       
#' @param x          A terra SpatRaster class object   
#' @param y          A terra SpatRaster class object to compare to x
#' @param values     Expected values in both rasters
#' @param labs       Labels associated with values argument
#' @param pct        (TRUE/FALSE) return proportions rather than counts
#' @param ...        Additional arguments 
#'
#' @details  
#' This function returns a cross tabulation between two nominal rasters. 
#' Arguments allow for labeling the results and returning proportions 
#' rather than counts. It also accounts for asymmetrical classes between
#' the two rasters
#'
#' @return a table with the cross tabulated counts 
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references 
#' Pontius Jr, R.G., Shusas, E., McEachern, M. (2004). Detecting important categorical land changes 
#    while accounting for persistence. Agriculture, Ecosystems & Environment 101(2):251-268.
#'
#' @examples
#' library(terra)
#'  
#' e <- ext(179407.8, 181087.9, 331134.4, 332332.1)
#' lulc2010 <- rast(e, resolution=20)
#'   lulc2010[] <- sample(1:5, ncell(lulc2010), replace=TRUE)
#' lulc2020 <- rast(e, resolution=20)
#'   lulc2020[] <- sample(1:5, ncell(lulc2020), replace=TRUE)
#'  
#'  ( v = sort(unique(c(lulc2010[], lulc2020[]))) )
#'  l = c("water","urban","forest",
#'        "ag","barren")
#' 
#' cross.tab(lulc2010, lulc2020) 
#' cross.tab(lulc2010, lulc2020, values = v, labs = l)
#' cross.tab(lulc2010, lulc2020, values = v, labs = l, pct=TRUE)
#' 
#' # Create asymmetrical classes 
#' na.idx <- which(!is.na(lulc2010[]))
#' lulc2020[na.idx] <- sample(c(1,2,4,5), length(na.idx), replace=TRUE)
#' cross.tab(lulc2010, lulc2020, values = v, labs = l, pct=TRUE)
#' 
#'
#' @export
cross.tab <- function(x, y, values = NULL, labs = NULL,  
                      pct = FALSE, ...) {
  if(!inherits(x, "SpatRaster"))
    stop(deparse(substitute(x)), " Must be a terra SpatRaster object") 
  if(!inherits(y, "SpatRaster"))
    stop(deparse(substitute(y)), " Must be a terra SpatRaster object") 
  if(!is.null(labs)){
    if(length(labs) != length(values))
	  stop("length of labs and values must match")
  }	
  xname = deparse(substitute(x))
  yname = deparse(substitute(y))
  ct <- terra::crosstab(c(x, y), long = FALSE, ...) 
  if(!is.null(labs)) {
    colnames(ct) <- labs[which(values %in% colnames(ct))] 
    rownames(ct) <- labs[which(values %in% rownames(ct))] 
  }
    if(pct) ct <- round(apply(ct, 1, function(x) x / sum(x) ),5) 
	  l <- list( rownames(ct), colnames(ct))
        names(l) <- c(yname, xname)	  
      dimnames(ct) <- l 
  return(ct)
}  
