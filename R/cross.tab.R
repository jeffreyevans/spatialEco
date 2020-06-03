#' @title Class comparison between two nominal rasters
#' @description Compares two categorical rasters using Cohen's Kappa (d) 
#'              or paired t-test statistic(s)
#'       
#' @param x          rasterLayer class object   
#' @param y          rasterLayer class object to compare to x
#' @param values     Expected values in both rasters
#' @param labs       Labels associated with values argument
#' @param pct        (TRUE/FALSE) return proportions rather than counts
#' @param ...        Additional arguments 
#'
#' @return a table with the cross tabulated counts 
#'
#' @note 
#' This function returns a cross tabulation between two nominal rasters. 
#' Arguments allow for labeling the results and returning proportions 
#' rather than counts. It also accounts for asymmetrical classes between
#' the two rasters
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references Pontius Jr, R.G., Shusas, E., McEachern, M. (2004). Detecting
#' important categorical land changes while accounting for persistence.
#' Agriculture, Ecosystems & Environment 101(2):251-268.
#'
#' @examples
#'  library(sp)
#'  library(raster)
#'    data(meuse.grid)
#'  
#'  r1 <- sp::SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], 
#'                                   data = meuse.grid)
#'  lulc2010 <- raster(r1)
#'    na.idx <- which(!is.na(lulc2010[]))
#'      lulc2010[na.idx] <- sample(1:5, length(na.idx), replace=TRUE)
#'   
#'  lulc2020 <- raster(lulc2010)
#'    lulc2020[na.idx] <- sample(1:5, length(na.idx), replace=TRUE)
#'  
#'  ( v = sort(unique(c(lulc2010[], lulc2020[]))) )
#'  l = c("water","urban","forest",
#'        "ag","barren")
#' 
#'  cross.tab(lulc2010, lulc2020) 
#'  cross.tab(lulc2010, lulc2020, values = v, labs = l)
#'  cross.tab(lulc2010, lulc2020, values = v, labs = l, pct=TRUE)
#' 
#' # Create asymmetrical classes 
#' lulc2020[na.idx] <- sample(c(1,2,4,5), length(na.idx), replace=TRUE)
#' 
#' cross.tab(lulc2010, lulc2020, values = v, labs = l, pct=TRUE)
#' 
#' @seealso \code{raster::\link[raster]{crosstab}}
#'
#' @export
cross.tab <- function(x, y, values = NULL, labs = NULL,  
                      pct = FALSE, ...) {
  if(class(x)[1] != "RasterLayer")
    stop("x must be a raster class object")  
  if(class(y)[1] != "RasterLayer")
    stop("y must be a raster class object")	
  if(!is.null(labs)){
    if(length(labs) != length(values))
	  stop("length of labs and values must match")
  }	
  xname = deparse(substitute(x))
  yname = deparse(substitute(y))
  ct <- raster::crosstab(x, y, long = FALSE, ...) 
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
