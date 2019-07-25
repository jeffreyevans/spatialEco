#' @title Focal landscape metrics
#' @description Calculates a variety of landscape metrics on integer rasters using focal approach
#'
#' @param x             raster class object 
#' @param w             Size of focal window, a single value or two values defining size of matrix (must be odd values)
#' @param bkg           Background value (will be ignored)
#' @param land.value    Raster cell value to calculate metrics on 
#' @param metric        Name of desired metric (see available metrics)
#' @param latlong       (FALSE/TRUE) Is raster data in lat-long 
#'
#' @return RasterLayer class object of specified landscape metric 
#'
#' @details The following metrics are available: 
#'  \itemize{ 
#'   \item  class - a particular patch type from the original input matrix (mat).
#'   \item  n.patches - the number of patches of a particular patch type or in a class.
#'   \item  total.area - the sum of the areas (m2) of all patches of the corresponding patch type.
#'   \item  prop.landscape - the proportion of the total landscape represented by this class
#'   \item  patch.density - the numbers of patches of the corresponding patch type divided by total landscape area (m2).
#'   \item  total.edge - the total edge length of a particular patch type.
#'   \item  edge.density - edge length on a per unit area basis that facilitates comparison among landscapes of varying size.
#'   \item  landscape.shape.index - a standardized measure of total edge or edge density that adjusts for the size of the landscape.
#'   \item  largest.patch.index - largest patch index quantifies the percentage of total landscape area comprised by the largest patch.
#'   \item  mean.patch.area - average area of patches.
#'   \item  sd.patch.area - standard deviation of patch areas.
#'   \item  min.patch.area - the minimum patch area of the total patch areas.
#'   \item  max.patch.area - the maximum patch area of the total patch areas.
#'   \item  perimeter.area.frac.dim - perimeter-area fractal dimension equals 2 divided by the slope of regression line obtained by regressing the logarithm of patch area (m2) against the logarithm of patch perimeter (m).
#'   \item  mean.perim.area.ratio - the mean of the ratio patch perimeter. The perimeter-area ratio is equal to the ratio of the patch perimeter (m) to area (m2).
#'   \item  sd.perim.area.ratio - standard deviation of the ratio patch perimeter.
#'   \item  min.perim.area.ratio - minimum perimeter area ratio
#'   \item  max.perim.area.ratio - maximum perimeter area ratio.
#'   \item  mean.shape.index - mean of shape index
#'   \item  sd.shape.index - standard deviation of shape index.
#'   \item  min.shape.index - the minimum shape index.
#'   \item  max.shape.index - the maximum shape index.
#'   \item  mean.frac.dim.index - mean of fractal dimension index.
#'   \item  sd.frac.dim.index - standard deviation of fractal dimension index.
#'   \item  min.frac.dim.index - the minimum fractal dimension index.
#'   \item  max.frac.dim.index - the maximum fractal dimension index.
#'   \item  total.core.area - the sum of the core areas of the patches (m2).
#'   \item  prop.landscape.core - proportional landscape core
#'   \item  mean.patch.core.area - mean patch core area.
#'   \item  sd.patch.core.area - standard deviation of patch core area.
#'   \item  min.patch.core.area - the minimum patch core area.
#'   \item  max.patch.core.area - the maximum patch core area.
#'   \item  prop.like.adjacencies - calculated from the adjacency matrix, which shows the frequency with which different pairs of patch types (including like adjacencies between the same patch type) appear side-by-side on the map (measures the degree of aggregation of patch types).
#'   \item  aggregation.index - computed simply as an area-weighted mean class aggregation index, where each class is weighted by its proportional area in the landscape.
#'   \item  lanscape.division.index - based on the cumulative patch area distribution and is interpreted as the probability that two randomly chosen pixels in the landscape are not situated in the same patch
#'   \item  splitting.index - based on the cumulative patch area distribution and is interpreted as the effective mesh number, or number of patches with a constant patch size when the landscape is subdivided into S patches, where S is the value of the splitting index.
#'   \item  effective.mesh.size - equals 1 divided by the total landscape area (m2) multiplied by the sum of patch area (m2) squared, summed across all patches in the landscape.
#'   \item  patch.cohesion.index - measures the physical connectedness of the corresponding patch type.
#'  }
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \dontrun{
#'  library(raster)
#'  library(sp)
#'
#'  r <- raster(nrows=180, ncols=360, xmn=571823.6, xmx=616763.6, ymn=4423540, 
#'              ymx=4453690, resolution=270, crs = CRS("+proj=utm +zone=12 +datum=NAD83 
#'              +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#'
#'  r[] <- rpois(ncell(r), lambda=1)
#'  r <- calc(r, fun=function(x) { x[x >= 1] <- 1; return(x) } )  
#' 
#' # proportion landscape class   
#' pland <- focal.lmetrics(r, w=11)
#'   plot(pland)
#'
#' # Aggregation index 
#' ai <- focal.lmetrics(r, w=11, metric = "aggregation.index")
#'   plot(ai)
#' }
#' 
#' @seealso \code{\link[spatialEco]{land.metrics}}
#' @seealso \code{\link[SDMTools]{ConnCompLabel}}
#' @seealso \code{\link[SDMTools]{PatchStat}} 
#' @seealso \code{\link[SDMTools]{ClassStat}}
#'
#' @export 
focal.lmetrics <- function(x, w = 5, bkg = 0, land.value = 1, 
                           metric = "prop.landscape", latlong = FALSE) {
  if( class(x) != "RasterLayer" ) stop( "x is not a valid raster layer")
    if( length(w) <= 1) { window.size = c(w,w) } else { window.size = c(w[1],w[2]) }    					  
    mnames <- c("class","n.patches","total.area",             
        "prop.landscape","patch.density","total.edge",             
        "edge.density","landscape.shape.index","largest.patch.index",    
        "mean.patch.area","sd.patch.area","min.patch.area",         
        "max.patch.area","perimeter.area.frac.dim","mean.perim.area.ratio",  
        "sd.perim.area.ratio","min.perim.area.ratio","max.perim.area.ratio",   
        "mean.shape.index","sd.shape.index","min.shape.index",        
        "max.shape.index","mean.frac.dim.index","sd.frac.dim.index",      
        "min.frac.dim.index","max.frac.dim.index","total.core.area",        
        "prop.landscape.core","mean.patch.core.area","sd.patch.core.area",     
        "min.patch.core.area","max.patch.core.area","prop.like.adjacencies",  
        "aggregation.index","lanscape.division.index","splitting.index",        
        "effective.mesh.size","patch.cohesion.index")						  
  m.idx <- which( mnames %in% metric )
    if( length(m.idx) < 1 ) stop("Not a valid landscape metric")
      cs = raster::res(x)[1] 
  lmetric <- function(v, ws = window.size, focal.values = land.value, bkgs = bkg, 
                      rcs = cs, latlongs = latlong, lm.idx = m.idx) {
    m <- matrix(v, nrow = window.size[1], ncol = window.size[2], byrow = TRUE)
      m <- ifelse(m != focal.values, bkgs, 1)	
    return( as.numeric(SDMTools::ClassStat(m, bkgd = bkgs, cellsize = rcs, 
	        latlon = latlongs))[lm.idx] )
  } 
  return( raster::focal(x, w = matrix(1,window.size[1], ncol = window.size[2]), fun=lmetric) )  
}
