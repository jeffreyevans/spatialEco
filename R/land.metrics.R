#' @title Landscape metrics for points and polygons
#' @description Calculates a variety of landscape metrics, on binary rasters, for polygons or points with a buffer distance 
#'
#' @param x          SpatalPointsDataFrame or SpatalPolgonsDataFrame class object 
#' @param y          raster class object 
#' @param bkgd       Background value (will be ignored)
#' @param metrics    Numeric index of desired metric (see available metrics)
#' @param bw         Buffer distance (ignored if x is SpatalPolgonsDataFrame) 
#' @param latlon     Is raster data in lat-long (TRUE/FALSE)
#' @param trace      Plot raster subsets and echo object ID at each iteration (TRUE | FALSE)
#'
#' @return If multiple classes are evaluated a list object with a data.frame for each class contaning specified metrics in columns. The data.frame is ordered and shares the same row.names as the input feature class and can be directly joined to the @@data slot. For single class problems a data.frame object is returned.   
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
#'   \item  landscape.division.index - based on the cumulative patch area distribution and is interpreted as the probability that two randomly chosen pixels in the landscape are not situated in the same patch
#'   \item  splitting.index - based on the cumulative patch area distribution and is interpreted as the effective mesh number, or number of patches with a constant patch size when the landscape is subdivided into S patches, where S is the value of the splitting index.
#'   \item  effective.mesh.size - equals 1 divided by the total landscape area (m2) multiplied by the sum of patch area (m2) squared, summed across all patches in the landscape.
#'   \item  patch.cohesion.index - measures the physical connectedness of the corresponding patch type.
#'  }
#' 
#' @note Modifications to the function incorporate multi-class metrics by fetching the unique values of the raster and creating a list object contaning a data.frame for each class. Unfortunately, retrieving unique values is a very slow function.    
#' @note depends: sp, raster, rgeos, SDMTools 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#'  library(raster)
#'  library(sp)
#'
#'  r <- raster(nrows=180, ncols=360, xmn=571823.6, xmx=616763.6, ymn=4423540, 
#'              ymx=4453690, resolution=270, crs = CRS("+proj=utm +zone=12 +datum=NAD83 
#'              +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#'
#'  r[] <- rpois(ncell(r), lambda=1)
#'  r <- calc(r, fun=function(x) { x[x >= 1] <- 1; return(x) } )  
#'  x <- sampleRandom(r, 10, na.rm = TRUE, sp = TRUE)
#'
#'  lmet <- c("prop.landscape", "edge.density", "prop.like.adjacencies", "aggregation.index") 
#'  ( class.1 <- land.metrics(x=x, y=r, bw=1000, bkgd = 0, metrics = lmet) )
#'  ( all.class <- land.metrics(x=x, y=r, bw=1000, bkgd = NA, metrics = lmet ) )
#'
#'  # Pull metrics associated with class "0"
#'  all.class[["0"]]
#'
#' @seealso \code{\link[spatialEco]{focal.lmetrics}}
#' @seealso \code{\link[SDMTools]{ConnCompLabel}}
#' @seealso \code{\link[SDMTools]{PatchStat}} 
#' @seealso \code{\link[SDMTools]{ClassStat}}
#'
#' @export 
land.metrics <- function(x, y, bkgd = NA, metrics = c("prop.landscape"), bw = 1000, latlon = FALSE, trace = TRUE) {
    if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
        stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT")
    if (!inherits(y, "RasterLayer")) 
        stop("MUST BE raster CLASS OBJECT")
	metrics <- unique(c("class", metrics))	
    mnames <- c("class", "n.patches", "total.area", "prop.landscape", "patch.density", "total.edge", "edge.density", 
        "landscape.shape.index", "largest.patch.index", "mean.patch.area", "sd.patch.area", "min.patch.area", "max.patch.area", 
        "perimeter.area.frac.dim", "mean.perim.area.ratio", "sd.perim.area.ratio", "min.perim.area.ratio", "max.perim.area.ratio", 
        "mean.shape.index", "sd.shape.index", "min.shape.index", "max.shape.index", "mean.frac.dim.index", "sd.frac.dim.index", 
        "min.frac.dim.index", "max.frac.dim.index", "total.core.area", "prop.landscape.core", "mean.patch.core.area", 
        "sd.patch.core.area", "min.patch.core.area", "max.patch.core.area", "prop.like.adjacencies", "aggregation.index", 
        "lanscape.division.index", "splitting.index", "effective.mesh.size", "patch.cohesion.index")
	m.idx <- which( mnames %in% metrics )	
	u <- raster::unique(y)
	  if(bkgd %in% u) { u <- u[-which(u == bkgd)] }
	results <- list()
	  for(i in 1:length(u)) { 
	    results[[i]] <- as.data.frame(array(0, dim=c(0, length(metrics))))
          names(results[[i]]) <- mnames[m.idx]
      }
	names(results) <- u	
	for (j in 1:nrow(x)) {
      if (trace == TRUE) cat("Processing observation -", j, "\n")
      lsub <- x[j,]
        if (class(lsub) == "SpatialPointsDataFrame") {
            f <- rgeos::gBuffer(lsub, width = bw, joinStyle = "ROUND", quadsegs = 10)
			fext <- methods::as(raster::extent(f), "SpatialPolygons") 
            cr <- raster::crop(y, fext, snap = "out")
            crop.NA <- raster::setValues(cr, NA)
            fr <- raster::rasterize(f, cr)
            lr <- raster::mask(x = cr, mask = fr)
        }
        if (class(lsub) == "SpatialPolygonsDataFrame") { 
            cr <- raster::crop(y, raster::extent(lsub), snap = "out")
            crop.NA <- raster::setValues(cr, NA)
            fr <- raster::rasterize(lsub, cr)
            lr <- raster::mask(x = cr, mask = fr)
        }
      LM <- SDMTools::ClassStat(lr, cellsize = raster::res(cr)[1], bkgd = bkgd, latlon = latlon)[m.idx]
        if (class(LM) == "NULL") {
		  LM <- as.data.frame(array(0, dim=c( length(u), length(metrics))))
		    LM[] <- NA
            names(LM) <- mnames[m.idx]
        }
	for(i in names(results)) {
      lm.class <- LM[LM$class == i,]
	    if(dim(lm.class)[1] == 0) { lm.class[1,] <- c(i, rep(NA, ncol(lm.class)-1)) }		  
	  results[[i]] <- rbind(results[[i]], lm.class)
      row.names(results[[i]])[nrow(results[[i]])] <- row.names(lsub@data)
    }
  }
  if (length(results) == 1) { return(results[[1]]) 
    } else {
    return(results)
  }	
} 
