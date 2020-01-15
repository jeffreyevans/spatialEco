#' @title Landscape Patch Statistics
#' @description Patch-level landscape metrics
#' 
#' @param mat        A matrix of data with individual patches identified as with
#'                   connected.pixels The matrix can be a raster of class 'asc' (this &
#'                   adehabitat package), 'RasterLayer' (raster package) or
#'                   'SpatialGridDataFrame' (sp package)				     
#' @param cellsize   Cell size (in meters) is a single value representing the
#'                   width/height of cell edges (assuming square cells)
#' @param latlon     Boolean value representing if the data is geographic. If
#'                   latlon == TRUE, matrix must be of class 'asc', 'RasterLayer' 
#'                   or 'SpatialGridDataFrame'
#'
#' @return a data.frame with all available landscape patch metrics  
#'
#' @details Available landscape metrics
#' * patchID - the unique ID for each patch.
#' * n.cell - the number of cells for each patch, specified in square meters.
#' * n.core.cell - the number of cells in the core area, without the edge area. 
#' * n.edges.perimeter - the number of outer perimeter cell edges of the patch. 
#' * n.edges.internal - the number of internal cell edges of the patch.
#' * area - the area of each patch comprising a landscape mosaic. 
#' * core.area - represents the interior area of the patch, greater than the 
#'               specified depth-of-edge distance from the perimeter. 
#' * perimeter - the perimeter of the patch, including any internal holes in the 
#'               patch, specified in meters.
#' * perim.area.ratio - the ratio of the patch perimeter (m) to area (m2).
#' * shape.index - the shape complexity, sum of each patches perimeter
#'                 divided by the square root of patch area.
#' * frac.dim.index - fractal dimension index reflects shape complexity across a 
#'                    range of spatial scales, approaches 2 times the logarithm of 
#'                    patch perimeter (m) divided by the logarithm of patch area (m2). 
#' * core.area.index - quantifies core area as a percentage of patch area.
#' @md
#'
#' @note
#' PatchStat calculates the patch statistics for individual patches
#' identified in a matrix of data. The matrix can be a raster of class 'asc'
#' (adehabitat package), 'RasterLayer' (raster package) or
#' 'SpatialGridDataFrame' (sp package).
#'      
#' @author Jeremy VanDerWal (code from depreciated/orphaned SDMTools package)
#'
#' @references 
#' McGarigal, K., S. A. Cushman, M. C. Neel, and E. Ene. (2002).
#' FRAGSTATS: Spatial Pattern Analysis Program for Categorical Maps. Computer
#' software program produced by the authors at the University of Massachusetts,
#' Amherst.
#'
#' @examples 
#' library(raster)
#' library(sp)
#' #define a simple binary matrix
#' tmat = { matrix(c( 0,0,0,1,0,0,1,1,0,1,
#'                    0,0,1,0,1,0,0,0,0,0,
#'                    0,1,NA,1,0,1,0,0,0,1,
#'                    1,0,1,1,1,0,1,0,0,1,
#'                    0,1,0,1,0,1,0,0,0,1,
#'                    0,0,1,0,1,0,0,1,1,0,
#'                    1,0,0,1,0,0,1,0,0,1,
#'                    0,1,0,0,0,1,0,0,0,1,
#'                    0,0,1,1,1,0,0,0,0,1,
#'                    1,1,1,0,0,0,0,0,0,1),nr=10,byrow=TRUE) }
#' 					
#' #do the connected component labelling
#' ( ccl.mat = connected.pixels(tmat) )
#' image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)))
#' 
#' #calculate the patch statistics
#' ( ps.data = PatchStat(ccl.mat) )
#' 
#' @export PatchStat
#' @useDynLib spatialEco projectedPS geographicPS
PatchStat <- function(mat, cellsize = 1, latlon = FALSE) {
  getXYcoords <- function(w) {
	if (any(class(w) %in% 'RasterLayer')) w = asc.from.raster(w)
	if (any(class(w) == 'SpatialGridDataFrame')) w = asc.from.sp(w)
    if (!inherits(w, "asc")) stop("must be of class asc")
    cs <- attr(w, "cellsize")
      xll <- attr(w, "xll")
        yll <- attr(w, "yll")
          nr <- nrow(w)
        nc <- ncol(w)
      x <- xll+c(0:(nr-1))*cs
    y <- yll+c(0:(nc-1))*cs
  return(list(x=x, y=y))
  }
	shape.index <- function(a,p) {
		n = trunc(sqrt(a))
		m = a - n^2
		minp=rep(0,length(m))
		for (ii in 1:length(m)){
			if (m[ii]==0) minp[ii] = 4*n[ii]
			if (n[ii]^2<a[ii] & a[ii]<=n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 2
			if (a[ii] > n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 4
		}
		return(p/minp)
	}
	if (any(class(mat) %in% 'RasterLayer')) mat = asc.from.raster(mat)
	if (any(class(mat) == 'SpatialGridDataFrame')) mat = asc.from.sp(mat)
	if (latlon){
		if (!any(class(mat) == 'asc')) 
		  stop('matrix must be of class asc, RasterLayer or SpatialGridDataFrame... see helpfile')
		cellinfo = grid.info(getXYcoords(mat)$y,attr(mat,'cellsize'))
		mat = try(as.matrix(mat))
		ID.vals = as.numeric(stats::na.omit(unique(as.vector(mat))))
		ID.vals = ID.vals[order(ID.vals)]
		out = as.data.frame(.Call('geographicPS',mat,ID.vals,cellinfo$area,cellinfo$top,cellinfo$bottom,cellinfo$side,PACKAGE='spatialEco'))
		names(out) = c('patchID','n.cell','n.core.cell','n.edges.perimeter','n.edges.internal','area','core.area','perimeter')
	} else {
		mat = try(as.matrix(mat))
		if (!is.matrix(mat)) stop('objects must be a matrix')
		ID.vals = as.numeric(stats::na.omit(unique(as.vector(mat))))
		ID.vals = ID.vals[order(ID.vals)]
		out = as.data.frame(.Call('projectedPS',mat,ID.vals,PACKAGE="spatialEco"))
		names(out) = c('patchID','n.cell','n.core.cell','n.edges.perimeter','n.edges.internal')
		out$area = out$n.cell * cellsize^2
		out$core.area = out$n.core.cell * cellsize^2
		out$perimeter = out$n.edges.perimeter * cellsize
	}
	out$perim.area.ratio = out$perimeter / out$area 
	out$shape.index = shape.index(out$n.cell,out$n.edges.perimeter)
	out$frac.dim.index = (2 * log(0.25 * out$perimeter)) / log(out$area)
	out$core.area.index = out$core.area / out$area
	return(out)
}
