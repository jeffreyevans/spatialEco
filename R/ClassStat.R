#' @title Landscape Class Statistics
#' @description Class-level landscape metrics
#' 
#' @param mat        A matrix of data with patches identified as classes (unique
#'                   integer values) as e.g., a binary landscape of a species  
#'                   distribution or a vegetation map. Matrix can be a raster of 
#'                   or class "asc" (adehabitat package), "RasterLayer" (raster package)  
#'                   "SpatialGridDataFrame" (sp package)
#' @param cellsize   Cell size (in meters) is a single value representing the
#'                   width/height of cell edges (assuming square cells)
#' @param bkgd       Background value for which statistics will not be calculated
#' @param latlon     Boolean value representing if the data is geographic. If
#'                   latlon == TRUE, Matrix must be of class "asc", "RasterLayer"
#'                   or "SpatialGridDataFrame"
#'
#' @return a data.frame with all available landscape metrics  
#'
#' @details Available landscape metrics
#' * class - A particular patch type from the original input matrix 
#' * n.patches - the number of patches of a particular patch type or in a class. 
#' * total.area - the sum of the areas (m2) of all patches of the corresponding patch type.
#' * prop.landscape - the proportion of the total landscape represented by this class
#' * patch.density - the numbers of patches of the corresponding patch type divided by
#'                   total landscape area (m2).  
#' * total.edge - the total edge length of a particular patch type. 
#' * edge.density - edge length on a per unit area basis that facilitates 
#'                  comparison among landscapes of varying size.
#' * landscape.shape.index - a standardized measure of total edge or edge density 
#'                           that adjusts for the size of the landscape.
#' * largest.patch.index - largest patch index quantifies the percentage of
#'                         total landscape area comprised by the largest patch.
#' * mean.patch.area - average area of patches.
#' * sd.patch.area - standard deviation of patch areas.
#' * min.patch.area - the minimum patch area of the total patch areas. 
#' * max.patch.area - the maximum patch area of the total patch areas.
#' * perimeter.area.frac.dim - perimeter-area fractal dimension equals 2
#'                             divided by the slope of regression line obtained by 
#'                             regressing the logarithm of patch area (m2) against 
#'                             the logarithm of patch perimeter (m).
#' * mean.perim.area.ratio - the mean of the ratio patch perimeter. The
#'                           perimeter-area ratio is equal to the ratio of the patch 
#'                           perimeter (m) to area (m2).
#' * sd.perim.area.ratio - standard deviation of the ratio patch perimeter. 
#' * min.perim.area.ratio - minimum perimeter area ratio
#' * max.perim.area.ratio - maximum perimeter area ratio.
#' * mean.shape.index - mean of shape index
#' * sd.shape.index - standard deviation of shape index. 
#' * min.shape.index - the minimum shape index.
#' * max.shape.index - the maximum shape index.
#' * mean.frac.dim.index - mean of fractal dimension index.
#' * sd.frac.dim.index - standard deviation of fractal dimension index.
#' * min.frac.dim.index - the minimum fractal dimension index.
#' * max.frac.dim.index - the maximum fractal dimension index.
#' * total.core.area - the sum of the core areas of the patches (m2).
#' * prop.landscape.core - proportional landscape core
#' * mean.patch.core.area - mean patch core area.
#' * sd.patch.core.area - standard deviation of patch core area.
#' * min.patch.core.area - the minimum patch core area.
#' * max.patch.core.area - the maximum patch core area.
#' * prop.like.adjacencies - calculated from the adjacency matrix, which
#'                           shows the frequency with which different pairs 
#'                           of patch types (including like adjacencies between 
#'                           the same patch type) appear side-by-side on the map
#'                           (measures the degree of aggregation of patch types)
#' * aggregation.index - computed simply as an area-weighted mean class
#'                       aggregation index, where each class is weighted by its 
#'                       proportional area in the landscape.
#' * landscape.division.index - based on the cumulative patch area distribution
#'                              and is interpreted as the probability that two randomly
#'                              chosen pixels in the landscape are not situated in the 
#'                              same patch
#' * splitting.index - based on the cumulative patch area distribution and is
#'                     interpreted as the effective mesh number, or number of patches 
#'                     with a constant patch size when the landscape is subdivided into 
#'                     S patches, where S is the value of the splitting index.
#' * effective.mesh.size - equals 1 divided by the total landscape area (m2) multiplied by 
#'                         the sum of patch area (m2) squared, summed across all patches in 
#                          the landscape.
#' * patch.cohesion.index - measures the physical connectedness of the corresponding patch type.
#' @md
#'
#' @note 
#' ClassStat calculates the class statistics for patch types identified
#' in a matrix of data or in a raster of class "asc" (adehabitat
#' packages), "RasterLayer" (raster package) or "SpatialGridDataFrame" (sp
#' package). The class statistics are based on statistics calculated by fragstats
#'
#' @author Jeremy VanDerWal (code from depreciated/orphaned SDMTools package)
#'
#' @references McGarigal, K., S. A. Cushman, M. C. Neel, and E. Ene. 2002.
#' FRAGSTATS: Spatial Pattern Analysis Program for Categorical Maps. Computer
#' software program produced by the authors at the University of Massachusetts,
#' Amherst.
#'
#' @examples
#' library(raster)
#' library(sp)
#' # define a simple binary matrix
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
#' #perform connected component labelling
#' ( ccl.mat = connected.pixels(tmat) )
#' image(t(ccl.mat[10:1,]),col=c("grey",rainbow(length(unique(ccl.mat))-1)))
#' 
#' #calculate the patch statistics
#' ( ps.data = PatchStat(ccl.mat) )
#' 
#' #calculate the class statistics
#' ( cl.data = ClassStat(tmat) )
#' 
#' #identify background data is 0
#' ( cl.data = ClassStat(tmat,bkgd=0) )
#' 
#' @export 
ClassStat <- function(mat, cellsize = 1, bkgd = NA, latlon = FALSE) {
	aggregation.index <- function(a,g) {
		n = trunc(sqrt(a))
		m = a - n^2
		if (m==0) maxg = 2*n*(n-1)
		if (m<=n) maxg = 2*n*(n-1)+2*m-1
		if (m>n) maxg = 2*n*(n-1)+2*m-2
		minp=rep(0,length(m))
		for (ii in 1:length(m)){
			if (m[ii]==0) minp[ii] = 4*n[ii]
			if (n[ii]^2<a[ii] & a[ii]<=n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 2
			if (a[ii] > n[ii]*(1+n[ii])) minp[ii] = 4 * n[ii] + 4
		}
		return((g/maxg)*100)
	}
	shape.index = function(a,p) {
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

	if (any(class(mat) %in% "RasterLayer")) mat = asc.from.raster(mat)
	if (any(class(mat) == "SpatialGridDataFrame")) mat = asc.from.sp(mat)
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop("objects must be a matrix")
	classes = as.numeric(stats::na.omit(unique(as.vector(mat))))
	classes = classes[order(classes)]
	if (!is.na(bkgd)) classes = classes[-which(classes==bkgd)]
	out = NULL
	for (cl in classes){
		mat2 = mat
		mat2 = mat * 0
		mat2[which(mat==cl)] = 1
		out.patch = PatchStat(connected.pixels(mat2),cellsize=cellsize,latlon=latlon)
		rm(mat2)
		L.cell = sum(out.patch$n.cell)
		L.area = sum(out.patch$area) 
		if (0 %in% out.patch$patchID) out.patch = out.patch[-which(out.patch$patchID==0),]		
		tout = list(class=cl)
		tout$n.patches = nrow(out.patch)
		tout$total.area = sum(out.patch$area)
		tout$prop.landscape = tout$total.area / L.area
		tout$patch.density = tout$n.patches / L.area
		tout$total.edge = sum(out.patch$perimeter)
		tout$edge.density = tout$total.edge / L.area
		tout$landscape.shape.index = shape.index(sum(out.patch$n.cell),sum(out.patch$n.edges.perimeter))
		tout$largest.patch.index = max(out.patch$area) / L.area
		tout$mean.patch.area = mean(out.patch$area)
		tout$sd.patch.area = stats::sd(out.patch$area)
		tout$min.patch.area = min(out.patch$area)
		tout$max.patch.area = max(out.patch$area)
		tout$perimeter.area.frac.dim = 2 / (((tout$n.patches*sum(log(out.patch$perimeter)+log(out.patch$area)))-(tout$total.edge*tout$total.area))/(tout$n.patches*sum(log(out.patch$perimeter^2))-tout$total.edge^2))
		tout$mean.perim.area.ratio = mean(out.patch$perim.area.ratio)
		tout$sd.perim.area.ratio = stats::sd(out.patch$perim.area.ratio)
		tout$min.perim.area.ratio = min(out.patch$perim.area.ratio)
		tout$max.perim.area.ratio = max(out.patch$perim.area.ratio)
		tout$mean.shape.index = mean(out.patch$shape.index,na.rm=T)
		tout$sd.shape.index = stats::sd(out.patch$shape.index,na.rm=T)
		tout$min.shape.index = min(out.patch$shape.index,na.rm=T)
		tout$max.shape.index = max(out.patch$shape.index,na.rm=T)
		tout$mean.frac.dim.index = mean(out.patch$frac.dim.index,na.rm=T)
		tout$sd.frac.dim.index = stats::sd(out.patch$frac.dim.index,na.rm=T)
		tout$min.frac.dim.index = min(out.patch$frac.dim.index,na.rm=T)
		tout$max.frac.dim.index = max(out.patch$frac.dim.index,na.rm=T)	
		tout$total.core.area = sum(out.patch$core.area)
		tout$prop.landscape.core = tout$total.core.area / L.area
		tout$mean.patch.core.area = mean(out.patch$core.area)
		tout$sd.patch.core.area = stats::sd(out.patch$core.area)
		tout$min.patch.core.area = min(out.patch$core.area)
		tout$max.patch.core.area = max(out.patch$core.area)
		tout$prop.like.adjacencies = sum(out.patch$n.edges.internal) / sum(out.patch$n.edges.internal+out.patch$n.edges.perimeter*2)
		tout$aggregation.index = aggregation.index(sum(out.patch$n.cell),sum(out.patch$n.edges.internal)/2)
		tout$lanscape.division.index = 1-sum((out.patch$n.cell / L.cell)^2)
		tout$splitting.index = L.area^2 / sum(out.patch$area^2)
		tout$effective.mesh.size = sum(out.patch$area^2) / L.area 
		tout$patch.cohesion.index = ((1-(sum(out.patch$n.edges.internal)/sum(out.patch$n.edges.internal*sqrt(out.patch$n.cell))) )*((1-1/sqrt(L.cell))/10))*100
		out = rbind(out,as.data.frame(tout))
	}
  return(out)
}
