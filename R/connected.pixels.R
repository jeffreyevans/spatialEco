#' @title Connected Components Labelling 
#' @description Unique Patch Labelling using Connected Components
#' 
#' @param mat is a binary matrix of data with 0 representing background and 1
#' representing environment of interest. NA values are acceptable. The matrix
#' can be a raster of class 'asc' (this & adehabitat package), 'RasterLayer'
#' (raster package) or 'SpatialGridDataFrame' (sp package)
#'
#' @return A matrix of the same dim and class of mat in which unique
#' components (individual patches) are numbered 1:n with 0 remaining background
#' value.
#'
#' @note
#' connected.pixels is a 1 pass implementation of connected components
#' labelling. Here it is applied to identify disjunct patches within a
#' distribution. The raster matrix can be a raster of class 'asc'
#' (adehabitat package), 'RasterLayer' (raster package) or
#' 'SpatialGridDataFrame' (sp package).
#'
#' @author Jeremy VanDerWal (code from depreciated/orphaned SDMTools package)
#'
#' @references 
#' Chang, F., C.J. Chen, and C.J. Lu. 2004. A linear-time component-labeling algorithm 
#' using contour tracing technique. Comput. Vis. Image Underst. 93:206-220.
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
#' # perform connected component labelling
#' ( ccl.mat = connected.pixels(tmat) )
#' image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)))
#' 
#' @export 
#' @useDynLib spatialEco ccl
connected.pixels <- function(mat)	{
	if (any(class(mat) == 'asc')) { 
	  attrib = attributes(mat)
	} else if (any(class(mat) %in% 'RasterLayer')) {
	  attrib = mat
	  mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) {
	  attrib = mat
	  mat = asc.from.sp(mat)
	} else {
	  attrib = attributes(mat)
	}
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	  out = .Call('ccl',mat,PACKAGE='spatialEco')
	if (any(class(attrib) %in% 'RasterLayer')) {
	  attrib <- raster::setValues(attrib, as.vector(t(t(unclass(out))[dim(out)[2]:1,])))
	    return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) {
	  attrib@data[1] = as.vector(unclass(out)[,dim(out)[2]:1])
	    return(attrib)
	} else {
	  attributes(out) = attrib
	    return(out)
	}
}
