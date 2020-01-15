#' @title Raster conversion functions for adehabitat, raster and sp packages
#'
#' @param x          is an object of class 'asc', 'RasterLayer' or
#'                   'SpatialGridDataFrame'. For the function as.asc, a matrix
#' @param projs      is a CRS projection string of the Proj4 package
#' @param xll        the x coordinate of the center of the lower left pixel of the map
#' @param yll        the y coordinate of the center of the lower left pixel of the map
#' @param cellsize   the size of a pixel on the studied map
#' @param type       a character string. Either "numeric" or "factor"
#' @param lev        if type = "factor", either a vector giving the labels of
#'                   the factor levels, or the name of a file giving the  
#'                   correspondence table of the map see adehabitat as.asc 
#'                   helpfile details
#'
#' @return Returns an object of class requested.
#'
#' @note
#' asc.from.raster and asc.from.sp extracts data from objects of
#' class 'RasterLayer' (raster package) and class 'SpatialGridDataFrame' 
#' (sp package) into an object of class 'asc' (adehabitat packages). 
#' raster.from.asc and sp.from.asc does the reverse.
#' as.asc creates an object of class 'asc' (SDMTools & adehabitat
#' packages) from a matrix of data. Code and helpfile associated with
#' as.asc were modified from adehabitat package.
#' 
#' These functions provide capabilities of using functions from many
#' packages including adehabitat, sp (plus e.g., maptools, rgdal) and raster.
#' 
#' @author Jeremy VanDerWal (code from depreciated/orphaned SDMTools package)
#'
#' @examples 
#' library(sp)
#' library(raster)
#'
#' #create a simple object of class 'asc'
#' tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100))
#'   print(tasc)
#'   str(tasc)
#' 
#' #convert to RasterLayer
#' traster = raster.from.asc(tasc)
#' str(traster)
#' 
#' #convert to SpatialGridDataFrame
#' tgrid = sp.from.asc(tasc)
#' str(tgrid)
#' 
#' #create a basic object of class asc
#' ( tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)) )
#' 
# @import raster, sp
#' @export 
asc.from.raster <- function(x) {
	if (!any(class(x) %in% 'RasterLayer')) stop('x must be of class raster or RasterLayer')
	cellsize = (x@extent@ymax-x@extent@ymin)/x@nrows
	yll = x@extent@ymin + 0.5 * cellsize
	xll = x@extent@xmin + 0.5 * cellsize
	tmat = t(matrix(raster::getValues(x),nrow=x@nrows,ncol=x@ncols,byrow=T)[x@nrows:1,])
	tmat[which(tmat==x@file@nodatavalue)] = NA
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}

#' @rdname asc.from.raster
#' @export
raster.from.asc <- function(x,projs=NA) {
	if (class(x) != 'asc') stop('x must be of class asc')
	cellsize = attr(x, "cellsize")
	nrows = dim(x)[2]; ncols= dim(x)[1]
	xmin = attr(x, "xll") - 0.5 * cellsize
	ymin = attr(x, "yll") - 0.5 * cellsize
	xmax = xmin + ncols*cellsize
	ymax = ymin + nrows*cellsize
	r <- raster::raster(ncols=ncols, nrows=nrows, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)
	raster::projection(r) <- projs
	tvals = as.vector(t(t(unclass(x))[nrows:1,])); tvals[which(is.na(tvals))] = r@file@nodatavalue
	r <- raster::setValues(r, as.vector(t(t(unclass(x))[nrows:1,])))
	return(r)
}

#' @rdname asc.from.raster
#' @export
asc.from.sp <- function(x) {
	#assumes single band data
	if (!any(class(x) == 'SpatialGridDataFrame')) stop('x must be of class SpatialGridDataFrame')
	cellsize = mean(x@grid@cellsize)
	yll = as.numeric(x@grid@cellcentre.offset[2])
	xll = as.numeric(x@grid@cellcentre.offset[1])
	names(x@data)[1] = 'z'
	tmat = t(matrix(x@data$z,nrow=x@grid@cells.dim[2],ncol=x@grid@cells.dim[1],byrow=T)[x@grid@cells.dim[2]:1,])
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}

#' @rdname asc.from.raster
#' @export
sp.from.asc <- function(x,projs=sp::CRS(as.character(NA))) {
	if (!inherits(x, "asc")) stop('x must be of class asc')
	tgrid = sp::GridTopology(c(attr(x, "xll"),attr(x, "yll")),rep(attr(x, "cellsize"),2),dim(x))
	return(sp::SpatialGridDataFrame(tgrid,data.frame(z=as.vector(unclass(x)[,dim(x)[2]:1])),proj4string=projs))
}

#' @rdname asc.from.raster
#' @export
as.asc <- function(x, xll=1, yll=1, cellsize=1,type=c("numeric", "factor"),lev=levels(factor(x))) {
    type=match.arg(type)
    if (!inherits(x, "matrix")) stop("x should be a matrix")
    mode(x) = "numeric"; attr(x, "xll") = xll; attr(x, "yll") = yll
    attr(x, "cellsize")=cellsize; attr(x, "type") = type
    if (type=="factor") attr(x, "levels") = lev
    class(x) = "asc"
    return(x)
}
