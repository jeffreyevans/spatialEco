#' @title Raster Distance
#' @description Calculates the Euclidean distance of a defined raster class and
#'              all the other cells in a taster
#'
#' @param  x          A terra SpatRast or sf class object 
#' @param  y          Value(s) in x to to calculate distance to
#' @param  scale      (FALSE/TRUE) Perform a row standardization on results 
#'
#' @details 
#' This replicates the terra distance function but uses the Arya & Mount
#' Approximate Near Neighbor (ANN) C++ library for calculating distances. Where this
#' results in a notable increase in performance it is not memory safe, needing to read
#' in the entire raster and does not use the GeographicLib (Karney, 2013) spheroid 
#' distance method for geographic data.  
#'
#' @return A terra SpatRast raster representing distances
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references 
#' Arya S., Mount D. M., Netanyahu N. S., Silverman R. and Wu A. Y (1998), An 
#'   optimal algorithm for approximate nearest neighbor searching, Journal of 
#'   the ACM, 45, 891-923.
#'
#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' 
#' # read, project and subset 10 polygons
#' nc <- suppressWarnings(st_cast(st_read(system.file("shape/nc.shp", 
#'          package="sf")), "POLYGON"))
#'   nc <- st_transform(nc, st_crs("ESRI:102008"))
#'     nc.sub <- nc[sample(1:nrow(nc),10),]
#' 
#' # create 1000m reference raster, rasterize subset polygons
#' ref <- rast(ext(nc), resolution=1000)
#'   rnc <- mask(rasterize(vect(nc.sub), field="CNTY_ID",
#'               ref, background=9999), vect(nc)) 
#'     crs(rnc) <- "ESRI:102008"  
#'   
#' # Calculate distance to class 1 in rnc raster, plot results
#' ids <- nc.sub$CNTY_ID 
#' rd <- rasterDistance(rnc, y=ids) 
#'   plot(rd)
#'     plot( st_geometry(nc.sub), add=TRUE)
#' 
#' }
#' @seealso \code{\link[terra]{distance}, \link[terra]{distance}}
#'
#' @import terra
#' @export rasterDistance 
rasterDistance <- function(x, y, scale = FALSE){
  if(length(find.package("RANN", quiet = TRUE)) == 0)
    stop("please install RANN package before running this function")
  if(missing(x))
    stop("x argument is missing")
  if(missing(y))
    stop("y argument is missing")	
  if(!any(y %in% unique(x[])[,1]))
    stop("values in ", deparse(substitute(y)), " are missing in ", deparse(substitute(x)))
  if(!inherits(x, "SpatRaster"))
	  stop(deparse(substitute(x)), " must be a terra SpatRast object")
  r <- terra::ifel(x %in% y, 1, NA)  
    idx <- which(r[] == 1)
      na.idx <- which(is.na(r[]))
  knn.dist <- RANN::nn2(terra::xyFromCell(r, idx),
                        terra::xyFromCell(r, na.idx), 
    			        k = 1)$nn.dists[,1]
	r <- terra::ifel(r == 1, 0, NA)					
      r[na.idx] <- knn.dist
	    r <- terra::mask(r, x)	
    if(scale) r <- r / terra::global(r, "max", na.rm=TRUE)[,1] 
  return(r)
}
