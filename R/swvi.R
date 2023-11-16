#' @title Senescence weighted Vegetation Index (swvi) 
#' @description Modified Soil-adjusted Vegetation Index (MSAVI) or Modified Triangular 
#'              Vegetation Index 2 (MTVI) weighted by the Normalized difference senescent 
#'              vegetation index (NDSVI)
#'
#' @param red            SpatRaster, Red band (0.636 - 0.673mm), landsat 5&7 band 3, OLI 
#'                       (landsat 8) band 4
#' @param nir            SpatRaster, Near infrared band (0.851 - 0.879mm) landsat 5&7 band 4, 
#'                       OLI (landsat 8) band 5 
#' @param swir           SpatRaster, short-wave infrared band 1 (1.566 - 1.651mm), landsat 5&7 
#'                       band 5, OLI (landsat 8) band 6
#' @param mtvi           (FALSE | TRUE) Use Modified Triangular Vegetation Index 2  
#'                       instead of MSAVI
#' @param green          Green band if MTVI = TRUE
#' @param senescence     The critical value, in NDSVI, representing senescent vegetation 
#' @param threshold      Threshold value for defining NA based on < p
#' @param weight.factor  Apply partial weights (w * weight.factor) to the NDSVI weights 
#' @param ...            Additional arguments passed to terra::lapp function
#'
#' @details
#' The intent of this index is to correct the MSAVI or MTVI index for bias associated 
#' with senescent vegetation. This is done by: 
#'   1 deriving the NDSVI
#'   2 applying a threshold to limit NDSVI to values associated with senescent vegetation 
#'   3 converting the index to inverted weights (-1*(NDSVI/sum(NDSVI))) 
#'   4 applying weights to MSAVI or MTVI 
#' 
#' The MSAVI formula follows the modification proposed by Qi et al. (1994), 
#' often referred to as MSAVI2. MSAVI index reduces soil noise and increases 
#' the dynamic range of the vegetation signal. The implemented modified version 
#' (MSAVI2) is based on an inductive method that does not use a constant L value, in 
#' separating soil effects, an highlights healthy vegetation. The MTVI(2) index follows 
#' Haboudane et al., (2004) and represents the area of a hypothetical triangle in spectral 
#' space that connects (1) green peak reflectance, (2) minimum chlorophyll absorption, and 
#' (3) the NIR shoulder. When chlorophyll absorption causes a decrease of red reflectance, 
#' and leaf tissue abundance causes an increase in NIR reflectance, the total area of the 
#' triangle increases. It is good for estimating green LAI, but its sensitivity to chlorophyll 
#' increases with an increase in canopy density. The modified version of the index accounts 
#' for the background signature of soils while preserving sensitivity to LAI  and resistance 
#' to the influence of chlorophyll. 
#'
#' The Normalized difference senescent vegetation index (NDSVI) follows methods from 
#' Qi et a., (2000). The senescence is used to threshold the NDSVI. Values less then this value 
#' will be NA. The threshold argument is used to apply a threshold to MSAVI. The default is NULL 
#' but if specified all values (MSAVI <= threshold) will be NA. Applying a weight.factor can be 
#' used to change the influence of the weights on MSAVI. 
#' @md
#' 
#' @return A terra SpatRaster class object of the weighted MSAVI metric 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @references 
#' Haboudane, D., et al. (2004) Hyperspectral Vegetation Indices and Novel Algorithms 
#'   for Predicting Green LAI of Crop Canopies: Modeling and Validation in the Context 
#'   of Precision Agriculture. Remote Sensing of Environment 90:337-352.
#'  
#' Qi J., Chehbouni A., Huete A.R., Kerr Y.H., (1994). Modified Soil Adjusted Vegetation 
#'   Index (MSAVI). Remote Sens Environ 48:119-126.
#' 
#' Qi J., Kerr Y., Chehbouni A., (1994). External factor consideration in vegetation 
#'   index development. Proc. of Physical Measurements and Signatures in Remote Sensing, 
#'   ISPRS, 723-730.
#'  
#' Qi, J., Marsett, R., Moran, M.S., Goodrich, D.C., Heilman, P., Kerr, Y.H., Dedieu, 
#'   G., Chehbouni, A., Zhang, X.X. (2000). Spatial and temporal dynamics of vegetation
#    in the San Pedro River basin area. Agricultural and Forest Meteorology. 105:55-68. 
#'
#' @examples
#' \donttest{
#' library(terra)
#' lsat <- rast(system.file("/extdata/Landsat_TM5.tif", package="spatialEco"))
#'  
#' # Using Modified Soil-adjusted Vegetation Index (MSAVI)
#' ( wmsavi <- swvi(red = lsat[[3]], nir = lsat[[4]], swir = lsat[[5]]) )
#'     plotRGB(lsat, r=6,g=5,b=2, scale=1, stretch="lin")
#'       plot(wmsavi, legend=FALSE, col=rev(terrain.colors(100, alpha=0.35)), add=TRUE )
#' 
#' # Using Modified Triangular Vegetation Index 2 (MTVI) 
#' ( wmtvi <- swvi(red = lsat[[3]], nir = lsat[[4]], swir = lsat[[5]],
#'                           green = lsat[[3]], mtvi = TRUE) )
#'   plotRGB(lsat, r=6,g=5,b=2, scale=1, stretch="lin")
#'     plot(wmtvi, legend=FALSE, col=rev(terrain.colors(100, alpha=0.35)), add=TRUE )
#' }
#' 
#' @export swvi
swvi <- function(red, nir, swir, green = NULL, mtvi = FALSE, 
                 senescence = 0, threshold = NULL, 
                 weight.factor = NULL, ...) {
  if(missing(red) | missing(nir) | missing(swir))
    stop("Must specify red, nir and swir1 bands")
  if(mtvi) { 
    if(is.null(green)) 
      stop("Must specify green band") 
    if (!inherits(green, "SpatRaster")) 
	stop("green band must be a terra SpatRaster object")	  
  } 	
  if (!inherits(red, "SpatRaster")) 
	stop("red band must be a terra SpatRaster object")
  if (!inherits(nir, "SpatRaster")) 
	stop("NIR band must be a terra SpatRaster object")
  if (!inherits(swir, "SpatRaster")) 
	stop("SWIR band must be a terra SpatRaster object")

  f.msavi <- function(nir, red) {
    return( (2 * nir + 1 - sqrt( (2 * nir + 1)^2 - 8 * (nir - red) )) / 2 )
  }
  f.mtvi <- function(nir, red, green) {
    return( 1.5 * (1.2*(nir - green) - 2.5*(red - green)) /
	        sqrt( (2*nir+1)^2 - (6 * nir - 5 * sqrt(red) - 0.5) ) )
  }
  ndsvi <- function(nir, swir) { return( (swir - nir) / (swir + nir) ) }  
    wf <- function(x, y) {
      if(!is.null(weight.factor)) { 
	    y <- y / weight.factor 
	  }
      return( x * y )
    }
  w <- ndsvi(nir, swir)
    w[w < senescence] <- NA
    # w <- (w / terra::global(w, "sum", na.rm=TRUE)[,1]) * -1
  if(mtvi == TRUE) {
    d <- c(f.mtvi(nir, red, green), w)  
    return( terra::lapp(d, fun = wf, ...) )
  } else {
    d <- c(f.msavi(nir, red), w)
	return( terra::lapp(d, fun=wf, ...) )
  }  
}
