#' @title Weighted MSAVI
#' @description Modified Soil-adjusted Vegetation Index (MSAVI) weighted by the 
#'              Normalized difference senescent vegetation index (NDSVI)
#'
#' @param red              Red band (0.636 - 0.673mm), landsat 5&7 band 3, OLI (landsat 8) band 4
#' @param nir              Near infrared band (0.851 - 0.879mm) landsat 5&7 band 4, OLI (landsat 8) band 5 
#' @param swir             short-wave infrared band 1 (1.566 - 1.651mm), landsat 5&7 band 5, OLI (landsat 8) band 6
#' @param senescence       The critical value, in NDSVI, representing senescent vegetation 
#' @param threshold        Threshold value for defining NA based on < p
#' @param weight.factor    Apply partial weights [w * weight.factor] to the NDSVI weights 
#'
#'
#' @return rasterLayer class object of the weighted MSAVI metric 
#'
#' @notes
#' The intent of this index is to correct the MSAVI index for bias associated with senescent vegetation. This is done by: 
#'   1) derive the NDSVI;
#'   2) apply a threshold to limit NDSVI to values associated with senescent vegetation; 
#'   3) convert the index to inverted weights [-1*(NDSVI/sum(NDSVI))]; 
#'   4) apply weights to MSAVI  
#' The MSAVI formula follows the modification proposed by Qi et al. (1994), often referred to as MSAVI2. 
#' The Normalized difference senescent vegetation index (NDSVI) follows methods from Qi et a., (2000). 
#' The senescence is used to threshold the NDSVI. Values less then this value will be NA.
#' The threshold argument is used to apply a threshold to MSAVI. The default is NULL but if specified all values [MSAVI <= threshold] will be NA.   
#' Applying a weight.factor can be used to change the influence of the weights on MSAVI. 
#' 
#' @references Qi J., Chehbouni A., Huete A.R., Kerr Y.H., (1994). Modified Soil Adjusted Vegetation Index (MSAVI). Remote Sens Environ 48:119-126.
#' @references Qi J., Kerr Y., Chehbouni A., (1994). External factor consideration in vegetation index development. Proc. of Physical Measurements and Signatures in Remote Sensing, ISPRS, 723-730.
#' @references Qi, J., Marsett, R., Moran, M.S., Goodrich, D.C., Heilman, P., Kerr, Y.H., Dedieu, G., Chehbouni, A., Zhang, X.X. (2000). Spatial and temporal dynamics of vegetation in the San Pedro River basin area. Agricultural and Forest Meteorology. 105:55-68. 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' library(raster)
#' library(RStoolbox)
#' 
#' data(lsat)
#' lsat <- radCor(lsat, metaData = readMeta(system.file("external/landsat/LT52240631988227CUB02_MTL.txt", 
#'                package="RStoolbox")), method = "apref")
#' 
#' ( msavi <- weighted.msavi(red = lsat[[3]], nir = lsat[[4]], swir = lsat[[5]]) )
#'   plot(msavi)
#'
#' @export
weighted.msavi <- function(red, nir, swir, senescence = 0, threshold = NULL, 
                    weight.factor = NULL, ...) {
  if(missing(red) & 
       missing(nir) & 
         missing(swir))
    stop("Must specify red, nir and swir1 bands")
  if(class(red) != "RasterLayer" & 
       class(nir) != "RasterLayer" & 
         class(swir) != "RasterLayer")
    stop("Data must be raster class objects")
  msavi <- function(nir, red) {
    return( (2 * nir + 1 - sqrt( (2 * nir + 1)^2 - 8 * (nir - red) )) / 2 )
  }
  ndsvi <- function(nir, swir) { return( (swir - nir) / (swir + nir) ) }
  wf <- function(y, p = threshold, partial = weight.factor, ...) {
    if( !is.null(p) ) {
      i <- ifelse(p <= y[1], y[1], p)
    } else if(is.na(y[2])) {
      i = y[1]
    } else if(is.na(y[1])) {
      i = NA
    } else {
      if(!is.null(partial)) { y[2] <- y[2] / partial }
        i = y[1] * y[2]
    }
    return(i)
  }
  w <- ndsvi(nir, swir)
    w[w < senescence] <- NA
       w <- (w / sum(w[], na.rm=TRUE)) * -1
  return( calc(stack(msavi(nir, red), w), fun=wf, ...) )
}
