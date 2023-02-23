#' @title built index
#' @description Remote sensing built-up index
#'
#' @param red            Red band (0.636 - 0.673mm), landsat 5&7 band 3, OLI 
#'                       (landsat 8) band 4
#' @param green          Green band (0.53 - 0.59mm), landsat 5&7 band 3, OLI 
#'                       (landsat 8) band 3
#' @param nir            Near infrared band (0.851 - 0.879mm) landsat 5&7 band 4, 
#'                       OLI (landsat 8) band 5 
#' @param swir1          short-wave infrared band 1 (1.566 - 1.651mm), landsat 5&7 
#'                       band 5, OLI (landsat 8) band 6
#' @param swir2          short-wave infrared band 2 (2.11 - 2.29mm), landsat 5&7 
#'                       band 7, OLI (landsat 8) band 7
#' @param L              The L factor for the savi index 
#' @param method         Method to use for index options are "Bouhennache", "Zha", "Xu"
#'
#' @description
#' This function calculates the built-up index. Three methods are available:
#' * Bouhennache is a new method that uses a larger portion of the VIR/NIR 
#'     following OLI bands (((b3+b4+b7)-b6)/3) / (((b3+b4+b7)+b6)/3)   
#' * Zha is the original band ratio method using TM5 ndbi = (b5 - b4) / (b5 + b4)
#' * Xu is a modification to eliminate noise using ETM+7 
#'    (ndbi-((savi-nndwi)/2) / (ndbi+((savi-nndwi)/2) 
#'
#' @description 
#' Generally water has the highest values where built-up areas will occur in the
#' mid portion of the distribution. Since Bouhennache et al (2018) index exploits 
#' a larger portion of the visible (Vis) and infra red spectrum, vegetation will 
#' occur as the  lowest values and barren will exhibit greater values than the 
#' vegetation and lower values than the built-up areas.    
#'
#' @description
#' Band wavelength (nanometers) designations for landsat
#' TM4, TM5 and ETM+7
#' * band-2 0.52-0.60 (green) 
#' * band-3 0.63-0.69 (red) 
#' * band-4 0.76-0.90 (NIR)
#' * band-5 1.55-1.75 (SWIR 1) 
#' * band-7 2.09-2.35 (SWIR 2) 
#' 
#' OLI (Landsat 8)
#' * band-3 0.53-0.59 (green) 
#' * band-4 0.64-0.67 (red) 
#' * band-5 0.85-0.88 (NIR)
#' * band-6 1.57-1.65 (SWIR 1) 
#' * band-7 2.11-2.29 (SWIR 2) 
#' @md
#'
#' @references 
#' Bouhennache, R., T. Bouden, A. Taleb-Ahmed & A. Chaddad(2018) A new spectral index 
#'   for the extraction of built-up land features from Landsat 8 satellite imagery, 
#'   Geocarto International 34(14):1531-1551
#' @references 
#' Xu H. (2008) A new index for delineating built-up land features in satellite imagery. 
#'  International Journal Remote Sensing 29(14):4269-4276. 
#' @references 
#' Zha G.Y., J. Gao, & S. Ni (2003) Use of normalized difference built-up index in 
#'   automatically mapping urban areas from TM imagery. International Journal of 
#'   Remote Sensing 24(3):583-594
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' \dontrun{
#' library(terra)
#' if(!unlist(lapply("RStoolbox", requireNamespace, quietly=TRUE)))
#'   message("Can't run examples, please install RStoolbox")
#' 
#' data(lsat, package = "RStoolbox")
#' lsat <- radCor(lsat, metaData = readMeta(system.file(
#'                "external/landsat/LT52240631988227CUB02_MTL.txt", 
#'                 package="RStoolbox")), method = "apref")
#'    # plotRGB(lsat, r=3, g=2, b=1, scale=1.0, stretch="lin")
#' 			   
#'  # Using Bouhennache et al., (2018) method (needs green, red, swir1 and swir2) 
#'  ( bouh <- built.index(red = lsat[[3]], green = lsat[[4]], swir1 = lsat[[5]], 
#'                       swir2 = lsat[[7]]) )
#'     plotRGB(lsat, r=6,g=5,b=2, scale=1, stretch="lin")
#'       plot(bouh, legend=FALSE, col=rev(terrain.colors(100, alpha=0.35)), 
#' 	       add=TRUE )
#' 
#'  # Using simple Zha et al., (2003) method (needs nir and swir1)
#'  ( zha <- built.index(nir = lsat[[4]], swir1 = lsat[[5]], method = "Zha") )
#'    plotRGB(lsat, r=6,g=5,b=2, scale=1, stretch="lin")
#'      plot(zha, legend=FALSE, col=rev(terrain.colors(100, alpha=0.35)), add=TRUE )
#' 
#'  # Using Xu (2008) normalized modification of Zha (needs green, red, nir and swir1)
#'  ( xu <- built.index(green= lsat[[3]], red = lsat[[3]], nir = lsat[[4]], 
#'                      swir1 = lsat[[5]], , method = "Xu") )
#'    plotRGB(lsat, r=6,g=5,b=2, scale=1, stretch="lin")
#'      plot(xu, legend=FALSE, col=rev(terrain.colors(100, alpha=0.35)), add=TRUE ) 
#' 
#' }
#' 
#' @export built.index
built.index <- function(green, red, nir, swir1, swir2, L = 0.5, 
                        method = c("Bouhennache", "Zha", "Xu")) { 
  if(method[1] == "Zha") {
    if(missing(nir) | missing(swir1))
      stop("must specify swir1 and nir bands")
  if(any(c(class(nir)[1],class(swir1)[1]) %in% 
	 c("RasterLayer", "SpatRaster") == FALSE))
       stop("Rasters must be of terra::SpatRaster or raster::rasterLayer class")	  
    bi <- (swir1 - nir) / (swir1 + nir)
  } else if(method[1] == "Xu") {
    if(missing(green) | missing(red) | missing(swir1))
      stop("Must specify green, red and swir1 bands")  
    if(any(c(class(green)[1],class(red)[1], class(swir1)[1]) %in% 
	 c("RasterLayer", "SpatRaster") == FALSE))
       stop("Rasters must be of terra::SpatRaster or raster::rasterLayer class")	  
    ibi <- function(g, r, nr, s1, l = 0.5) { 
	  savi <- ((nr - r)*(1+l)) / (nr + r + l) 
      mndwi <- (g - s1) / (g - s1) 
      ndbi <- (swir1 - nir) / (swir1 + nir)	  
	  return( (ndbi - ((savi + mndwi) / 2)) / 
	          (ndbi + ((savi + mndwi) / 2)) ) 
	}
	bi <- ibi(g=green, r=red, nr= nir, s1=swir1,  l=L)
  } else if(method[1] == "Bouhennache") {
    if(missing(green) | missing(red) | missing(swir1) | missing(swir2))
      stop("Must specify green, red and swir1 bands")  
    if(any(c(class(green)[1],class(red)[1], class(swir1)[1], class(swir2)[1]) %in% 
	 c("RasterLayer", "SpatRaster") == FALSE))
       stop("Rasters must be of terra::SpatRaster or raster::rasterLayer class")	    
    blfei <- function(g, r, s1, s2) {
       return( (( (g + r + s2) / 3) - s1) /
              ((( g + r + s2 ) / 3) + s1) )
    }
    bi <- blfei(g=green, r=red, s1=swir1, s2=swir2) 
  }
  return(bi)
}  
