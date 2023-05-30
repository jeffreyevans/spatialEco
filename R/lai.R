#' @title built index
#' @description Remote sensing built-up index
#'
#' @param ndvi     NDVI in floating point standard scale range (-1 to 1)
#' @param method   Method to use for index options c("Jonckheere", "Chen")
#'
#' @description
#' This function calculates the Leaf Area Index (LAI) representing the amount of leaf area 
#' per unit of ground area. This is an important parameter for understanding the structure 
#' and function of vegetation, as it affects processes such as photosynthesis, transpiration, 
#' and carbon cycling. These two approaches are based on the empirical relationship between 
#' NDVI and LAI, which has been observed in many studies, and it is a widely used method for 
#' estimating LAI from remote sensing data. The formulas are derived from the fact that vegetation 
#' with higher LAI tends to have higher reflectance in the near-infrared (NIR) band and lower 
#' reflectance in the red band, resulting in higher NDVI values. But still, the exact relationship 
#' between NDVI and LAI can vary depending on factors such as vegetation type, canopy structure, 
#' and environmental conditions.
#'
#' @references 
#' Jonckheere, I., Fleck, S., Nackaerts, K., Muys, B., Coppin, P. (2004). A comparison of two 
#'   methods to retrieve the leaf area index (LAI) from SPOT-4 HRVIR data. International 
#'   Journal of Remote Sensing, 25(21):4407–4425.
#' @references 
#' Chen, J. M., Liu, R., & Ju, W. (2014). A simple and effective method for estimating 
#'   leaf area index from Landsat imagery. Remote Sensing of Environment, 152:538–548.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' \dontrun{
#' library(terra)
#'  lsat <- rast(system.file("extdata/Landsat_TM5", package="spatialEco"))
#'    plotRGB(lsat, r=3, g=2, b=1, scale=1.0, stretch="lin")
#' 	
#'   ndvi <-  ( lsat[[4]] - lsat[[3]] ) / (lsat[[4]] + lsat[[3]]) 
#' 		   
#'  # Using Jonckheere et al., (2004) method
#'  lai01 <- lai(ndvi)
#'    plot(lai01)
#'
#'  # Using Chen et al., (2014) method
#'  lai02 <- lai(ndvi, method = "Chen")
#'    plot(lai02)
#'
#' }
#' 
#' @export lai
lai <- function(ndvi, method = c("Jonckheere", "Chen")) { 
  if(missing(ndvi))
    stop("must specify NDVI")
  if(terra::nlyr(ndvi) > 1)
    stop("Expecting single band for NDVI")
  if (!inherits(ndvi, "SpatRaster")) 
	stop(deparse(substitute(ndvi)), " must be a terra SpatRaster object")		
  ndvi <- terra::ifel(ndvi < 0, 0, ndvi)
  if(tolower(method[1]) == "jonckheere") {
    LA <- (ndvi - 0.2) / 0.3	  
  } else if(tolower(method[1]) == "chen") {
    LA = (1 - exp(-0.69 * ndvi)) / 0.69
  }   
  return(LA * 10)
}  
