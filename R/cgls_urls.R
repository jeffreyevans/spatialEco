#' @title Provide URL's for Copernicus Global Land Service datasets
#' @description Returns URL's of a product/version/resolution 
#'   
#' @param ...  not used
#'    
#' @details 
#' Given the changes to ESA-Copenricus data distribution, using digests is
#' no longer reliable. Plese use the Sentinel Hub or a STAC API for data 
#' acccess 
#'
#' @export cgls_urls
cgls_urls <- function(...) {
  .Deprecated("cgls_urls", package="spatialEco", 
    msg="Given the changes to ESA-Copenricus data distribution, using the data  
         digest is no longer reliable")
 message("Plese use the Sentinel Hub or a STAC API for data acccess")	
}
