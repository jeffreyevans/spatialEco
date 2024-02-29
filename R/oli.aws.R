#' @title Query AWS-OLI
#' @description Query of Amazon AWS OLI-Landsat 8 cloud service
#'
#' @param ...  not used
#'    
#' @details 
#' Given the changes to AWS Registry of Open Data, using the data  
#' digest is no longer reliable. Plese use the AWS Data Exchange 
#' or a STAC API for data acccess 
#'
#' @export oli.aws
oli.aws <- function(...) {
  .Deprecated("oli.aws", package="spatialEco", 
    msg="Given the changes to AWS Registry of Open Data, using the data  
         digest is no longer reliable")
 message("Plese use the AWS Data Exchange or a STAC API for data acccess")	
}
