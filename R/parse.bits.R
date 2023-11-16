#' @title Parse bits
#' @description Returns specified bit value based on integer input
#' 
#' @param x      Integer value
#' @param bit    A single or vector of bits to return             
#' @param depth  The depth (length) of the bit range, default is 8
#' @param order  c("reverse", "none") sort order for the bits
#'
#' @description
#' Data such as MODIS the QC band are stored in bits. This function returns the 
#' value(s) for specified bit. For example, the MODIS QC flag are bits 0-1 with 
#' the bit value 00 representing the "LST produced, good quality" flag. When 
#' exported from HDF the QC bands are often in an 8 bit integer range (0-255). 
#' With this function you can parse the values for each bit to assign the 
#' flag values. 
#'
#' @return a vector or data.frame of parsed interger value(s) associated with input bit
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples 
#' # Return value for bit 5 for integer value 100
#' parse.bits(100, 5)
#'  
#' # Return value(s) for bits 0 and 1 for integer value 100
#' parse.bits(100, c(0,1))
#'
#' # Return value(s) for bits 0 and 1 for integer values 0-255
#' for(i in 0:255) { print(parse.bits(i, c(0,1))) }
#'  
#' \donttest{
#' #### Applied Example using Harmonized Landsat Sentinel-2 QC 
#'
#' # Create dummy data and qc band
#' library(terra)
#' r <- rast(nrow=100, ncol=100)
#'   r[] <- round(runif(ncell(r), 0,1)) 
#' qc <- rast(nrow=100, ncol=100)
#'   qc[] <- round(runif(ncell(qc), 64,234)) 
#' 
#' # Calculate bit values from QC table
#' ( qc_bits <- data.frame(int=0:255, 
#' 	cloud = unlist(lapply(0:255, FUN=parse.bits, bit=1)),
#' 	shadow = unlist(lapply(0:255, FUN=parse.bits, bit=3)),
#' 	acloud = unlist(lapply(0:255, FUN=parse.bits, bit=2)),
#' 	cirrus = unlist(lapply(0:255, FUN=parse.bits, bit=0)),
#' 	aerosol = unlist(lapply(0:255, FUN=parse.bits, bit=c(7,6)))) )
#' 		
#' # Query the results to create a vector of integer values indicating what to mask 
#' #  cloud is bit 1 and shadow bit 3	
#' m <- sort(unique(qc_bits[c(which(qc_bits$cloud == 1),
#'                            which(qc_bits$shadow == 1)
#' 						   ),]$int))
#' 
#' # Apply queried integer values to mask image with QA band
#' qc[qc %in% m] <- NA
#' r <- mask(r, qc)
#' }
#'
#' @export parse.bits
parse.bits <- function(x, bit, depth=8, order = c("reverse", "none") ) {
  position <- depth - bit
  if(order[1] == "reverse") sort.order = rev(1:depth) else sort.order = 1:depth  
    b <- as.integer(intToBits(x)[1:depth])[depth:1][position]
    if(length(bit) > 1) b <- paste0(b, collapse=" ")
  return(b)
}
