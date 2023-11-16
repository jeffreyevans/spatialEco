#' @title Shannon's Diversity (Entropy) Index
#' @description Calculates Shannon's Diversity Index and Shannon's Evenness Index
#'
#' @param x        data.frame object containing counts or proportions 
#' @param counts   Are data counts (TRUE) or relative proportions (FALSE) 
#' @param ens      Calculate effective number of species (TRUE/FALSE)
#' @param margin   Calculate diversity for rows or columns. c("row", "col")    
#'
#' @details 
#' The expected for H is 0-3+ where a value of 2 has been suggested as medium-high diversity, 
#' for evenness is 0-1 with 0 signifying no evenness and 1, complete evenness. 
#'
#' @return 
#' data.frame with "H" (Shannon's diversity) and "evenness" (Shannon's 
#' evenness where H / max( sum(x) ) ) and ESN 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Shannon, C. E. and W. Weaver (1948) A mathematical theory of communication. The Bell 
#'   System Technical Journal, 27:379-423.
#' 
#' Simpson, E. H. (1949) Measurement of diversity. Nature 163:688
#' 
#' Roth, D. S., I. Perfecto, and B. Rathcke (1994) The effects of management systems on 
#'   ground-foraging ant diversity in Costa Rica. Ecological Applications 4(3):423-436.
#'
#' @examples
#' # Using Costa Rican ant diversity data from Roth et al. (1994)
#' data(ants)
#'   
#' # Calculate diversity for each covertype ("col") 
#' shannons(ants[,2:ncol(ants)], ens = TRUE, counts = FALSE, margin = "col")
#'
#' # Calculate diversity for each species ("row") 
#' ant.div <- shannons(ants[,2:ncol(ants)], ens = TRUE, counts = FALSE, 
#'                     margin = "row")
#'   row.names(ant.div) <- ants[,1]
#'   ant.div
#'	
#' @export
shannons <- function(x, counts = TRUE, ens = FALSE, margin = "row") {
  if( margin == "row") { m = 1 } else { m = 2 }
    s <- function(x) { -1 * sum( x[!is.na(x) > 0] * log(x[!is.na(x) > 0]) ) } 
    e <- function(x) { -1 * sum( (1 / x[!is.na(x) > 0]) * log((1 / x[!is.na(x) > 0])) ) } 
  if( counts ) {
    x.sum <- apply(x, MARGIN = m, sum, na.rm=TRUE)  
    x <- sweep(x, MARGIN = m, x.sum, "/")
  }   
    H <- apply(x, MARGIN = m, FUN = s)
	n.spp <- apply(x, MARGIN = m, FUN = function(x) {length(x[x > 0])} )
    H <- data.frame(H=H, evenness = ( H / log(n.spp) ) )
  if( ens ) {
    H <- data.frame(H, ENS = round(exp ( H[,1] ),0) ) 
  }
  return ( H ) 	
}
