#' @title Dispersion (H-prime) 
#' @description Calculates the dispersion ("rarity") of targets associated with planning units
#'
#' @param  x       data.frame object of target values 
#'
#' @return data.frame with columns H values for each target, H , sH, sHmax 
#'
#' @note The dispersion index (H-prime) is calculated H = sum( sqrt(p) / sqrt(a) ) 
#'         where; P = [sum of target in planning unit / sum of target across all planning units] 
#'         and a = [count of planning units containing target / number of planning units]  
#'  
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Evans, J.S., S.R. Schill, G.T. Raber (2015) A Systematic Framework for Spatial Conservation 
#'   Planning and Ecological Priority Design in St. Lucia, Eastern Caribbean. Chapter 26 in 
#'   Central American Biodiversity : Conservation, Ecology and a Sustainable Future. F. Huettman (eds). 
#'   Springer, NY.   
#'
#' @examples
#'  library(sp)
#'  data(pu)
#'  
#'  d <- dispersion(pu@data[,2:ncol(pu)])
#'
#' \dontrun{   
#'  p <- d[,"H"]
#'  clr <- c("#3288BD", "#99D594", "#E6F598", "#FEE08B", 
#'           "#FC8D59", "#D53E4F")      
#'  clrs <- ifelse(p < 0.5524462, clr[1], 
#'            ifelse(p >= 0.5524462 & p < 1.223523, clr[2],
#'              ifelse(p >= 1.223523 & p < 2.465613, clr[3],
#'  	          ifelse(p >= 2.465613 & p < 4.76429, clr[4],
#'  	            ifelse(p >= 4.76429 & p < 8.817699, clr[5],
#'  	              ifelse(p >= 8.817699, clr[6], NA))))))
#'  plot(pu, col=clrs, border=NA)
#'    legend("topleft", legend=rev(c("Very Rare","Rare","Moderately Rare",
#'           "Somewhat Common","Common","Over Dispersed")),
#'           fill=clr, cex=0.6, bty="n") 
#'    box()
#' }
#'	
#' @export
dispersion <- function (x) {
  if (!inherits(x, "data.frame")) 
    stop("x is not a data.frame or matrix object")
  H <- as.data.frame(array(0, dim=c( dim(x)[1], 0 )))
   rownames(H) <- rownames(x)
    tcounts <- apply(x, MARGIN=1, function(x){ length(x[x > 0]) } )
    total <- apply(x, MARGIN=2, sum, na.rm = TRUE) 
       for (n in 1:ncol(x) ) {
          s = total[n]
          p <- (x[,n] / s)
          a <- length(x[x[,n] > 0 ,]) / length(x[,n])
          r <- sqrt(ifelse(p >= 0, p, 0)) / sqrt(a)   
          H <- cbind(H, r)
       }
    names(H) <- names(x)
	Hm <- rowSums(H)
	maxH <- apply(H, MARGIN=1, max)
	return ( data.frame(row.names = rownames(x), H, H = Hm, sH = Hm / max(Hm), 
	         sHmax = maxH / max(maxH) ) )
}  
