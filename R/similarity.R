#' @title Ecological similarity
#' @description Uses row imputation to identify "k" ecological similar observations 
#'
#' @param x          data.frame containing ecological measures 
#' @param k          Number of k nearest neighbors (kNN)
#' @param method     Method to compute multivariate distances c("mahalanobis", "raw", 
#'                   "euclidean", "ica")
#' @param frequency  Calculate frequency of each reference row (TRUE/FALSE) 
#' @param scale      Scale multivariate distances to standard range (TRUE/FALSE)
#' @param ID         Unique ID vector to use as reference ID's (rownames). Must be 
#'                   unique and same length as number of rows in x
#' 
#' @details 
#' This function uses row-based imputation to identify k similar neighbors for each 
#' observation. Has been used to identify offsets based on ecological similarity. 
#' 
#' @return 
#' data.frame with k similar targets and associated distances. If frequency = TRUE  the 
#' freq column represents the number of times a row (ID) was selected as a neighbor.
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Evans, J.S., S.R. Schill, G.T. Raber (2015) A Systematic Framework for Spatial 
#'   Conservation Planning and Ecological Priority Design in St. Lucia, Eastern 
#'   Caribbean. Chapter 26 in Central American Biodiversity : Conservation, Ecology 
#'   and a Sustainable Future. F. Huettman (eds). Springer, NY.   
#'
#' @examples
#' \donttest{ 
#'  library(sf)
#'  data(pu)
#'  kNN <- similarity(st_drop_geometry(pu[2:ncol(pu)]), k = 4, 
#'                    frequency = TRUE, ID = pu$UNIT_ID)  
#'  p <- kNN$freq   
#'  clr <- c("#3288BD", "#99D594", "#E6F598", "#FEE08B", 
#'           "#FC8D59", "#D53E4F")   
#'  p <- ifelse(p <= 0, clr[1], 
#'         ifelse(p > 0 & p < 10, clr[2],
#'           ifelse(p >= 10 & p < 20, clr[3],
#'  	       ifelse(p >= 20 & p < 50, clr[4],
#'  	         ifelse(p >= 50 & p < 100, clr[5],
#'  	           ifelse(p >= 100, clr[6], NA))))))
#'  plot(st_geometry(pu), col=p, border=NA)
#'    legend("topleft", legend=c("None","<10","10-20",
#'           "20-50","50-100",">100"),
#'           fill=clr, cex=0.6, bty="n") 
#'    box()
#' }
#'  
#' @export similarity
similarity <- function(x, k=4, method="mahalanobis", frequency = TRUE, 
                      scale = TRUE, ID = NULL) {
  if(length(find.package("yaImpute", quiet = TRUE)) == 0)
    stop("please install yaImpute package before running this function")
  if(!class(x)[1] == "data.frame") stop( "x is not a data.frame")
    if(!is.null(x)) {  
      if(!length(unique(ID)) == nrow(x) ) stop("ID's are not unique")
	  rownames(x) <- ID
    }	  
    offsets <- yaImpute::yai(x=x, method=method, k=k)
      if( scale == TRUE) {
       dmax <- max(offsets$neiDstRefs)
         for (i in 1:dim(offsets$neiDstRefs)[2] ) {
           offsets$neiDstRefs[,i] <- offsets$neiDstRefs[,i] / dmax 
         }
      }
    imp.ids <- as.data.frame(offsets$neiIdsRefs) 
      for (i in 1:dim(imp.ids)[2] ) { 
        imp.ids[,i] <- as.numeric(as.character(imp.ids[,i])) 
      }
    KNN <- data.frame(row.names=as.character(rownames(offsets$neiIdsRefs)), 
	                  imp.ids, offsets$neiDstRefs)					 
      if(frequency == TRUE) {
        neighbors <- as.vector(as.matrix(KNN[,1:4])) 	  
        f <- vector()
        for(i in 1:nrow(KNN)) {  
          f <- append(f, length(neighbors[neighbors == as.numeric(rownames(KNN[i,]))]))
		}
	KNN <- data.frame(KNN, freq = f) 
   }
  return( KNN ) 
}
