#' @title All pairwise combinations
#' @description Creates all pairwise combinations list for iteration
#'
#' @param x A numeric or character vector 
#'
#' @return A list object with increasing all combination objects,
#'         the first list element are the pairwise comparisons
#' 
#' @note This returns a list of vector combinations starting with
#' pairwise, as the first nested list element, then in groups of 
#'  threes, fours, to length of the vector.  
#'
#' @author Jeffrey S. Evans    <jeffrey_evans<at>tnc.org>
#'
#' @examples
#' classes <- paste0("class", 1:10)
#' 
#' all_pairwise(classes)[[1]]
#' 
#' #### How to use as an iterator
#' # dataframe with 4 cols, 100 rows
#' d <- as.data.frame(matrix(runif(100*4), 100, 4)) 
#'   names(d) <- paste0("class", 1:4) 
#' 
#' ( idx <- all_pairwise(colnames(d))[[1]] ) 
#' 
#' opar <- par(no.readonly=TRUE)
#'   par(mfrow=c(2,3))
#'     lapply(idx, function(i) {
#'       plot(d[,i[1]], d[,i[2]], main=paste0(i[1], " vs ", i[2]) )
#'     })	
#' par(opar)
#' 
#' @export
all_pairwise <- function(x) {
  if(!is.vector(x))
    stop(deparse(substitute(x)), " must be a vector object")  
  n <- length(x)
    idx <- lapply(2:n, function(y) {
      utils::combn(x, y, simplify = FALSE)
    })
} 
 