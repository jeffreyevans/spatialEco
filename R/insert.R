#' @title Insert a row or column into a data.frame
#' @description Inserts a new row or column into a data.frame
#'              at a specified location
#' 
#' @param x        Existing data.frame
#' @param MARGIN   Insert a 1 = row or 2 = column
#' @param value    A vector of values equal to the length of MARGIN,
#'                 if nothing specified values with be NA
#' @param idx      Index position to insert row or column
#' @param name     Name of new column (not used for rows,
#'                 MARGIN=1)
#'
#' @return A data.frame with the new row or column inserted
#'
#' @note 
#' Where there are methods to easily add a row/column to 
#' the end or beginning of a data.frame, it is not straight
#' forward to insert data at a specific location within the
#' data.frame. This function allows for inserting a vector
#' at a specific location eg., between columns or rows 1 and 2
#' where row/column 2 is moved to the 3rd position and a new
#' vector of values is inserted into the 2nd position.   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' d <- data.frame(ID=1:10, y=runif(10))
#'
#' # insert row
#' insert(d, idx=2)
#' insert(d, value=c(20,0), idx=2)
#'
#' # insert column
#' insert(d, MARGIN=2, idx=2)
#' insert(d, MARGIN = 2, value = rep(0,10), idx=2, name="x")
#'
#' @export insert
insert <- function(x, MARGIN = 1, value = NULL, idx, name=NULL) {
  if(class(x)[1] != "data.frame")
    stop("x must be data.frame object")
  if(missing(idx))
    stop("idx argument must be supplied")
  idx = idx[1]
  if(MARGIN == 1) {
    cat("Inserting row", "\n")
    if(is.null(value)) value = rep(NA, ncol(x))
	  if(length(value) != ncol(x))
	    stop("specified values not equal number of columns")
      x[seq(idx+1, nrow(x)+1),] <- x[seq(idx, nrow(x)),]
        x[idx,] <- value
  } else if(MARGIN == 2) {
    cat("Inserting column", "\n")
    n <- names(x)
    if(is.null(value)) value = rep(NA, nrow(x))
	  if(length(value) != nrow(x))
	    stop("specified values not equal number of columns")
      x[,seq(idx,ncol(x)+1)] <- x[,seq(idx, ncol(x))]
        x[,idx] <- value
	    names(x)[-idx] <- n
      if(is.null(name)) name = "V1" 
    names(x)[idx] <- name
  }
  return(x)
}
