#' @title sp na.omit
#' @description Removes row or column NA's in sp object
#'
#' @param x Object of class SpatialPointsDataFrame OR SpatialPolygonsDataFrame 
#' @param col.name  The name of a specific column to remove NA's from
#' @param margin Margin (1,2) of data.frame 1 for rows or 2 for columns
#'
#' @note This function will remove all NA's in the object or NA's associated with a specific column.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'                                                                       
#' @examples 
#'  library(sp)
#'  data(meuse)
#'  coordinates(meuse) <- ~x+y
#'    
#'  # Display rows with NA  
#'  meuse@@data[!complete.cases(meuse@@data),] 
#'
#'  # Remove all NA's in rows (and associated points)
#'  meuse2 <- sp.na.omit(meuse) 
#'    dim(meuse)
#'    dim(meuse2)
#'    
#'  # Plot deleted points in red
#'  plot(meuse, col='red', pch=20)
#'  plot(meuse2, col='black', pch=20, add=TRUE)
#'
#'  # Remove NA's associated with specific column 
#'  meuse2 <- sp.na.omit(meuse, col.name = "om") 
#'    head(meuse@@data)
#'    head(meuse2@@data)
#'
#' @export
sp.na.omit <- function(x, col.name = NULL, margin = 1) {
    if (!inherits(x, "SpatialPointsDataFrame") & 
	      !inherits(x, "SpatialPolygonsDataFrame") & 
		    !inherits(x, "SpatialLinesDataFrame") )
        stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame class object")
	if(!is.null(col.name)) {
	  if(is.na(match(col.name, names(x)))) stop(col.name, "does not exist in data") 
	  return( x[-which(is.na(x@data[,col.name])),] )
    } else {    
      na.index <- unique(as.data.frame(which(is.na(x@data), arr.ind = TRUE))[, margin])
      if (margin == 1) {
          cat("Deleting rows: ", na.index, "\n")
          return(x[-na.index, ])
      }
      if (margin == 2) {
          cat("Deleting columns: ", na.index, "\n")
          return(x[, -na.index])
      }
    }
} 
