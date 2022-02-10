#' @title Weighted centroid
#' @description Creates centroid of [x,y] coordinates based on a 
#'              weights field 
#'
#' @param x  sp SpatialPointsDataFrame class object                       
#' @param p  Weights column in x@@data slot 
#' @param sp  Output sp SpatailPoints class object (TRUE | FALSE)    
#' 
#' @return 
#' A vector or an sp class SpatialPoints object of the weighted coordinate centroid      
#'
#' @note
#' The weighted centroid is calculated as:
#' [Xw]=[X]*[p], [Yw]=[Y]*[p], [sXw]=SUM[Xw], [sYw]=SUM[Yw], [sP]=SUM[p]
#'    wX=[sXw]/[sP], wY=[sYw]/[sP]    
#'    where; X=X COORDINATE(S), Y=Y COORDINATE(S), p=WEIGHT 
#'    
#' @note Depends: sp
#'                                                                  
#' @examples 
#' library(sp)
#'  data(meuse)
#'  coordinates(meuse) = ~x+y
#'  wt.copper <- wt.centroid(meuse, 'copper', sp=TRUE) 
#'    wt.zinc <- wt.centroid(meuse, 'zinc', sp=TRUE) 
#'      plot(meuse, pch=20, cex=0.75, main='Weighted centroid(s)')
#'        points(wt.copper, pch=19, col='red', cex=1.5)  
#'          points(wt.zinc, pch=19, col='blue', cex=1.5)
#'         box() 
#' legend('topleft', legend=c('all','copper', 'zinc'), 
#'        pch=c(20,19,19),col=c('black','red','blue'))
#'
#' @export wt.centroid                                                                                   
wt.centroid <- function(x, p, sp = TRUE) {
  # if(class(x)[1] == "sf") { x <- as(x, "Spatial") }
    if (!inherits(x, "SpatialPointsDataFrame")) 
        stop(deparse(substitute(x)), " MUST BE A SpatialPointsDataFrame OBJECT")
    p <- x@data[, p]
    Xw <- sum(sp::coordinates(x)[, 1] * p)
    Yw <- sum(sp::coordinates(x)[, 2] * p)
    wX <- Xw/sum(p)
    wY <- Yw/sum(p)
    if (sp == FALSE) {
        return(c(wX, wY))
    } else {
        xy <- sp::SpatialPoints(matrix(c(wX, wY), nrow = 1, ncol = 2))
        sp::proj4string(xy) <- sp::proj4string(x)
        return(xy)
    }
} 
