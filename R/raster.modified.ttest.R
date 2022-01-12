#' @title Dutilleul moving window bivariate raster correlation 
#' @description A bivarate raster correlation using Dutilleul's 
#'              modified t-test
#'       
#' @param x           x raster for correlation, SpatialPixelsDataFrame or 
#'                    SpatialGridDataFrame object    
#' @param y           y raster for correlation, SpatialPixelsDataFrame or 
#'                    SpatialGridDataFrame object 
#' @param x.idx       Index for the column in the x raster object  
#' @param y.idx       Index for the column in the y raster object  
#' @param d           Distance for finding neighbors
#' @param sub.sample  Should a sub-sampling approach be employed (TRUE/FALSE)  
#' @param type        If sub.sample = TRUE, what type of sample (random  or hexagon)
#' @param p           If sub.sample = TRUE, what proportion of population 
#'                    should be sampled
#' @param size        Fixed sample size               
#'
#' @return A SpatialPixelsDataFrame or SpatialPointsDataFrame with the 
#'         following attributes:
#' \itemize{ 
#' \item   corr        Correlation 
#' \item   Fstat       The F-statistic calculated as [degrees of freedom * 
#'                     unscaled F-statistic]
#' \item   p.value     p-value for the test
#' \item   moran.x     Moran's-I for x 
#' \item   moran.y     Moran's-I for y  
#'  } 
#'
#' @description 
#' This function provides a bivariate moving window correlation using the modified  
#' t-test to account for spatial autocorrelation. Point based subsampling is provided 
#' for computation tractability. The hexagon sampling is recommended as it it good  
#' at capturing spatial process that includes nonstationarity and anistropy.    
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references
#' Clifford, P., S. Richardson, D. Hemon (1989), Assessing the significance of the  
#'   correlationbetween two spatial processes. Biometrics 45:123-134.
#' @references
#' Dutilleul, P. (1993), Modifying the t test for assessing the correlation between 
#'   two spatial processes. Biometrics 49:305-314. 
#' 
#' @examples
#' \dontrun{
#' library(gstat)                                         
#' library(sp)                                            
#'                                                         
#' data(meuse)                                            
#' data(meuse.grid)                                       
#' coordinates(meuse) <- ~x + y                           
#' coordinates(meuse.grid) <- ~x + y                      
#'                                                         
#' # GRID-1 log(copper):                                              
#' v1 <- variogram(log(copper) ~ 1, meuse)                  
#' x1 <- fit.variogram(v1, vgm(1, "Sph", 800, 1))           
#' G1 <- krige(zinc ~ 1, meuse, meuse.grid, x1, nmax = 30)
#' gridded(G1) <- TRUE                                      
#' G1@data = as.data.frame(G1@data[,-2])
#' 
#' # GRID-2 log(elev):                                              
#' v2 <- variogram(log(elev) ~ 1, meuse)                  
#' x2 <- fit.variogram(v2, vgm(.1, "Sph", 1000, .6))        
#' G2 <- krige(elev ~ 1, meuse, meuse.grid, x2, nmax = 30)
#' gridded(G2) <- TRUE    
#' G2@data <- as.data.frame(G2@data[,-2])
#' G2@data[,1] <- G2@data[,1]
#' 
#' corr <- raster.modified.ttest(G1, G2)
#'   plot(raster::raster(corr,1))
#'   
#' corr.rand <- raster.modified.ttest(G1, G2, sub.sample = TRUE, type = "random")	 
#' corr.hex <- raster.modified.ttest(G1, G2, sub.sample = TRUE, d = 500, size = 1000)	
#'   head(corr.hex@data)
#'     bubble(corr.hex, "corr") 
#' }
#' 
#' @seealso \code{\link[SpatialPack]{modified.ttest}} for test details
#'
#' @export raster.modified.ttest
raster.modified.ttest <- function(x, y, x.idx = 1, y.idx = 1, d = "AUTO", 
                                  sub.sample = FALSE, type = "hexagon", p = 0.10, 
								  size = NULL) {
    if(!any(which(utils::installed.packages()[,1] %in% "SpatialPack")))
      stop("please install SpatialPack package before running this function")							  
	if (!sp::gridded(x))
	  stop(deparse(substitute(x)), " Must be an sp raster object")
    if (!sp::gridded(y)) 
      stop(deparse(substitute(y)), " Must be an sp raster object")	
   if ( (d == "AUTO") == TRUE){
     cs <- x@grid@cellsize[1]
      d = sqrt(2*((cs*3)^2))
    } else {
      if (!is.numeric (d)) stop("Distance (d) must be numeric")
    }
  nb <- spdep::dnearneigh(sp::coordinates(x),0, d)
  if(sub.sample == FALSE) { 	
    spatial.corr <- data.frame()
      for(i in 1:length(nb)) {
        # x.var <- x@data[nb[[i]],][x.idx][,1]
		x.var <- x@data[,x.idx][nb[[i]]]
        # y.var <- y@data[nb[[i]],][y.idx][,1]
		y.var <- y@data[,y.idx][nb[[i]]]
      sc <- SpatialPack::modified.ttest(x.var, y.var, sp::coordinates(x[nb[[i]],]), 
	                                    nclass = 1)		
        spatial.corr <- rbind(spatial.corr, round(data.frame(corr = sc$corr, 
		                      Fstat= (sc$dof * sc$Fstat),  
		                      p.value = sc$p.value, moran.x = sc$imoran[1], 
							  moran.y = sc$imoran[2]),5))  
      }
	  names(spatial.corr) <- c("corr", "Fstat", "p.value", "moran.x", "moran.y")
      s <- x    
      s@data <- spatial.corr
    } else {
      if(!is.null(size)) {
        n = size 
	  } else {
	    n = round(nrow(x) * p, 0)
	  } 
    if(type == "random") {
	  rs <- sample(1:length(nb), n)
      spatial.corr <- data.frame()	  
      for(i in 1:length(rs)) {
        #x.var <- x@data[nb[[rs[i]]],][x.idx][,1]
		x.var <- x@data[,x.idx][nb[[i]]]
        # y.var <- y@data[nb[[rs[i]]],][x.idx][,1]
		y.var <- y@data[,y.idx][nb[[i]]]
      sc <- SpatialPack::modified.ttest(x.var, y.var, sp::coordinates(x[nb[[i]],]), 
	                                    nclass = 1)		
        spatial.corr <- rbind(spatial.corr, round(data.frame(corr = sc$corr, Fstat= (sc$dof * sc$Fstat),  
		                      p.value = sc$p.value, moran.x = sc$imoran[1], 
							  moran.y = sc$imoran[2]),5)) 
      }
	  names(spatial.corr) <- c("corr", "Fstat", "p.value", "moran.x", "moran.y")
      s <- x[rs,]    
      s@data <- spatial.corr
      s <- as(s, "SpatialPointsDataFrame")
	  
    } else {	
      e <- as(raster::extent(x), "SpatialPolygons")  
	  s <- sp::spsample(e, n = n,  type = "hexagonal")
	  r.ids <- raster::extract(raster::raster(x), s, method='simple', buffer=d,  
	                           small=TRUE, cellnumbers=TRUE) 
		names(r.ids) <- 1:length(r.ids) 			 
	  rm.na <- function(x) {
        x <- x[stats::complete.cases(x),]
	  	if( length(x) < 4 ) {
	  	  x <- NA
	  	} else {
	      x <- as.numeric( x[,1] ) 
	  	}
	    return(x)
	  }	
	r.ids <- lapply(r.ids, FUN = rm.na  ) 
    r.ids <- r.ids[sapply(r.ids, function(z) !all(is.na(z)))]
    spatial.corr <- data.frame()
	  for(i in 1:length(r.ids)) {
	    cdat <- data.frame(xvar = raster::raster(x)[r.ids[[i]]], 
		                   yvar = raster::raster(y)[r.ids[[i]]], 
	                       raster::xyFromCell(raster::raster(x), 
						   r.ids[[i]]) )  						   
	sc <- SpatialPack::modified.ttest(cdat[,1], 
		                         cdat[,2], cdat[,3:4], nclass = 1)
        spatial.corr <- rbind(spatial.corr, round(data.frame(corr = sc$corr, 
		                      Fstat = (sc$dof * sc$Fstat),  
		                      p.value = sc$p.value, moran.x = sc$imoran[1], 
							  moran.y = sc$imoran[2]),5)) 
      }
	    names(spatial.corr) <- c("corr", "Fstat", "p.value", "moran.x", "moran.y")
	  s <- s[as.numeric(names(r.ids)),] 
      s <- sp::SpatialPointsDataFrame(s,  spatial.corr) 
	} 	
  }
return( s )  
}
