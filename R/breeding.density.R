#' @title Breeding density areas (aka, core habitat areas)
#' @description Calculates breeding density areas base on population counts and 
#'              spatial point density.
#' 
#' @param x        sf POINT object
#' @param pop      Population count/density column in x 
#' @param p        Target percent of population 
#' @param bw       Bandwidth distance for the kernel estimate (default 8500) 
#' @param b        Buffer distance (default 8500)     
#' @param self     (TRUE/FALSE) Should source observations be included in 
#'                 density (default TRUE)
#'
#' @return A list object with:
#' \itemize{ 
#' \item pop.pts   sf POINT object with points identified within the specified p
#' \item pop.area  sf POLYGON object of buffered points specified by parameter b
#' \item bandwidth Specified distance bandwidth used in identifying neighbor counts 
#' \item buffer    Specified buffer distance used in buffering points for pop.area  
#' \item p         Specified population percent
#' }
#'
#' @note 
#' The breeding density areas model identifies the Nth-percent population exhibiting 
#' the highest spatial density and counts/frequency. It then buffers these points by 
#' a specified distance to produce breeding area polygons. If you would like to recreate 
#' the results in Doherty et al., (2010), then define bw = 6400m and b[if p < 0.75 
#' b = 6400m, | p >= 0.75 b = 8500m]  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Doherty, K.E., J.D. Tack, J.S. Evans, D.E. Naugle (2010) Mapping breeding densities of 
#'   greater  sage-grouse: A tool for range-wide conservation planning.  
#'   Bureau of Land Management. Number L10PG00911
#'                                                              
#' @examples 
#' if(require(sf, quietly = TRUE)) { 
#' 
#' n=1500
#' bb <- rbind(c(-1281299,-761876.5),c(1915337,2566433.5))
#' bb.mat <- round(cbind(c(bb[1,1], bb[1,2], bb[1,2], bb[1,1]),
#'                   c(bb[2,1], bb[2,1], bb[2,2], bb[2,2])),2)
#'  bbp <- st_sfc(st_polygon(list(rbind(bb.mat, bb.mat[1,]))))
#'    pop <- st_as_sf(st_sample(bbp, n, type = "random"))
#'	  st_geometry(pop) <- "geometry"
#'      pop$ID <- 1:nrow(pop)
#'	  pop$counts <- round(runif(nrow(pop), 1,250),0)
#'    
#'     bd75 <- breeding.density(pop, pop='counts', p=0.75, b=8500, bw=6400)	 
#'       plot(st_geometry(bd75$pop.area), border = NA,  
#'	        main='75% breeding density areas', col="grey")
#'          plot(st_geometry(pop), pch=20, col='black', add=TRUE)
#'          plot(st_geometry(bd75$pop.pts), pch=20, col='red', add=TRUE)
#'       legend("bottomright", legend=c("selected areas","selected sites", "all sites"),
#'              bg="white", fill=c("grey","red", "black"), pt.cex = 2) 
#' 
#' }
#'
#' @export
breeding.density <- function(x, pop, p = 0.75, bw = 6400, 
                             b = 8500, self = TRUE) {
  if (!inherits(x, c("SpatialPointsDataFrame", "SpatialPoints", "sf", "sfc", "matrix")))		
    stop(deparse(substitute(x)), " x must be a spatial (sp, df) or matrix object")
  if(inherits(x, c("SpatialPointsDataFrame", "SpatialPoints"))) {
     x <- sf::st_as_sf(x)
  }
  if(as.character(unique(sf::st_geometry_type(x))) != "POINT")
    stop(deparse(substitute(x)), " x must be an sf POINT object")
  if(is.na(match(pop, names(x)))) 
    stop("Count/density field not present in data")
    point.density <- function(x, bw, self = TRUE) {
        d <- sf::st_distance(x, x)
        if (self == TRUE) {
            diag(d) <- 1
        } else {
            diag(d) <- NA
        }
        den <- vector()
        for (i in 1:nrow(d)) {
          di <- stats::na.omit(d[i, ])
          den <- append(den, length(di[di <= bw]))
        }
      den
    }
    pn <- which(names(x) == pop)
    pop.counts <- sf::st_drop_geometry(x[,pn]) 
	pop.n <- sum(pop.counts) * p
    d <- (point.density(x, bw = bw, self = self)) * pop.counts
    pop.den <- data.frame(sf::st_coordinates(x)[,1:2], pop.counts, d)
      names(pop.den) <- c("x", "y", "pop", "pden")
    pop.den <- pop.den[order(-pop.den$pden), ]
      i <- 0
      j <- 0
    pts <- as.data.frame(array(0, dim = c(0, 4)))
      while (i <= pop.n) {
          j <- j + 1
          i <- i + pop.den[, "pop"][j]
          pts <- rbind(pts, pop.den[j, ])
      }
    pts <- sf::st_as_sf(pts, coords = c("x", "y"), agr = "constant")    
	  if(!is.na(sf::st_crs(x))) {
	    sf::st_crs(pts) <- sf::st_crs(x) 
	  }	 		
	pop.buff <- sf::st_buffer(pts, dist=b,
                           nQuadSegs = 30,
                           endCapStyle = "ROUND",
                           joinStyle = "ROUND",
                           mitreLimit = 1)
  return( list(pop.pts = pts, pop.area = pop.buff, bandwidth = bw, 
          buffer = b, p = p) )	
} 
