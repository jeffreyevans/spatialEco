#' @title Breeding density areas (aka, core habitat areas)
#' @description Calculates breeding density areas base on population counts and 
#'              spatial point density.
#' 
#' @param x        sp SpatialPointsDataFrame object
#' @param pop      Population count/density column in x@@data 
#' @param p        Target percent of population 
#' @param bw       Bandwidth distance for the kernel estimate (default 8500) 
#' @param b        Buffer distance (default 8500)     
#' @param self     (TRUE/FALSE) Should source observations be included in 
#'                 density (default TRUE)
#'
#' @return A list object with:
#' \itemize{ 
#' \item pop.pts   sp point object with points identified within the specified p
#' \item pop.area  sp polygon object of buffered points specified by parameter b
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
#' require(sp)
#' n=1500
#' bb <- rbind(c(-1281299,-761876.5),c(1915337,2566433.5))
#'   bb.mat <- cbind(c(bb[1,1], bb[1,2], bb[1,2], bb[1,1]),   
#'                   c(bb[2,1], bb[2,1], bb[2,2], bb[2,2]))  
#'     bbp <- Polygon(bb.mat)
#'      s <- spsample(bbp, n, type='random')
#'         pop <- SpatialPointsDataFrame(s, data.frame(ID=1:length(s), 
#'                                  counts=runif(length(s), 1,250)))
#'
#'     bd75 <- breeding.density(pop, pop='counts', p=0.75, b=8500, bw=6400)
#'      plot(bd75$pop.area, main='75% breeding density areas')
#'        plot(pop, pch=20, col='black', add=TRUE)
#'          plot(bd75$pop.pts, pch=20, col='red', add=TRUE)
#' 
#' @export
breeding.density <- function(x, pop, p = 0.75, bw = 6400, b = 8500, self = TRUE) {
    if(class(x)[1] == "sf") { x <- as(x, "Spatial") }
    if (!inherits(x, "SpatialPointsDataFrame")) 
        stop("must be a SpatialPointsDataFrame object")
    if (is.na(match(pop, names(x@data)))) 
        stop("Count/density field not present in data")
    point.density <- function(x, bw, self = TRUE) {
        d <- sp::spDists(x, x)
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
    pn <- (1:length(names(x@data)))[names(x@data) == pop]
    pop.n <- sum(x@data[, pn]) * p
    d <- (point.density(x, bw = bw, self = self)) * x@data[, pn]
    pop.den <- data.frame(sp::coordinates(x), x@data[, pn], d)
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
    sp::coordinates(pts) <- ~x + y
    pop.buff <- rgeos::gBuffer(pts, byid = FALSE, id = NULL, width = b, joinStyle = "ROUND", quadsegs = 10)
    polys <- pop.buff@polygons[[1]]@Polygons
    pl <- vector("list", length(polys))
      for (i in 1:length(polys)) {
          pl[[i]] <- sp::Polygons(list(polys[[i]]), i)
      }
    pop.buff <- sp::SpatialPolygons(pl)
    row.ids <- sapply(methods::slot(pop.buff, "polygons"), function(i) methods::slot(i, "ID"))
    pop.buff <- sp::SpatialPolygonsDataFrame(pop.buff, data.frame(FID = as.numeric(row.ids)))
      list(pop.pts = pts, pop.area = pop.buff, bandwidth = bw, buffer = b, p = p)
} 
